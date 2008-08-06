-----------------------------------------------------------------------------
-- |
-- Module      :  Gen
-- Copyright   :  Copyright (c) 2008  - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :    <igor@bytelabs.org>
--
-- This module is responsible for creating all necessary Java Classes
-- for the final Code Generator. The following classes are generated:
--    * Enumeration class containing Enums for Non Terminals, Rules etc.
--    * Tiling class contains code for tiling IR tree
--    * Eval class contains code for emitting instructions based on tiling
--    * Node Interface specifies methods that must be implemented by IR nodes
--    * CodeGenerator class that provides the interface to the final compiler
-----------------------------------------------------------------------------

module Hburg.Gen.Java.Gen (
  -- Types
  -- Functions
  generate,
) where

{- unqualified imports  -}
import Control.Monad.State

import Text.PrettyPrint

import Hburg.Ast.Def (Definition, getProds, setProds)
import Hburg.Ast.Prod (Production, setRuleLabel, setResultLabel)

import Hburg.Gen.Java.Modifier (Modifier(..))

{- unqualified imports  -}
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Hburg.Ast.Ir as Ir (Ir(..), baseRuleMap, linkSet)

import qualified Hburg.Gen.Ident as I (Ident, pkgId, ntN, rN, nN, nTy, ntTy, rTy, eTy, cTy, eN, nId)
import qualified Hburg.Gen.Label as Lab (Label, prodToEnum, termToEnum)
import qualified Hburg.Gen.Java.Class as C (Class(..), new)
import qualified Hburg.Gen.Java.Enum as E (Enum, new)
import qualified Hburg.Gen.Java.Method as M (Method(..), new)
import qualified Hburg.Gen.Java.Param as P (fromList, getIdent)
import qualified Hburg.Gen.Java.Var as V (new)
import qualified Hburg.Gen.Java.Tile as Tile (tile)
import qualified Hburg.Gen.Java.Eval as Eval (eval)

-----------------------------------------------------------------------------

type ClassName = String
type ImportName = String
type NodeKind = String
type PackageName = String

type Children = Int
type Link = Bool
type KindReturn = String
type Package = I.Ident

-- | generate. Generate all necessary classes and interfaces
generate :: ClassName -> Package -> NodeKind -> Ir.Ir -> [C.Class]
generate cname pkg ntype ir =
  let (ir', enums) = enumerationClasses pkg ir  -- enum classes (sideffect on Ir)
      tile = Tile.tile pkg ntype ir'            -- tiling class
      eval = Eval.eval pkg ir'                  -- eval class
      nodeInterface =
        nodeInterfaceClass                      -- node interface class
          (pkg)
          (fst (M.findMax $ Ir.baseRuleMap ir)) -- max. amount of children node can have
          (not (S.null $ Ir.linkSet ir))        -- is it a tree with links?
          ntype                                 -- Node tyoe
      mapEntry = mapEntryClass pkg              -- mapEntry class
      codeGenerator = codeGeneratorClass cname pkg tile eval ir'
  in
  -- return generated classes
  [  codeGenerator  -- Code generator class
  ,  mapEntry       -- Entry class
  ,  nodeInterface] -- Node interface
  ++ enums          -- Enumeration classes


-- | enumerationClasses. Creates enumeration classes and stores rule labels with
--   productions in the IR.
enumerationClasses :: I.Ident -> Ir.Ir -> (Ir.Ir, [C.Class])
enumerationClasses ids ir =
  let ntClass = C.new (I.pkgId ids) (I.ntN ids)
      ruleClass = C.new (I.pkgId ids) (I.rN ids)
      (ir', rules) = ruleEnums
  in
  -- return modified IR and enumeration Classes
  (ir', [ntClass {C.enumerations = [ntEnums]}, ruleClass {C.enumerations = [rules]}])
  where
    -- | Generates Java Enumeration for NonTerminals
    ntEnums :: E.Enum
    ntEnums = E.new Public (I.ntN ids) (map (Lab.termToEnum) (Ir.definitions ir))
    -- | Generates Java Enumeration for rules and stores rule labels with productions.
    ruleEnums :: (Ir.Ir, E.Enum)
    ruleEnums =
      let (ndefs, labels) =
            unzip
              (map
                (\d ->
                  let (prods, labs) = unzip (labelProds d (getProds d) 0) in
                  (setProds d prods, labs))
                (Ir.definitions ir)) in
      (ir { Ir.definitions = ndefs }, E.new Public (I.rN ids) (concat labels))
      where
        labelProds :: Definition -> [Production] -> Int -> [(Production, Lab.Label)]
        labelProds d [] _ = []
        labelProds d (p:ps) num =
          let label = Lab.prodToEnum d p (show num)
              prod = execState
                          (do
                              modify (\p -> setRuleLabel p label)
                              modify (\p -> setResultLabel p $ Lab.termToEnum d))
                          p
          in
          (prod, label) : (labelProds d ps (succ num))


-- | nodeInterfaceClass. Generates Java Node Interface class.
nodeInterfaceClass :: I.Ident -> Children -> Link -> KindReturn -> C.Class
nodeInterfaceClass ids children hasLnk retTy =
  let methods =
        childMethods ids children ++
        entryMethods ids ++
        kindMethod retTy ++
        (if (hasLnk) then linkMethod ids else []) in
  execState
    (do
      modify $ \c -> c {C.modifier = Public,
                        C.isIface = True,
                        C.methods = methods})
    $ C.new (I.pkgId ids) (I.nN ids)
  where
    -- | Generate Child Node access methods.
    childMethods :: I.Ident -> Children -> [M.Method]
    childMethods ids children =
      map
        (\arity ->
          M.new Public False (I.nTy ids) ("child"++ show arity) [] empty)
        ([1..children])

    -- | linkMethod.
    linkMethod :: I.Ident -> [M.Method]
    linkMethod ids = [M.new Public False (I.nTy ids) "link" [] empty]

    -- | Generate link node access method interface
    kindMethod :: KindReturn -> [M.Method]
    kindMethod ty = [M.new Public False ty "kind" [] empty]

    -- | Generate Java MapEntry manipulation method interfaces
    entryMethods :: I.Ident -> [M.Method]
    entryMethods ids =
      [M.new Public False "boolean"    "is"   (P.fromList [(I.ntTy ids,"nt")]) empty] ++
      [M.new Public False (I.eTy ids)  "put"  (P.fromList [(I.ntTy ids,"nt"), (I.eTy ids,"entry")]) empty] ++
      [M.new Public False (I.eTy ids)  "get"  (P.fromList [(I.ntTy ids,"nt")]) empty] ++
      [M.new Public False (I.cTy ids)  "cost" (P.fromList [(I.ntTy ids,"nt")]) empty] ++
      [M.new Public False (I.rTy ids)  "rule" (P.fromList [(I.ntTy ids,"nt")]) empty]


-- | mapEntryClass. Generate MapEntry Class.
mapEntryClass :: I.Ident -> C.Class
mapEntryClass ids =
  execState
    (do
      modify $ \c -> c {C.methods =
                          [ M.new Public False "" (I.eN ids) [] empty
                          , M.new Public False "" (I.eN ids)
                              (P.fromList [(I.cTy ids, "c"), (I.rTy ids,"r")])
                              (text "cost = c;" $+$ text "rule = r;")] ,
                        C.variables =
                          [ V.new Public False (I.cTy ids) "cost" ""
                          , V.new Public False (I.rTy ids) "rule" ""]})
    $ C.new (I.pkgId ids) (I.eN ids)

-- | codeGeneratorClass.
codeGeneratorClass :: ClassName -> I.Ident -> C.Class -> C.Class -> Ir.Ir -> C.Class
codeGeneratorClass cname ids tClass eClass ir =
  execState
    (do
      modify $ \c -> c {C.imports = -- Set imports
                            [import' (I.ntTy ids) "*" True,
                             import' (I.rTy ids) "*" True,
                             import' "java.util" "EnumSet" False,
                             text "// @USER INCLUDES START",
                             text . show . Ir.include $ ir,
                             text "// @USER INCLUDES END"],
                        -- Set code defined in 'declarations' section
                        C.userCode = text . show . Ir.declaration $ ir,
                        -- Add 'tiling' and 'eval' classes as nested classes
                        C.nestedClasses = [tClass, eClass],
                        -- Generate Interface method to the outside world
                        C.methods = [emitMethod] })
    $ C.new (I.pkgId ids) cname
  where
    -- | Generate import statements
    import' :: PackageName -> ClassName -> Bool -> Doc
    import' p name static = text "import"
      <+> (if (static)
            then text "static"
            else empty)
      <+> (if (null p) then empty else text p <> text ".")
      <>  text name <> semi

    -- | Create method in our code generator which is public and callable from the outside.
    emitMethod :: M.Method
    emitMethod =
      let m = case (C.methods eClass) of -- retrieve the entry method for evaluation
                [] -> error ("\nERROR: Class "++ C.name eClass ++" has no methods!\n")
                list -> head list in
      -- given the entry method for evaluation, its parameters and return
      -- type, generate the emit method which serves as an entry point
      -- to our code generator
      M.new Public True (M.retTy m) "emit" (M.params m) (body m)
      where
        -- | Method body of emit method.
        body :: M.Method -> Doc
        body m = text cname <> text ".Tiling.tile"
            <>  parens ( text $ I.nId ids ) <> semi
            $+$ (if (M.retTy m == "void")
                  then empty
                  else text "return")
            <+> text cname <> text ".Eval." <> text (M.name m) <>
                lparen <>                      -- parameters
                  (if (null $ M.params m)
                      then empty
                      else
                        foldr1
                          (\p1 p2 -> p1 <> comma <+> p2)
                          (map (text . P.getIdent) $ M.params m)) <>
                rparen <> semi

-----------------------------------------------------------------------------