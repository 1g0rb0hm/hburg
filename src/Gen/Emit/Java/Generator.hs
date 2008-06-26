-----------------------------------------------------------------------------
-- |
-- Module      :  Generator
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

module Gen.Emit.Java.Generator (
  -- Types
  -- Functions
  generate,
) where

{- unqualified imports  -}
import Control.Monad.State
import Util (stringFoldr)

import Ast.Def (Definition, getProds, setProds)
import Ast.Prod (Production, setRuleLabel, setResultLabel)

import Gen.Emit.Label (Label, prodToEnumLab, termToEnumLab)
import Gen.Emit.Java.Modifier (Modifier(..))

{- unqualified imports  -}
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Ast.Ir as Ir (Ir(..), baseRuleMap, linkSet)

import qualified Gen.Ident as I (Ident, pkgId, ntN, rN, nN, nTy, ntTy, rTy, eTy, cTy, eN, nId)
import qualified Gen.Emit.Java.Class as C (Class(..), new)
import qualified Gen.Emit.Java.Enum as E (Enum, new)
import qualified Gen.Emit.Java.Method as M (Method(..), new)
import qualified Gen.Emit.Java.Param as P (newFromList, getIdent)
import qualified Gen.Emit.Java.Var as V (new)
import qualified Gen.Emit.Java.Tile as Tile (tile)
import qualified Gen.Emit.Java.Eval as Eval (eval)

-----------------------------------------------------------------------------

type ClassName = String
type ImportName = String
type NodeKind = String
type PackageName = String

type Children = Int
type Link = Bool
type KindReturn = String

-- | generate. Generate all necessary classes and interfaces
generate :: ClassName -> I.Ident -> NodeKind -> Ir.Ir -> [C.Class]
generate cname ids nkind ir =
  let (ir', enums) = enumerationClasses ids ir in -- enum classes (sideffect on Ir)
  let tile = Tile.tile ids nkind ir' in           -- tiling class
  let eval = Eval.eval ids ir' in                 -- eval class
  let nodeInterface = nodeInterfaceClass          -- node interface class
                (ids)
                -- max. amount of children node can have
                (fst (M.findMax $ Ir.baseRuleMap ir))
                (not (S.null $ Ir.linkSet ir))
                nkind in
  let mapEntry = mapEntryClass ids in             -- mapEntry class
  let codeGenerator = codeGeneratorClass cname ids tile eval ir' in
  -- return generated classes
  [  codeGenerator  -- Code generator class
  ,  mapEntry       -- Entry class
  ,  nodeInterface] -- Node interface
  ++ enums          -- Enumeration classes


-- | enumerationClasses. Creates enumeration classes and stores rule labels with
--   productions in the IR.
enumerationClasses :: I.Ident -> Ir.Ir -> (Ir.Ir, [C.Class])
enumerationClasses ids ir =
  let ntClass = C.new (I.pkgId ids) (I.ntN ids) in
  let ruleClass = C.new (I.pkgId ids) (I.rN ids) in
  let (ir', rules) = genRuleEnums in
  -- return modified IR and enumeration Classes
  (ir', [ntClass {C.enumerations = [genNtEnums]}, ruleClass {C.enumerations = [rules]}])
  where
    -- | Generates Java Enumeration for NonTerminals
    genNtEnums :: E.Enum
    genNtEnums = E.new Public (I.ntN ids) (map (termToEnumLab) (Ir.definitions ir))
    -- | Generates Java Enumeration for rules and stores rule labels with productions.
    genRuleEnums :: (Ir.Ir, E.Enum)
    genRuleEnums =
      let (ndefs, labels) =
            unzip
              (map
                (\d ->
                  let (prods, labs) = unzip (labelProds d (getProds d) 0) in
                  (setProds d prods, labs))
                (Ir.definitions ir)) in
      (ir { Ir.definitions = ndefs }, E.new Public (I.rN ids) (concat labels))
      where
        labelProds :: Definition -> [Production] -> Int -> [(Production, Label)]
        labelProds d [] _ = []
        labelProds d (p:ps) num =
          let label = prodToEnumLab d p (show num) in
          let prod = evalState
                          (do
                              modify (\p -> setRuleLabel p label)
                              modify (\p -> setResultLabel p $ termToEnumLab d)
                              get)
                          (p) in
          (prod, label) : (labelProds d ps (succ num))


-- | nodeInterfaceClass. Generates Java Node Interface class.
nodeInterfaceClass :: I.Ident -> Children -> Link -> KindReturn -> C.Class
nodeInterfaceClass ids children hasLnk retTy =
  let methods =
        genChildMethods ids children ++
        genKindMethod retTy ++
        genEntryMethods ids ++
        (if (hasLnk) then genLinkMethod ids else []) in
  evalState
    (do
      modify (\c -> c { C.modifier = Public,
                        C.isIface = True,
                        C.methods = methods})
      get)
    (C.new (I.pkgId ids) (I.nN ids))
  where
    -- | Generate Child Node access methods.
    genChildMethods :: I.Ident -> Children -> [M.Method]
    genChildMethods ids children =
      map
        (\arity ->
          M.new Public False (I.nTy ids) ("child"++ show arity) [] "")
        ([1 .. children])

    -- | genLinkMethod.
    genLinkMethod :: I.Ident -> [M.Method]
    genLinkMethod ids = [M.new Public False (I.nTy ids) "link" [] ""]

    -- | Generate link node access method interface
    genKindMethod :: KindReturn -> [M.Method]
    genKindMethod ty = [M.new Public False ty "kind" [] ""]

    -- | Generate Java MapEntry manipulation method interfaces
    genEntryMethods :: I.Ident -> [M.Method]
    genEntryMethods ids =
      [M.new Public False "boolean"    "is"   (P.newFromList [(I.ntTy ids,"nt")]) ""] ++
      [M.new Public False (I.eTy ids)  "put"  (P.newFromList [(I.ntTy ids,"nt"), (I.eTy ids,"entry")]) ""] ++
      [M.new Public False (I.eTy ids)  "get"  (P.newFromList [(I.ntTy ids,"nt")]) ""] ++
      [M.new Public False (I.cTy ids)  "cost" (P.newFromList [(I.ntTy ids,"nt")]) ""] ++
      [M.new Public False (I.rTy ids)  "rule" (P.newFromList [(I.ntTy ids,"nt")]) ""]


-- | mapEntryClass. Generate MapEntry Class.
mapEntryClass :: I.Ident -> C.Class
mapEntryClass ids =
  evalState
    (do
      modify (\c -> c {C.methods =
                          [ M.new Public False "" (I.eN ids) [] ""
                          , M.new Public False "" (I.eN ids)
                              (P.newFromList [(I.cTy ids, " c"), (I.rTy ids," r")])
                              "\tcost = c;\n\trule = r;"] ,
                       C.variables =
                          [ V.new Public False (I.cTy ids) "cost" ""
                          , V.new Public False (I.rTy ids) "rule" ""]})
      get)
    (C.new (I.pkgId ids) (I.eN ids))

-- | codeGeneratorClass.
codeGeneratorClass :: ClassName -> I.Ident -> C.Class -> C.Class -> Ir.Ir -> C.Class
codeGeneratorClass cname ids tClass eClass ir =
  evalState
    (do
      modify (\c -> c { C.imports = -- Set imports
                          [genImport (I.ntTy ids) "*" True,
                           genImport (I.rTy ids) "*" True,
                           genImport "java.util" "EnumSet" False,
                           "// @USER INCLUDES START",
                           (show $ Ir.include ir),
                           "// @USER INCLUDES END"],
                        -- Set code defined in 'declarations' section
                        C.userCode = show $ Ir.declaration ir,
                        -- Add 'tiling' and 'eval' classes as nested classes
                        C.nestedClasses = [tClass, eClass],
                        -- Generate Interface method to the outside world
                        C.methods = [genEmitFun] })
      get)
    (C.new (I.pkgId ids) cname)
  where
    -- | Generate import statements
    genImport :: PackageName -> ClassName -> Bool -> ImportName
    genImport pkg cname static =
      "import"++
      (if (static) then " static " else " ") ++
      (if (pkg /= "") then pkg ++"." else "") ++
      cname ++";"

    -- | Create method in our code generator which is public and callable from the outside.
    genEmitFun ::M.Method
    genEmitFun = 
      let m = case (C.methods eClass) of -- retrieve the entry method for evaluation
                [] -> error ("\nERROR: Class "++ C.name eClass ++" has no methods!\n")
                list -> head list in
      -- given the entry method for evaluation, its parameters and return
      -- type, generate the emit method which serves as an entry point
      -- to our code generator
      M.new Public True (M.retTy m) "emit" (M.params m) (genBody m)
      where
        -- | Method body of emit method.
        genBody :: M.Method -> String
        genBody m =
          "\t"++ cname ++".Tiling.tile("++ I.nId ids ++");\n"++
          (if (M.retTy m == "void")
            then "\t"
            else "\treturn ") ++
          cname ++".Eval."++ (M.name m) ++
          "("++
          (stringFoldr
            (\x y -> x ++", "++ y)
            (map (P.getIdent) (M.params m))) ++
          ");"

-----------------------------------------------------------------------------