-----------------------------------------------------------------------------
-- |
-- Module      :  Tile
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module generates tiling target source code.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Tile (
  -- * Functions
  tile,
) where

{- unqualified imports  -}
import Control.Monad.State

import Ast.Term (TermClass(..), nonTerminal)
import Ast.Op (Operator, opSem)
import Ast.Def (getNodeReturnType)
import Ast.Cost as Cost (isZero)
import Ast.Prod (Production, getCost, getNode, getRuleLabel, getResultLabel, getArity)

import Gen.Emit.Label (termToEnumLab, childCallLab)
import Gen.Emit.Java.Modifier (Modifier(..))

{- qualified imports  -}

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Ast.Node as N (mapChildren, mapPreOrder2)
import qualified Ast.Ir as Ir (Ir(..), baseRuleMap, linkSet)
import qualified Ast.Closure as Cl (Closure, Label(..), closure, fromLabels, toLabels, empty)

import qualified Gen.Ident as I (Ident, ntId, rId, nId, cId, cTy, nTy, ntTy, rTy, eTy)
import qualified Gen.Emit.Java.Class as Class (Class(..), new)
import qualified Gen.Emit.Java.Method as M (Method, new)
import qualified Gen.Emit.Java.Var as V (Var, new)
import qualified Gen.Emit.Java.Param as Param (new, newFromList)

-----------------------------------------------------------------------------

type NodeKind = String
type Arity = Int

-- | Top level function for generating tiling target source code and the appropriate node interface.
tile :: I.Ident -> NodeKind -> Ir.Ir -> Class.Class
tile ids nkind ir =
  let prodmap = Ir.baseRuleMap ir in
  let linkset = Ir.linkSet ir in
  let variables = genEnumSetVars ir linkset in        -- generate EnumSet variables
  let closures = Cl.closure $ Ir.definitions ir in
  let methods =
        [ genLabelMethod ids closures                 -- generate label() mehtod
        , genTileMethod ids linkset (M.keys prodmap)] -- generate tile() method [Cooper p.566]
        ++ genLabelSetMethods ids ir prodmap in       -- generate label_N() methods for nodes with arity N
  evalState
    (do
      modify (\c -> c { Class.modifier = Private
                      , Class.isStatic = True
                      , Class.variables = variables
                      , Class.methods = methods})
      get)
    (Class.new "" "Tiling")

-- | Generate a name for node sets based on node arity.
genSetName :: Int -> String
genSetName arity = "arity"++ (show arity) ++"Set"

-- | Generate a name for the label method depending on the arity of the nodes.
genLabelMethodName :: Int -> String
genLabelMethodName arity = "label_"++ (show arity)

-- | Produce EnumSet variables.
--      * Example: private static EnumSet unarySet = EnumSet.of(FUN,FUNAP,SIDEFFECT)
genEnumSetVars :: Ir.Ir -> S.Set Operator -> [V.Var]
genEnumSetVars ir linkset =
  let opsets = Ir.operatorMap ir in
  let vars =
        map
          (\key -> genVar (genSetName key) (opsets M.! key))
          (M.keys opsets) in
  if (S.null linkset)
    then vars
    else (genVar "linkSet" linkset):vars
  where
    -- | genVar.
    genVar :: String -> S.Set Operator -> V.Var
    genVar name opset =
      V.new Private True "EnumSet"  name ("EnumSet.of("++ (transformOpSet opset) ++")")

    -- | Make String representation from an Operator set
    transformOpSet :: S.Set Operator -> String
    transformOpSet opset =
      S.fold
        (\x y ->
          if (null y)
            then show (opSem x)
            else show (opSem x) ++", "++ y)
        ("")
        (opset)

-- | Generates label method.
genLabelMethod :: I.Ident -> Cl.Closure -> M.Method
genLabelMethod ids cls =
  let params =
        Param.newFromList
          [(I.nTy ids, I.nId ids), (I.ntTy ids,I.ntId ids),
           (I.cTy ids, I.cId ids), (I.rTy ids, I.rId ids)] in
  M.new Private True "void" "label" params methodBody
  where
    -- | Method body of label method
    methodBody :: String
    methodBody = 
      "\tif ("++ I.cId ids ++" < "++ I.nId ids ++".cost("++ I.ntId ids ++")) {\n\t\t"++
      I.nId ids ++".put("++ I.ntId ids ++", new "++ I.eTy ids ++"("++ I.cId ids ++", "++ I.rId ids ++"));\n"++
      -- only if we have a closure we emit the code for it
      (if (not $ Cl.empty cls)
        then closure
        else "") ++
      "\t}"
      where
        -- | Transitive closures: stmt = reg, reg = lab, etc...
        closure :: String
        closure =
          "\t\tswitch ("++ I.ntId ids ++") {\n"++
          concatMap
            (\fromL ->
              "\t\t\tcase "++ fromL ++": {\n"++
              concatMap
                (\lab ->
                  "\t\t\t\tlabel ("++ I.nId ids ++", "++ Cl.toL lab ++", "++ I.cId ids ++
                  (if (Cost.isZero (Cl.cost lab))
                      then ", "
                      else "+ "++ show (Cl.cost lab) ++" , ") ++
                  Cl.ruleL lab ++");\n")
                (S.toList $ Cl.toLabels fromL cls)
              ++"\t\t\t\tbreak;\n\t\t\t}\n")
            (Cl.fromLabels cls)  ++"\t\t}\n"

-- | Generates tile method as in [Cooper p.566]
genTileMethod :: I.Ident -> S.Set Operator -> [Int] -> M.Method
genTileMethod ids linkset sets =
  M.new Public True "void" "tile" [Param.new (I.nTy ids) (I.nId ids)] methodBody
  where
    -- | Method body of tile method
    methodBody :: String
    methodBody =
      "\tassert ("++ I.nId ids ++" != null) : \"ERROR: tile() - node is null.\";\n"++
      -- Generate 'If' cascade to distinguish in which Set a node is in
      ifCascade sets ++"\n"++
      if (S.null linkset) -- is there a linkset?
          then ""
          else
            "\tif (linkSet.contains("++ I.nId ids++ ".kind())) {\n"++ 
            "\t\t"++ I.nTy ids ++" link = "++ I.nId ids ++".link();\n"++
            "\t\tif (link != null) tile(link);\n\t}"

    -- Generate the 'if ... else if ... else' cascade in the tiling() method
    ifCascade :: [Int] -> String
    ifCascade [] =
      error "\nERROR: Can not generate tiling() method because no nodes are defined!\n"
    -- if (..) {...}
    ifCascade (x:[]) =
      "\tif ("++ genSetName x ++".contains("++ I.nId ids ++".kind())) {\n"++
      concat [ "\t\ttile("++ I.nId ids ++"."++ (childCallLab pos) ++"());\n" | pos <- [1 .. x]] ++
      "\t\t"++ genLabelMethodName x ++"(n);\n\t}\n"
    -- if (..) {...} else if (..) ... else {...}
    ifCascade (x:xs) =
      "\tif ("++ genSetName x ++".contains("++ I.nId ids ++".kind())) {\n"++
      concat [ "\t\ttile("++ I.nId ids ++"."++ (childCallLab pos) ++"());\n" | pos <- [1 .. x]] ++
      "\t\t"++ genLabelMethodName x ++"("++ I.nId ids ++");\n\t} else "++
      (concatMap
        (\arity -> 
          "if ("++ genSetName arity ++".contains("++ I.nId ids ++".kind())) {\n"++
          concat [ "\t\ttile("++ I.nId ids ++"."++ (childCallLab pos) ++"());\n" | pos <- [1 .. arity]] ++
          "\t\t"++ genLabelMethodName arity ++"("++ I.nId ids ++");\n\t} else ")
        (xs)) ++
      "{\n\t\tthrow new AssertionError(\"ERROR: tile() - Encountered undefined node '\"+ "++
      I.nId ids ++".kind() +\"\'.\");\n\t}\n"

-- | Generate methods which do the actual labelling of AST nodes.
genLabelSetMethods :: I.Ident -> Ir.Ir -> M.Map Int [Production] -> [M.Method]
genLabelSetMethods ids ir prodmap =
  map -- map over all arities and generate methods
    (\(arity, prod) -> genLabelSetMethod arity prod)
    (M.toList prodmap)
  where
    -- | Generate method which labels Nodes with a certain arity
    genLabelSetMethod :: Arity -> [Production] -> M.Method
    genLabelSetMethod arity prods =
      M.new Private True "void" (genLabelMethodName arity) [Param.new (I.nTy ids) (I.nId ids)] methodBody
      where
        -- | methodBody.
        methodBody :: String
        methodBody =
          "\t"++ I.cTy ids ++" "++ I.cId ids ++";\n\tswitch ("++ I.nId ids ++".kind()) {"++
          genCases ++
          -- Error handling in case we are in a label method 
          -- and encounter an unknown kind of node
          "\n\t\tdefault: {\n"++
          "\t\t\tthrow new AssertionError(\"ERROR - "++
          (genLabelMethodName arity) ++
          "(): Unhandeled Node kind: \" + "++I.nId ids ++".kind());\n\t\t}"++
          "\n\t} // END SWITCH"
        
        -- | Generate case stmts. for label methods
        genCases :: String
        genCases =
          M.fold
            (\prods prev ->
                (if ((length prods > 1))
                  then complexCase prods
                  else simpleCase (head prods)) 
                ++ prev)
            ""
            (genProdMap prods)
        
        -- | Generate map holding productions keyed by production identifiers
        -- for faster lookup
        genProdMap :: [Production] -> M.Map String [Production]
        genProdMap prods =
          foldr
            (\p m ->
              let key = show $ getId p in
              if (M.member key m)
                then M.insert key (p:(m M.! key)) m
                else M.insert key [p] m)
            M.empty
            prods
        
        -- | simpleCase.
        simpleCase :: Production -> String
        simpleCase p =
          "\n\t\tcase "++ termToEnumLab p ++": {\n"++
          (costAndLabel p "\t\t\t") ++"\n\t\t\tbreak;"++ "\n\t\t}"
        
        -- | complexCase.
        complexCase :: [Production] -> String
        complexCase prods =
          "\n\t\tcase "++ termToEnumLab (head prods) ++": {"++
          concatMap
            (\p ->
              if (getArity p > 0)
                then
                  "\n\t\t\tif ("++ iff p ++") {\n"++ 
                  (costAndLabel p "\t\t\t\t") ++ 
                  "\n\t\t\t}"
                else "\n"++ (costAndLabel p "\t\t\t"))
            (prods) ++"\n\t\t\tbreak;"++"\n\t\t}"
        
        -- | Create "cost = ...;" and "label(...)" calls.
        costAndLabel :: Production -> String -> String
        costAndLabel p indent =
          let childCalls =
                N.mapChildren
                  (\pos n -> ("."++ childCallLab pos ++"()", n))
                  (getNode p) in
          indent ++ I.cId ids ++" = "++
          (if (not $ null childCalls)
              then
                foldr1
                  (\new old -> new ++" + "++ old)
                  (map
                    (\(call, n) ->
                      I.nId ids ++ call ++".cost("++
                      (case getNodeReturnType (Ir.definitions ir) n of
                        Just term -> termToEnumLab $ nonTerminal term
                        Nothing ->
                          error ("\nERROR: costAndLabel() Node '"++ show n ++"' has no return type!\n")) ++
                      ")"  )
                  childCalls) ++
                  if (Cost.isZero $ getCost p)
                    then ""
                    else " + "++ show (getCost p)
              else show (getCost p)) ++";\n"++
          -- Call label
          indent ++"label("++ I.nId ids ++", "++ (getResultLabel p) ++", "++ I.cId ids ++", "++ (getRuleLabel p) ++");"
        
        -- | Produce code which is used within an 'if' to evaluate if a certain node should be labeled.
        --        Example:
        --          * if (n.child1().kind() = ADD && n.child1().left().is(NT_REG) ...)
        iff :: Production -> String
        iff p =
          let childCalls =
                N.mapPreOrder2
                  (\pos n -> "."++ childCallLab pos ++"()")
                  (\node -> node)
                  (getNode p) in
          foldr1
            (\new old -> new ++" && "++ old)
            (map
              (\(call, n) ->
                if (isTerminal n)
                  then I.nId ids ++ call ++".kind() == "++ termToEnumLab n
                  else
                    I.nId ids ++ call ++".is("++ 
                    (case getNodeReturnType (Ir.definitions ir) n of
                        Just term -> termToEnumLab $ nonTerminal term
                        Nothing -> error "\nERROR: iff() Node has no return type!\n") ++")" )
              (childCalls))

-----------------------------------------------------------------------------