-----------------------------------------------------------------------------
-- |
-- Module      :  Tile
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- This module generates tiling target source code and the appropriate 
-- Java node interface.
-----------------------------------------------------------------------------

module Gen.Emit.Tile (
        -- * Functions
        genTiling,
    ) where

import Control.Monad.State

import Ast.Term (TermClass(..), nonTerminal)
import qualified Ast.Node as N (mapChildren, mapPreOrder2)
import Ast.Op (Operator, opSem)
import Ast.Def (getNodeReturnType)
import Ast.Cost as Cost (isZero)
import Ast.Prod (Production, getCost, getNode, getRuleLabel, getResultLabel, getArity)
import qualified Ast.Ir as Ir (Ir(..), baseRuleMap, linkSet)
import qualified Ast.Closure as Cl (Closure, Label(..), closure, fromLabels, toLabels, empty)

import Gen.Emit.Label (termToEnumLab, childCallLab)

import Gen.Emit.Class (JavaClass(..))
import Gen.Emit.Java.Class (Java, java)
import qualified Gen.Emit.Java.Comment as Comment (new)
import qualified Gen.Emit.Java.Method as Meth (Method, new, setComment)
import Gen.Emit.Java.Modifier (Modifier(..))
import qualified Gen.Emit.Java.Variable as Var (Variable, new)
import qualified Gen.Emit.Java.Parameter as Param (new, newFromList)

import qualified Data.Set as S
import qualified Data.Map as M
------------------------------------------------------------------------------------

type NodeKind = String
type Arity = Int

-- | Top level function for generating tiling target source code and the appropriate node interface.
genTiling :: String -> NodeKind -> Ir.Ir -> Java
genTiling pkg nkind ir
    = let prodmap = Ir.baseRuleMap ir in
    let linkset = Ir.linkSet ir in
    let vars = genEnumSetVars ir linkset in     -- generate EnumSet variables
    let closures = Cl.closure $ Ir.definitions ir in
    evalState
        (do
            clazz <- get
            put (setModifier clazz Private)
            clazz <- get
            put (setStatic clazz True)
            clazz <- get
            put (setVariables clazz vars)
            clazz <- get
            put (setMethods
                    clazz
                    $ [  genLabelMethod closures        -- generate label() mehtod
                      ,  genTileMethod                  -- generate tile() method [Cooper p.566]
                            linkset (M.keys prodmap)]
                      ++ genLabelSetMethods ir prodmap) -- generate label_N() methods for nodes with arity N
            get)
        (java "" "Tiling")


-- | Generate a name for node sets based on node arity.
genSetName :: Int -> String
genSetName arity = "arity" ++ (show arity) ++ "Set"

-- | Generate a name for the label method depending on the arity of the nodes.
genLabelMethodName :: Int -> String
genLabelMethodName arity = "label_" ++ (show arity)


-- | Produce EnumSet variables.
--      * Example: private static EnumSet unarySet = EnumSet.of(FUN,FUNAP,SIDEFFECT)
genEnumSetVars :: Ir.Ir -> S.Set Operator -> [Var.Variable]
genEnumSetVars ir linkset
    = let opsets = Ir.operatorMap ir in
    let vars = map
                    (\key -> genVar (genSetName key) (opsets M.! key))
                    (M.keys opsets)
            in
    if (S.null linkset)
        then vars
        else (genVar "linkSet" linkset):vars
    where
        -- | genVar.
        genVar :: String -> S.Set Operator -> Var.Variable
        genVar name opset
            = Var.new Private True "EnumSet"  name
                    ("EnumSet.of(" ++ (transformOpSet opset) ++ ")")

        -- | Make String representation from an Operator set
        transformOpSet :: S.Set Operator -> String
        transformOpSet opset
            = (S.fold
                (\x y -> case y of
                            [] -> show (opSem x)
                            otherwise -> (show (opSem x)) ++ ", " ++ y )
                ""
                opset)

-- | Generates label method.
genLabelMethod :: Cl.Closure -> Meth.Method
genLabelMethod cls
    = let params = Param.newFromList [("Node","n"), ("NT","nt"), ("int","c"), ("RuleEnum","r")] in
    let m = Meth.new Private True "void" "label" params funBody in
    Meth.setComment m (Comment.new ["label():","  Label each AST node appropriately."])
    where
        -- | Function Body of label method
        funBody :: String
        funBody 
            = "\tif (c < n.cost(nt)) {\n" ++
            "\t\tn.put(nt, new MapEntry(c, r));\n" ++
            -- only if we have a closure we emit the code for it
            (if (not $ Cl.empty cls)
                then closure
                else "") ++
            "\t}"
            where
                -- | Transitive closures: stmt = reg, reg = lab, etc...
                closure :: String
                closure 
                    = "\t\tswitch (nt) {\n" ++
                    concatMap
                        (\fromL ->
                            "\t\t\tcase " ++ fromL ++ ": {\n" ++
                            concatMap
                                (\lab ->
                                     "\t\t\t\tlabel (n, " ++ Cl.toL lab ++
                                     ", n.cost(nt) " ++
                                     (if (Cost.isZero (Cl.cost lab))
                                         then ", "
                                         else "+ " ++ show (Cl.cost lab) ++ " , ") ++
                                    Cl.ruleL lab ++ ");\n")
                                (S.toList $ Cl.toLabels fromL cls)
                            ++ "\t\t\t\tbreak;\n\t\t\t}\n")
                        (Cl.fromLabels cls)  ++ "\t\t}\n"

-- | Generates tile method as in [Cooper p.566]
genTileMethod :: S.Set Operator -> [Int] -> Meth.Method
genTileMethod linkset sets
    = let m = Meth.new Public True "void" "tile" [Param.new "Node" "n"] funBody in 
    Meth.setComment m (Comment.new ["tile():" , "   Tile the AST as in [Cooper p.566]"])
    where
        -- | Function Body of tile method
        funBody :: String
        funBody
            = "\tassert (n != null) : \"ERROR - Can not tile null node.\";\n" ++
            -- Generate 'If' cascade to distinguish in which Set a node is in
            ifCascade sets ++ "\n" ++
            if (not (S.null linkset)) -- is there a linkset?
                then
                    "\tif (linkSet.contains(n.kind())) {\n" ++ 
                    "\t\tNode link = n.link();\n" ++
                    "\t\tif (link != null) tile(link);\n\t}"
                else ""

        -- Generate the 'if ... else if ... else' cascade in the tiling() method
        ifCascade :: [Int] -> String
        ifCascade [] 
            = error "\nERROR: Can not generate tiling() method because no nodes are defined!\n"
        -- if (..) {...}
        ifCascade (x:[])
            = "\tif (" ++ genSetName x ++ ".contains(n.kind())) {\n" ++
            concat [ "\t\ttile(n." ++ (childCallLab pos) ++ "());\n" | pos <- [1 .. x]] ++
            "\t\t" ++ genLabelMethodName x ++ "(n);\n\t}\n"
        -- if (..) {...} else if (..) ... else {...}
        ifCascade (x:xs)
            = "\tif (" ++ genSetName x ++ ".contains(n.kind())) {\n" ++
            concat [ "\t\ttile(n." ++ (childCallLab pos) ++ "());\n" | pos <- [1 .. x]] ++
            "\t\t" ++ genLabelMethodName x ++ "(n);\n\t} else " ++
            (concatMap
                (\arity -> 
                    "if ("++ genSetName arity ++".contains(n.kind())) {\n" ++
                    concat [ "\t\ttile(n." ++ (childCallLab pos) ++ "());\n" | pos <- [1 .. arity]] ++
                    "\t\t"++ genLabelMethodName arity ++"(n);\n\t} else ")
                (xs)) ++
            "{\n\t\tSystem.err.println(\"ERROR: Encountered undefined node: \" + n.kind() );\n\t}\n"

-- | Generate methods which do the actual labelling of AST nodes.
genLabelSetMethods :: Ir.Ir -> M.Map Int [Production] -> [Meth.Method]
genLabelSetMethods ir prodmap
    = map -- Iterate over all arities and generate methods
        (\key -> genLabelSetMethod key (prodmap M.! key))
        (M.keys prodmap)
    where
        -- | Generate method which labels Nodes with a certain arity
        genLabelSetMethod :: Arity -> [Production] -> Meth.Method
        genLabelSetMethod arity prods
            = let m = Meth.new Private True "void" (genLabelMethodName arity) [Param.new "Node" "n"] funBody in
            Meth.setComment m (Comment.new [genLabelMethodName arity ++ "():" , "  Label nodes with arity " ++ show arity])
            where
                -- | funBody. Function Body
                funBody :: String
                funBody
                    =  "\tint cost;\n\tswitch (n.kind()) {" ++
                    genCases ++
                    -- Error handling in case we are in a label method 
                    -- and encounter an unknown kind of node
                    "\n\t\tdefault: {\n" ++
                    "\t\t\tthrow new AssertionError(\"ERROR - " ++ 
                    (genLabelMethodName arity) ++ 
                    "(): Unhandeled Node kind: \" + n.kind());\n\t\t}" ++
                    "\n\t} // END SWITCH"

                -- | Generate case stmts. for label methods
                genCases :: String
                genCases
                    = M.fold
                        (\prods old ->
                            (if ((length prods > 1))
                                then complexCase prods
                                else simpleCase (head prods)) 
                            ++ old)
                        ""
                        (genProdMap prods)

                -- | Generate map holding productions keyed by arity for faster lookup
                genProdMap :: [Production] -> M.Map String [Production]
                genProdMap prods
                    = foldr
                        (\p m ->
                            let key = show $ getId p in
                            if (M.member key m)
                                then M.insert key (p:(m M.! key)) m
                                else M.insert key [p] m)
                        M.empty
                        prods

                -- | simpleCase.
                simpleCase :: Production -> String
                simpleCase p 
                    = "\n\t\tcase " ++ termToEnumLab p ++ ": {\n" ++
                    (costAndLabel p "\t\t\t") ++ "\n\t\t\tbreak;" ++
                    "\n\t\t}"

                -- | complexCase.
                complexCase :: [Production] -> String
                complexCase prods
                    = "\n\t\tcase " ++ termToEnumLab (head prods) ++ ": {" ++
                    concatMap
                        (\p ->
                            if (getArity p > 0)
                                then "\n\t\t\tif (" ++ iff p ++ ") {\n" ++ 
                                        (costAndLabel p "\t\t\t\t") ++ 
                                        "\n\t\t\t}"
                                else "\n" ++ (costAndLabel p "\t\t\t"))
                        (prods) ++ "\n\t\t\tbreak;" ++
                    "\n\t\t}"

                -- | Create "cost = ...;" and "label(...)" calls.
                costAndLabel :: Production -> String -> String
                costAndLabel p indent
                    = let childCalls 
                            = N.mapChildren
                                (\pos n -> ("." ++ childCallLab pos ++ "()", n))
                                (getNode p)
                        in
                    indent ++ "cost = " ++
                    (if (childCalls /= [])
                        then (foldr1
                                (\new old -> new ++ " + " ++ old)
                                (map
                                    (\(call, n) ->
                                            "n" ++ call ++ ".cost(" ++
                                            (case getNodeReturnType (Ir.definitions ir) n of
                                                Just term -> termToEnumLab $ nonTerminal term
                                                Nothing -> error ("\nERROR: costAndLabel() Node '"++ show n ++"' has no return type!\n")) ++
                                            ")"  )
                                childCalls)) ++
                                if (Cost.isZero $ getCost p)
                                    then ""
                                    else " + " ++ show (getCost p)
                        else show (getCost p))
                    ++ ";\n" ++
                    -- Call label
                    indent ++ "label(n, " ++ (getResultLabel p) ++ ", cost, " ++ (getRuleLabel p) ++ ");"

                -- | Produce code which is used within an 'if' to evaluate if a certain node should be labeled.
                --        Example:
                --          * if (n.child1().kind() = ADD && n.child1().left().is(NT_REG) ...)
                iff :: Production -> String
                iff p = let childCalls
                                = N.mapPreOrder2
                                    (\pos n -> "." ++ childCallLab pos ++ "()")
                                    (\node -> node)
                                    (getNode p)
                            in
                    foldr1
                        (\new old -> new ++ " && " ++ old)
                        (map
                            (\(call, n) ->
                                if (isTerminal n)
                                    then "n" ++ call ++ ".kind() == " ++ termToEnumLab n
                                    else "n" ++ call ++ ".is(" ++ 
                                            (case getNodeReturnType (Ir.definitions ir) n of
                                                Just term -> termToEnumLab $ nonTerminal term
                                                Nothing -> error "\nERROR: iff() Node has no return type!\n") ++ ")" )
                            (childCalls))