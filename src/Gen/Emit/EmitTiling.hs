-----------------------------------------------------------------------------
-- |
-- Module      :  EmitTiling
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- This module generates tiling target source code and the appropriate 
-- Java node interface.
-----------------------------------------------------------------------------

module Gen.Emit.EmitTiling (
        -- * Functions
        genTiling,
    ) where

import List (sort)

import Ast.TermTy (TermTyClass(..))
import qualified Ast.Node as N (mapChildren, mapPreOrder2, getName)
import Ast.Op (Operator, opSem)
import Ast.Def (Definition, getNodeReturnType)
import Ast.Prod (Production, getName, getCost, getNode, getRuleLabel, getResultLabel, getArity)

import qualified Gen.Tile as T (Tiling, Arity,
    new, getClosures, getProductionsPerArity,
    getOperatorsPerArity, getLinkSet,
    closureGetFromLabel, closureGetToLabel,
    closureGetRuleLabel, hasClosures)

import Gen.Emit.Label (ntToEnumLabel, childCallLabel)

import Gen.Emit.JavaClass (JavaClass(..))
import Gen.Emit.EmitNodeIf (genNodeInterface)
import Gen.Emit.Java.Java (Java, java)
import qualified Gen.Emit.Java.JComment as Comment (new)
import qualified Gen.Emit.Java.JMethod as Method (JMethod, new, setComment)
import Gen.Emit.Java.JModifier (JModifier(..))
import qualified Gen.Emit.Java.JVariable as Variable (JVariable, new)
import qualified Gen.Emit.Java.JParameter as Parameter (new, newFromList)

import qualified Data.Set as S
import qualified Data.Map as M
------------------------------------------------------------------------------------

type NodeKind = String

-- | Top level function for generating tiling target source code and the appropriate node interface.
genTiling :: [Operator] -> [Definition] -> NodeKind -> Java -> (Java, Java)
genTiling ops defs nkind parentClass
    = let tile = T.new ops defs in                      -- produce Tiling
    let j0 = jSetModifier (java "" "Tiling") Private in -- create new class which will hold tilings
    let j1 = jSetStatic j0 True in
    let (vars, sets) = genEnumSetVars tile in           -- generate EnumSet variables
    let nodeIf
            = genNodeInterface                          -- generate Node interface
                        (jGetPackage parentClass)       -- package name
                        (last (sort sets))              -- max. amount of children node can have
                        (not
                            (S.null
                            (T.getLinkSet tile)))       -- do link nodes exist
                        nkind                           -- return type of node
        in
    let j2 = jSetVariables j1 vars in
    let mLab = genLabelMethod tile in                   -- generate label() mehtod
    let mTile = genTileMethod tile sets in              -- generate tile() method as in in [Cooper p.566]
    let mLabS = genLabelSetMethods defs tile in         -- generate label_N() methods for nodes with arity N
    let nestedClass 
            = jSetMethods j2                            -- finally set all methods for this tiling class
                ([mLab, mTile] ++ mLabS ++
                    if (T.getClosures tile /= [])       -- only generate closure() method if there are closures
                        then [genClosureMethod tile]
                        else [])
        in
    (jSetNestedClasses parentClass [nestedClass], nodeIf)   -- the Tiling class is nested into another class


-- | Generate a name for node sets based on node arity.
genSetName :: Int -> String
genSetName arity = "arity" ++ (show arity) ++ "Set"

-- | Generate a name for the label method depending on the arity of the nodes.
genLabelMethodName :: Int -> String
genLabelMethodName arity = "label_" ++ (show arity)


-- | Produce EnumSet variables.
--      * Example: private static EnumSet unarySet = EnumSet.of(FUN,FUNAP,SIDEFFECT)
genEnumSetVars :: T.Tiling -> ([Variable.JVariable], [Int])
genEnumSetVars tile
    = let opsets = T.getOperatorsPerArity tile in
    let linkset = T.getLinkSet tile in
    let (vars, sets) = unzip (map
                        (\key -> (genVar (genSetName key) (opsets M.! key), key))
                        (M.keys opsets))
            in
    if (S.null linkset)
        then (vars, sets)
        else ((genVar "linkSet" linkset):vars, sets)
    where
        -- | genVar.
        genVar :: String -> S.Set Operator -> Variable.JVariable
        genVar name opset
            = Variable.new Private True "EnumSet"  name
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
genLabelMethod :: T.Tiling -> Method.JMethod
genLabelMethod tiling
    = let params = Parameter.newFromList [("Node","n"), ("NT","nt"), ("int","c"), ("RuleEnum","r")] in
    let m = Method.new Private True "void" "label" params funBody in
    Method.setComment m (Comment.new ["label():","  Label each AST node appropriately."])
    where
        -- | Function Body of label method
        funBody :: String
        funBody 
            = "\tif (c < n.cost(nt)) {\n" ++
            "\t\tn.put(nt, new MapEntry(c, r));\n" ++
            -- only if we have a closure we emit the code for it
            (if (T.hasClosures tiling)
                then "\t\tclosure(n, nt, r);\n"
                else "") ++
            "\t}"

-- | Generate closure method.
genClosureMethod :: T.Tiling -> Method.JMethod
genClosureMethod t
    = let params = Parameter.newFromList [("Node","n"), ("NT","nt"), ("RuleEnum","r")] in
    let m = Method.new Private True "void" "closure" params funBody in
    Method.setComment m (Comment.new ["closure():", "  Calculate the transitive closure for this Node."])
    where
        -- | Function Body of closure method
        funBody :: String
        funBody 
            = let closures = T.getClosures t in
            "\tswitch (nt) {\n" ++
            concat ["\t\tcase " ++ T.closureGetFromLabel c ++ ": {\n" ++
                 "\t\t\tlabel (n, " ++ T.closureGetToLabel c ++  
                 ", n.cost(nt), " ++ T.closureGetRuleLabel c ++ 
                 "); break;\n\t\t}\n"
                | c <- closures ] ++ "\t}"

-- | Generates tile method as in [Cooper p.566]
genTileMethod :: T.Tiling -> [Int] -> Method.JMethod
genTileMethod tiling sets
    = let m = Method.new Public True "void" "tile" [Parameter.new "Node" "n"] funBody in 
    Method.setComment m (Comment.new ["tile():" , "   Tile the AST as in [Cooper p.566]"])
    where
        -- | Function Body of tile method
        funBody :: String
        funBody
            = let hasLinkSet = (not (S.null (T.getLinkSet tiling))) in
            "\tassert (n != null) : \"ERROR - Can not tile null node.\";\n" ++
            -- Generate 'If' cascade to distinguish in which Set a node is in
            ifCascade sets ++ "\n" ++
            if (hasLinkSet)
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
            concat [ "\t\ttile(n." ++ (childCallLabel pos) ++ "());\n" | pos <- [1 .. x]] ++
            "\t\t" ++ genLabelMethodName x ++ "(n);\n\t}\n"
        -- if (..) {...} else if (..) ... else {...}
        ifCascade (x:xs)
            = "\tif (" ++ genSetName x ++ ".contains(n.kind())) {\n" ++
            concat [ "\t\ttile(n." ++ (childCallLabel pos) ++ "());\n" | pos <- [1 .. x]] ++
            "\t\t" ++ genLabelMethodName x ++ "(n);\n\t} else " ++
            (concatMap
                (\arity -> 
                    "if (" ++ genSetName arity ++".contains(n.kind())) {\n" ++
                    concat [ "\t\ttile(n." ++ (childCallLabel pos) ++ "());\n" | pos <- [1 .. arity]] ++
                    "\t\t" ++ genLabelMethodName arity ++ "(n);\n\t} else ")
                (xs)) ++
            "{\n\t\tSystem.err.println(\"ERROR: Encountered undefined node: \" + n.kind() );\n\t}\n"

-- | Generate methods which do the actual labelling of AST nodes.
genLabelSetMethods :: [Definition] -> T.Tiling -> [Method.JMethod]
genLabelSetMethods defs tiling
    = let funmap = T.getProductionsPerArity tiling in
    map -- Iterate over all arities and generate methods
        (\key -> genLabelSetMethod key (funmap M.! key))
        (M.keys funmap)
    where
        -- | Generate method which labels Nodes with a certain arity
        genLabelSetMethod :: T.Arity -> [Production] -> Method.JMethod
        genLabelSetMethod arity prods
            = let m = Method.new Private True "void" (genLabelMethodName arity) [Parameter.new "Node" "n"] funBody in
            Method.setComment m (Comment.new [genLabelMethodName arity ++ "():" , "  Label nodes with arity " ++ show arity])
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
                            let key = getName p in
                            if (M.member key m)
                                then M.insert key (p:(m M.! key)) m
                                else M.insert key [p] m)
                        M.empty
                        prods

                -- | simpleCase.
                simpleCase :: Production -> String
                simpleCase p 
                    = "\n\t\tcase " ++ getName p ++ ": {\n" ++
                    (costAndLabel p "\t\t\t") ++ "\n\t\t\tbreak;" ++
                    "\n\t\t}"

                -- | complexCase.
                complexCase :: [Production] -> String
                complexCase prods
                    = "\n\t\tcase " ++ getName (head prods) ++ ": {" ++
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
                                (\pos n -> ("." ++ childCallLabel pos ++ "()", n))
                                (getNode p)
                        in
                    indent ++ "cost = " ++
                    concatMap 
                        (\(call, n) ->  
                                "n" ++ call ++ ".cost(" ++ 
                                (case getNodeReturnType defs n of
                                    Just ty -> ntToEnumLabel ty
                                    Nothing -> error ("\nERROR: costAndLabel() Node '"++ show n ++"' has no return type!\n")) ++
                                ") + "  )
                        (childCalls) ++
                    show (getCost p) ++ ";\n" ++
                    -- Call label
                    indent ++ "label(n, " ++ (getResultLabel p) ++ ", cost, " ++ (getRuleLabel p) ++ ");"

                -- | Produce code which is used within an 'if' to evaluate if a certain node should be labeled.
                --        Example:
                --          * if (n.child1().kind() = ADD && n.child1().left().is(NT_REG) ...)
                iff :: Production -> String
                iff p = let childCalls
                                = N.mapPreOrder2
                                    (\pos n -> "." ++ childCallLabel pos ++ "()")
                                    (\node -> node)
                                    (getNode p)
                            in
                    foldr1
                        (\new old -> new ++ " && " ++ old)
                        (map
                            (\(call, n) ->
                                if (isTerm n)
                                    then "n" ++ call ++ ".kind() == " ++ N.getName n
                                    else "n" ++ call ++ ".is(" ++ 
                                            (case getNodeReturnType defs n of
                                                Just ty -> ntToEnumLabel ty
                                                Nothing -> error "\nERROR: iff() Node has no return type!\n") ++ ")" )
                            (childCalls))