-----------------------------------------------------------------------------
-- |
-- Module      :  Eval
-- Copyright   :  Copyright (c) 2007 Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- This module generates target code executing user supplied semantic actions
-- in the second top-down pass over the intermediate representation.
-----------------------------------------------------------------------------

module Gen.Emit.Eval (
        -- * Functions
        genEval,
    ) where

import Maybe (fromJust, isJust)
import Control.Monad.State

import Ast.Attr (attrGetIn, attrGetOut, attrId, attrTy)
import Ast.Term (TermClass(..))
import qualified Ast.Code as C (Code, isEmpty)
import Ast.Node (Node, Position(..), mapPreOrder3, getSemAct, hasLink, getLink)
import Ast.Prod (getRuleLabel, getNode)
import Ast.Def (Definition, getProds, getCode)
import qualified Ast.Ir as Ir (Ir(..))

import qualified Gen.Ident as I (Ident, rId, nId, nTy, rTy)
import Gen.Emit.Label (termToEvalLab, termToEnumLab, childCallLab)

import Gen.Emit.Class (JavaClass(..))
import Gen.Emit.Java.Class (Java, java)
import Gen.Emit.Java.Modifier (Modifier(..))
import qualified Gen.Emit.Java.Method as Method (Method, new)
import qualified Gen.Emit.Java.Parameter as Parameter (Parameter, new)
-----------------------------------------------------------------------------


-- | This function is the top level function for generating the Target Source Code of the code emission.
genEval :: I.Ident -> Ir.Ir -> Java
genEval ids ir
    = evalState
        (do
            clazz <- get
            put (setModifier clazz Private)
            clazz <- get
            put (setStatic clazz True)
            clazz <- get
            put (setMethods clazz (genEvalMethods ids $ Ir.definitions ir))
            get)
        (java "" "Eval")

returnType :: Definition -> String
returnType d 
    = case attrGetOut (getAttr d) of
        [] -> "void"
        (x:_) -> show (attrTy x)

-- | If a definition returns something (e.g. has an out parameter), this function defines the
--   variable which holds the result.
defineReturnVar :: Definition -> String -> String
defineReturnVar d indent
    = case attrGetOut (getAttr d) of
        [] -> ""
        list -> indent ++ concatMap (\x -> show x ++";\n") list

-- | Generates the return statement given a definition.
returnStmt :: Definition -> String
returnStmt d
    = case attrGetOut (getAttr d) of
        [] -> ""
        list -> concatMap (\x -> "return "++ show (attrId x) ++";\n") list

-- | Calculates parameters for each evaluation method.
genParameters :: Definition -> [Parameter.Parameter]
genParameters d
    = case attrGetIn (getAttr d) of
        [] -> []
        list -> map (\x -> Parameter.new (show $ attrTy x) (show $ attrId x)) list

-- | Generates all evaluation methods which emit code supplied by the user in semantic actions.
genEvalMethods :: I.Ident -> [Definition] -> [Method.Method]
genEvalMethods ids defs
    = map
        (\d ->
            let params = [Parameter.new (I.nTy ids) (I.nId ids)] ++ (genParameters d) in
            Method.new Private True (returnType d) (termToEvalLab d) params (funBody d))
        (defs)
    where
        -- | Compute body of each evaluation method.
        --        Body structure:
        --            1. retrieve rule label for current node
        --            2. define possible return variable if present
        --            3. emit semantic action defined at nt definition level
        --            4. emit cases which emit user supplied code for the various rule labels.
        --            5. return result of this definition if present
        funBody :: Definition ->  String
        funBody d
            = "\t"++ I.rTy ids ++" "++ I.rId ids ++" = "++ I.nId ids ++".rule("++ termToEnumLab d ++");\n"++
            defineReturnVar d "\t\n\t"++
            wrapUserCode "\t" (getCode d) ++
            genCases d "\t\n\t"++
            returnStmt d

        -- | Wraps up user code with '(.' and '.)' so in case of compile errors,
        --  it will be a bit easier to identify automatically generated from user specified code
        wrapUserCode :: String -> C.Code -> String
        wrapUserCode _ code | C.isEmpty code
             = ""
        wrapUserCode indent code
            = indent ++"// (.\n"++
            show code ++
            "\n"++ indent ++"// .)\n"

        -- | Generates case statements for rule labels which. Within each case
        -- statement the semantic actions specifed by the user are inserted.
        genCases :: Definition -> String -> String
        genCases def indent
            = indent ++"switch ("++ (I.rId ids) ++") {\n"++
            cases def ++
            indent ++"\tdefault: {\n"++
            indent ++"\t\tthrow new AssertionError(\"ERROR: Unhandeled semantic rule - \" + "++ (I.rId ids) ++" +\".\");\n"++
            indent ++"\t}\n"++
            indent ++"}"
            where
                -- | Maps all child nodes of a definition AST to code.
                cases :: Definition -> String
                cases def
                    = concatMap
                        (\p ->
                            let childCalls 
                                    = mapPreOrder3
                                        (\pos n -> "."++ childCallLab pos ++"()")
                                        (\path n -> genPreCode path n)
                                        (\path n -> genPostCode path n)
                                        (getNode p)
                                in
                            indent ++"\tcase "++ getRuleLabel p ++": {\n"++
                            nodeBody (getNode p) (childCalls) ++
                            indent ++"\t\tbreak;\n"++
                            indent ++"\t}\n")
                        (getProds def)
                    where
                        nodeBody :: Node -> [(String, String, Node)] -> String
                        nodeBody root nodes
                            = genPreCode "" root ++         -- code which goes before processing children (evaluation methods)
                            (concatMap                      -- process children by calling the appropriate evaluation methods
                                (\(path, code, n) -> code)
                                (nodes)) ++
                            genPostCode "" root             -- code which goes after processing children

                        -- | genPreCode.
                        genPreCode :: String -> Node -> String
                        genPreCode path n
                            = -- First Semantic action 
                            wrapUserCode "\t\t\t" (getSemAct Pos1 n) ++
                            -- If this is a Nt emit a function call to respective eval method
                            (if (isNonTerminal n)
                                then
                                    let ret = (genFunRetVal n) in
                                    "\t\t\t"++ 
                                    -- If there are out parameters we assign the fun call to them
                                    (if (isJust ret)
                                        then (fst (fromJust ret)) ++" "++ (snd (fromJust ret)) ++" = "
                                        else "") ++
                                    genFunCall n path ++";\n"
                                else "") ++
                            -- Generate binding code if present
                            (if (hasBinding n)
                                then "\t\t\t"++ genBinding n path
                                else "") ++
                            -- Second Semantic action
                            wrapUserCode "\t\t\t" (getSemAct Pos2 n)

                        -- | genPostCode.
                        genPostCode :: String -> Node -> String
                        genPostCode path n
                            = -- Third semantic action
                            wrapUserCode "\t\t\t" (getSemAct Pos3 n) ++
                            -- Emit code for link evaluation
                            (if (hasLink n)
                                then 
                                    let ret = (genFunRetVal $ getLink n) in
                                    "\t\t\tif ("++ (I.nId ids) ++".link() != null) {\n"++
                                    wrapUserCode "\t\t\t\t" (getSemAct Pos5 n) ++
                                    (if (isJust ret)
                                        then
                                            "\t\t\t\t"++ (fst (fromJust ret)) ++" "++ (snd (fromJust ret)) ++" = "
                                        else
                                            "\t\t\t\t") ++
                                    (genFunCall (getLink n) ".link()") ++";\n"++
                                    wrapUserCode "\t\t\t" (getSemAct Pos6 n) ++"\t\t\t}\n"
                                else "") ++
                            -- Fourth semantic action
                            wrapUserCode "\t\t\t" (getSemAct Pos4 n)

                        -- | genBinding.
                        genBinding :: TermClass a => a -> String -> String
                        genBinding term path | hasBinding term
                            = I.nTy ids ++" "++ (show (getBinding term)) ++" = "++ I.nId ids ++ path ++";\n"
                        genBinding _ _ = ""

                        -- | Given a NonTerm, this function gives the the return value as a
                        --  definition (e.g. List<String> list) in the form of a tuple where
                        --   fst is the type and snd is the identifier.
                        genFunRetVal :: TermClass a => a -> Maybe (String, String)
                        genFunRetVal term | (isNonTerminal term)
                            = let outattr 
                                    = map
                                        (\a -> (show (attrTy a), show (attrId a)))
                                        (attrGetOut (getAttr term))
                                in
                            case outattr of
                                [] -> Nothing
                                (ret:_) -> Just ret
                        genFunRetVal _ = Nothing

                        -- | genFunCall.
                        genFunCall :: TermClass a => a -> String ->  String
                        genFunCall term path | (isNonTerminal term)
                            = let inattrs 
                                    = concatMap 
                                        (\a -> ", "++ show (attrId a))
                                        (attrGetIn (getAttr term))
                                in
                            let funname = termToEvalLab term in
                            funname ++"("++ I.nId ids ++ path ++ inattrs ++")"
                        genFunCall _ _ = ""