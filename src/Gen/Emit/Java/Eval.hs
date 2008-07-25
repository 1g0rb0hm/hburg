-----------------------------------------------------------------------------
-- |
-- Module      :  Eval
-- Copyright   :  Copyright (c) 2007 Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module generates target code executing user supplied semantic actions
-- in the second top-down pass over the intermediate representation.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Eval (
  -- * Functions
  eval,
) where

{- unqualified imports  -}
import Maybe (fromJust, isJust)
import Control.Monad.State

import Text.PrettyPrint

import Ast.Attr (attrGetIn, attrGetOut, attrId, attrTy)
import Ast.Term (TermClass(..))
import Ast.Node (Node, Position(..), mapPreOrder3, getSemAct, hasLink, getLink)
import Ast.Prod (getRuleLabel, getNode)
import Ast.Def (Definition, getProds, getCode)

import Gen.Emit.Label (termToEvalLab, termToEnumLab, childCallLab)
import Gen.Emit.Java.Modifier (Modifier(..))

{- qualified imports  -}
import qualified Ast.Code as C (Code, isEmpty)
import qualified Ast.Ir as Ir (Ir(..))

import qualified Gen.Ident as I (Ident, rId, nId, nTy, rTy)
import qualified Gen.Emit.Java.Class as Class (Class(..), new)
import qualified Gen.Emit.Java.Method as Method (Method, new)
import qualified Gen.Emit.Java.Param as P (Param, new)

-----------------------------------------------------------------------------

{- | This function is the top level function for generating the target source
     code of the code emission. -}
eval :: I.Ident -> Ir.Ir -> Class.Class
eval ids ir =
  execState
    (do
      modify $ \c ->
                c { Class.modifier = Private
                  , Class.isStatic = True
                  , Class.methods = evalMethods ids $ Ir.definitions ir })
    (Class.new "" "Eval")

{- | Short cut -}
t :: String -> Doc
t = text

{- | returnType. -}
returnType :: Definition -> String
returnType d = 
  case attrGetOut (getAttr d) of
    [] -> "void"
    (x:_) -> show (attrTy x)

{- | If a definition returns something (e.g. has an out parameter), this
     function defines the variable which holds the result. -}
returnVar :: Definition -> Doc
returnVar d =
  case attrGetOut (getAttr d) of
    [] -> empty
    xs -> vcat $ map (\x -> t (show x) <> semi) xs

{- | Calculates parameters for each evaluation method. -}
params :: Definition -> [P.Param]
params d =
  case attrGetIn (getAttr d) of
    [] -> []
    xs -> map (\x -> P.new (show $ attrTy x) (show $ attrId x)) xs

{- | Generates all evaluation methods which emit code supplied by the user
     in semantic actions. -}
evalMethods :: I.Ident -> [Definition] -> [Method.Method]
evalMethods ids defs =
  map (\d ->
        let ps = (P.new (I.nTy ids) (I.nId ids)) : (params d) in
        Method.new Private True (returnType d) (termToEvalLab d) ps (body d))
      (defs)
  where
    {- | Compute body of each evaluation method.
         Body structure:
            1. retrieve rule label for current node
            2. define possible return variable if present
            3. emit semantic action defined at Nt definition level
            4. emit cases which emit user supplied code for the various rule labels.
            5. return result of this definition if present -}
    body :: Definition ->  Doc
    body d = t (I.rTy ids) <+> t (I.rId ids) <+> equals -- 1.
        <+> t (I.nId ids) <> t ".rule" <> parens (t $ termToEnumLab d) <> semi
      $+$ returnVar d                                   -- 2.
      $+$ userCode (getCode d)                          -- 3.
      $+$ switch d                                      -- 4.
      $+$ case attrGetOut (getAttr d) of                -- 5.
        [] -> empty
        xs -> t "return" <+> t (show . attrId $ head xs) <> semi

    {- | Wraps up user code with '(.' and '.)' so in case of compile errors,
        it will be a bit easier to identify automatically generated from user
        specified code -}
    userCode :: C.Code -> Doc
    userCode code | C.isEmpty code = empty
    userCode code = t "// (." $+$ t (show code) $+$ t "// .)"
    
    {- | Generates case statements for rule labels which. Within each case
         statement the semantic actions specifed by the user are inserted. -}
    switch :: Definition -> Doc
    switch def = t "switch" <+> parens (t $ I.rId ids)
      <+> lbrace
        -- cases of switch statements
        $+$ nest 2 (vcat $ cases def)
        -- default branch of switch statement
        $+$ nest 2 (t "default" <> colon <+>
              lbrace
              $+$ nest 2 (t "throw new AssertionError" <>
                parens (doubleQuotes (t "ERROR: Unhandled semantic rule - ") <>
                  t "+" <+> t (I.rId ids) <+> t "+" <> doubleQuotes (t "."))
                <> semi)
              $+$ rbrace
            )
      $+$ rbrace
      where
        {- | Maps all child nodes of a definition AST to code. -}
        cases :: Definition -> [Doc]
        cases def =
          map
            (\p ->
                let childCalls =
                      mapPreOrder3
                        (\pos  n -> [t "." <> t (childCallLab pos) <> parens (empty)])
                        (\path n -> [preCode  n $ hcat path])
                        (\path n -> [postCode n $ hcat path])
                        (getNode p) in
                t "case" <+> t (getRuleLabel p) <> colon
                <+> lbrace
                  $+$ nest 2 (nodeBody (getNode p) (childCalls)
                    $+$ t "break" <> semi)
                $+$ rbrace)
            (getProds def)
          where
            {- | nodeBody. -}
            nodeBody :: Node -> [([Doc], [Doc], Node)] -> Doc
            nodeBody root nodes =
              preCode root empty      -- code which goes before processing children (evaluation methods)
              $+$ vcat
                (map                  -- process children by calling the appropriate evaluation methods
                  (\(_,code,_) -> vcat code)
                  (nodes))
              $+$ postCode root empty -- code which goes after processing children
            
            {- | preCode. Code which goes before processing children (evaluation methods) -}
            preCode :: Node -> Doc -> Doc
            preCode n path =
              -- FIRST SEMANTIC ACTION
              userCode (getSemAct Pos1 n)
              $+$ -- If this is a Nt emit a function call to respective eval method
              (if (isNonTerminal n)
                then let ret = returnDef n in
                  -- If there are out parameters we assign the fun call to them
                  (if (isJust ret)
                    then fst (fromJust ret) <+> snd (fromJust ret)
                      <+> equals
                    else empty)
                  <+> methodCall n path <> semi
                else empty)
              $+$ -- Generate binding code if present
              (if (hasBinding n)
                  then bind n path
                  else empty)
              $+$ -- SECOND SEMANTIC ACTION
              userCode (getSemAct Pos2 n)
            
            {- | postCode. Code which goes after processing children -}
            postCode :: Node -> Doc -> Doc
            postCode n path =
              -- THIRD SEMANTIC ACTION
              userCode (getSemAct Pos3 n)
              $+$ -- Emit code for link evaluation
              (if (hasLink n)
                then let ret = returnDef $ getLink n in
                  t "if" <+>
                    parens (t (I.nId ids) <> t ".link() != null")
                    <+> lbrace
                        -- 1. SEMANTIC ACTION IN LINK BLOCK
                      $+$ nest 2 (userCode (getSemAct Pos5 n)
                        $+$ (if (isJust ret)
                          then fst (fromJust ret) <+> snd (fromJust ret)
                            <+> equals
                          else empty)
                        <+> methodCall (getLink n) (t ".link()") <> semi
                        -- 2. SEMANTIC ACTION IN LINK BLOCK
                        $+$ userCode (getSemAct Pos6 n))
                    $+$ rbrace
                else empty)
              $+$ -- FOURTH SEMANTIC ACTION
              userCode (getSemAct Pos4 n)
            
            {- | bind. -}
            bind :: TermClass a => a -> Doc -> Doc
            bind term path | hasBinding term =
              t (I.nTy ids) <+> t (show (getBinding term)) <+> equals
              <+> t (I.nId ids) <> path <> semi
            bind _ _ = empty
            
            {- | Given a NonTerm, this function gives the return value as a
                 definition (e.g. List<String> list) in the form of a tuple where
                 fst is the type and snd is the identifier. -}
            returnDef :: TermClass a => a -> Maybe (Doc, Doc)
            returnDef term | (isNonTerminal term) =
              let outattr =
                    map
                      (\a -> (t (show (attrTy a)), t (show (attrId a))))
                      (attrGetOut (getAttr term)) in
              if (null outattr)
                then Nothing
                else Just $ head outattr
            returnDef _ = Nothing
            
            {- | methodCall. -}
            methodCall :: TermClass a => a -> Doc ->  Doc
            methodCall term path | (isNonTerminal term) =
              let inattrs =
                    map
                      (\a -> comma <+> t (show (attrId a)))
                      (attrGetIn (getAttr term)) in
              t (termToEvalLab term)              -- function name
              <> parens (t (I.nId ids) <> path <> -- node path
                         hcat inattrs)            -- input attributes
            methodCall _ _ = empty

-----------------------------------------------------------------------------