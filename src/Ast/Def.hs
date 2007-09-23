-----------------------------------------------------------------------------
-- |
-- Module      :  Def
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module provides the necessary abstractions to work with definitions.
-- Each non terminal must be defined and consists of one or more productions:
--  * Example: nt = prod1
--                | prod2
--                | ...
--                | prodN .
-----------------------------------------------------------------------------

module Ast.Def (
        -- * Types
        Definition,
        -- * Construction
        new,
        -- * Functions
        getCode, getProds, getClosures,
        getNodeReturnType, getDefForProd,
        setProds,
        isNodeDefined,
        mergeDefs,
    ) where

import List (find)
import Maybe (isJust)

import Debug (Debug(..))

import qualified Ast.Ident as Id (Ident)
import Ast.Attr (Attr, attrEqualInOut)
import qualified Ast.Bind as B (empty)
import Ast.Node (Node, getTerm)
import qualified Ast.Nt as Nt (Nt, new, getIdent, getAttr, getBinding, hasBinding)
import Ast.Term (TermClass(..))
import Ast.Code (Code)
import qualified Ast.Prod as P (Production, getNode, isDefined, getProdsByIdent)

import qualified Csa.Elem as E (ElemClass(..), ElemType(EDef))
------------------------------------------------------------------------------------

type Closure = [P.Production]

-- | Non terminal definition type
data Definition
    = Def { nt      :: Nt.Nt            -- ^ the non terminal being defined
          , code    :: Code             -- ^ associated semantic action
          , prods   :: [P.Production]}  -- ^ a definition consists of productions

instance Eq Definition where
    (==) d1 d2 =  nt d1 == nt d2

instance Ord Definition where
    compare d1 d2 = compare (nt d1) (nt d2)

instance E.ElemClass Definition where
    elemShow d = E.elemShow (nt d)
    elemType _ = E.EDef
    elemL d = E.elemL (Nt.getIdent(nt d))
    elemC d = E.elemC (Nt.getIdent(nt d))

instance Show Definition where
    show d
        = "\nDef: " ++ debug (nt d) ++ " @closure->" ++ 
        show 
            (map
                (\p -> E.elemShow p)
                (calcClosures (prods d))) ++
            "\n\n " ++
            (concatMap (\p -> show p ++ "\n\n ") (prods d))

instance TermClass Definition where
    getId d = Nt.getIdent(nt d)

    isTerminal _ = False
    isNonTerminal _ = True

    getTerminal _ = error "\nERROR: getTerminal() called with non Term parameter!\n"
    getNonTerminal d = nt d

    getAttr d = Nt.getAttr (nt d)

    hasBinding d = Nt.hasBinding (nt d)
    getBinding d = Nt.getBinding (nt d)

-- | Construct a non terminal
new :: Id.Ident -> [Attr] -> Code -> [P.Production] -> Definition
new i attrs c ps
    = Def { nt = (Nt.new i B.empty attrs)
          , code = c
          , prods = ps }

--
-- Getters
--
getCode :: Definition -> Code
getCode d = code d

-- | Array of Def nodes we have to hook up with
getClosures :: Definition -> Closure
getClosures d = calcClosures (prods d)

getProds :: Definition -> [P.Production]
getProds d = prods d

setProds :: Definition -> [P.Production] -> Definition
setProds d ps = d { prods = ps }

-- | Get definition for an NonTerm Prod.
getDefForProd :: [Definition] -> P.Production -> Maybe Definition
getDefForProd defs p | isNonTerminal p
    = find (\d ->  (getId d) == (getId p)) (defs)
getDefForProd _ _ = Nothing

-- | Compare a definition with a node.
equalsNode :: Definition -> Node -> Bool
equalsNode (Def { nt = nt1}) n 
    = case getTerm n of
        Just term -> 
            if (isNonTerminal term)
                then (((getId term) == (Nt.getIdent nt1)) &&
                        (attrEqualInOut (Nt.getAttr nt1) (getAttr term)))
                else False
        Nothing -> False

-- | Check if a production is defined
isNodeDefined :: [Definition] -> Node -> Bool
isNodeDefined [] _ = False
isNodeDefined defs n
    = isJust
        (find
            (\d -> (equalsNode d n) || (P.isDefined (prods d) n))
            (defs))

-- | Check all productions which are NonTerm productions
calcClosures :: [P.Production] -> [P.Production]
calcClosures [] = []
calcClosures prods 
    = filter (\p -> isNonTerminal p) (prods)

-- | Merge definitions together
mergeDefs :: [Definition] -> Definition -> Either (Node, Node) [Definition]
mergeDefs [] def = Right [def]
mergeDefs defs def
    = let prods = concatMap (\d -> getProds d) (defs) in
    let clashes =
            find
                (\p -> P.isDefined prods (P.getNode p))
                (getProds def)
        in
    case clashes of
        Nothing -> Right (def:defs)
        Just p -> Left (P.getNode (head (P.getProdsByIdent prods (P.getNode p))), P.getNode p)

-- | Given all definitions and a Node, the return 'type' if the Node
--      is calculated.
getNodeReturnType :: [Definition] -> Node -> Maybe Nt.Nt
getNodeReturnType [] _ = Nothing
getNodeReturnType defs n | isNonTerminal n
    = case (find (\def -> equalsNode def n) (defs)) of
        Just d -> Just (getNonTerminal d)
        otherwise -> Nothing
getNodeReturnType (d:ds) n
    = if (P.isDefined (getProds d) n)
        then Just (getNonTerminal d)
        else getNodeReturnType ds n