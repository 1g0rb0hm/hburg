-----------------------------------------------------------------------------
-- |
-- Module      :  Prod
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Each definition consists of one or more productions. This module provides
-- the necessary abstractions to deal with productions:
--        * e.g.:  def = prod1
--                        | prod2
--                        | ...
--                        | prodN
-----------------------------------------------------------------------------

module Ast.Prod (
        -- * Types
        Production,
        -- * Construction
        prod,
        -- * Functions
        getProdsByIdent,getArity,getNode,getCost,
        getRuleLabel,getResultLabel,
        setRuleLabel,setResultLabel,
        toOp,
        isDefined,
    ) where

import Ast.Op (Operator, op)
import Ast.Term (TermClass(..))
import Ast.Cost (Cost)
import qualified Ast.Node as N (Node, TreeClass(getChildren))

import qualified Csa.Elem as E (ElemClass(..), ElemType(EProd))
-----------------------------------------------------------------------------

-- | Rule label for this production (e.g. R_REG_ASSIGN_0) - assigned during code generation phase
type RuleLabel = String

-- | Result Label for this production (e.g. NT_STMT) - assigned during code generation phase
type ResultLabel = String

-- | Production type
data Production
    = Prod  {   pattern :: N.Node       -- ^ tree pattern for this production
            ,   cost    :: Cost         -- ^ cost of this production
            ,   rule    :: RuleLabel    -- ^ rule label
            ,   result  :: ResultLabel} -- ^ result label
    deriving (Ord)

instance Eq Production where
    (==) p1 p2 = ((pattern p1 == pattern p2) && (cost p1 == cost p1))

instance Show Production where
    show p = "Prod {Cost: "++ show (cost p) ++"}:\n  "++ show (pattern p)

instance TermClass Production where
    getId p = getId (pattern p)

    isTerminal p = isTerminal (pattern p)
    isNonTerminal p = isNonTerminal (pattern p)

    getTerminal p = getTerminal (pattern p)
    getNonTerminal p = getNonTerminal (pattern p)

    getAttr p = getAttr (pattern p)

    hasBinding p = hasBinding (pattern p)
    getBinding p = getBinding (pattern p)

instance E.ElemClass Production where
    elemShow p = E.elemShow $ pattern p
    elemType p = E.EProd
    elemL p = E.elemL $ pattern p
    elemC p = E.elemC $ pattern p

-- | Constructor for building a production
prod :: N.Node -> Cost -> Production
prod n c = Prod {pattern = n,
                 cost = c,
                 rule = "",
                 result = ""}

getNode :: Production -> N.Node
getNode p = pattern p

toOp :: Production -> Operator
toOp p | isTerminal p = op (getId p)
toOp _ = error "\nERROR: toOp() called with NonTerm as argument!\ns"

getCost :: Production -> Cost
getCost p = cost p

getRuleLabel :: Production -> RuleLabel
getRuleLabel p = rule p

setRuleLabel :: Production -> RuleLabel -> Production
setRuleLabel p rl = p { rule = rl }

setResultLabel :: Production -> ResultLabel -> Production
setResultLabel p rl = p { result = rl }

getResultLabel :: Production -> ResultLabel
getResultLabel p = result p

getArity :: Production -> Int
getArity p = length (N.getChildren (pattern p))

isDefined :: [Production] -> N.Node -> Bool
isDefined [] _ = False
isDefined prods n =  n `elem` (map (\p -> pattern p) prods)

-- | Retrieves all productions which have the same identifier
getProdsByIdent :: [Production] -> N.Node -> [Production]
getProdsByIdent [] _ = []
getProdsByIdent prods n = filter (\p -> getId p == getId n) (prods)