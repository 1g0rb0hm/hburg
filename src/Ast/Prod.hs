-----------------------------------------------------------------------------
-- |
-- Module      :  Prod
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Each definition consists of one or more productions. This module provides
-- the necessary abstractions to deal with productions:
--		* e.g.:  def = prod1
--						| prod2
--						| ...
--						| prodN
--
-----------------------------------------------------------------------------

module Ast.Prod (
		-- * Introduction
		-- $intro
		Cost,Prod,
        -- *  Construction
        -- $construction
		prod,
        -- *  Operations on attributes
        -- $attribute operations
		getName,getProdsByIdent,getArity,getNode,getCost,
		getRuleLabel,getResultLabel,
		setRuleLabel,setResultLabel,
		toOp,
		isDefined,
		mergeProds,
	) where

import List (find)

import Debug (Debug(..))

import Ast.Op (Operator, op)
import Ast.TermTy (TermTyClass(..))
import qualified Ast.Node as N (Node, NodeClass(getChildren), equalIdents, getName)

------------------------------------------------------------------------------------

-- | Production Cost
type Cost = Int

-- | Rule Label for this production (R_REG_ASSIGN_0) - assigned during code generation phase
type RuleLabel = String

-- | Result Label for this production (e.g. NT_STMT) - assigned during code generation phase
type ResultLabel = String

-- | ADT production
data Prod
	= Prod {node		:: N.Node,		-- the root node for this production
			cost		:: Cost,        -- the cost of this production
			rulelab		:: RuleLabel,   -- Rule label
			resultlab	:: ResultLabel  -- Result label
	}

instance Eq Prod where
	(==) p1 p2 = ((node p1 == node p2) && (cost p1 == cost p1))

instance Show Prod where
	show p = "Prod[" ++ show (cost p) ++ "]:\n  " ++ show (node p)

instance Debug Prod where
	debug p = "Prod[" ++ show (cost p) ++ "]:\n  " ++ show (node p)

instance TermTyClass Prod where
	getId p = getId (node p)

	isTerm p = isTerm (node p)
	isNonTerm p = isNonTerm (node p)

	getTerm p = getTerm (node p)
	getNonTerm p = getNonTerm (node p)

	getAttr p = getAttr (node p)

	hasBinding p = hasBinding (node p)
	getBinding p = getBinding (node p)

prod :: N.Node -> Cost -> Prod
prod n c = Prod {node = n,
 				cost = c,
 				rulelab = "",
 				resultlab = ""
			}

getNode :: Prod -> N.Node
getNode p = node p

getName :: Prod -> String
getName p = N.getName (getNode p)

toOp :: Prod -> Operator
toOp p | isTerm p = op (getId p)
toOp _ = error "\nERROR: toOp() called with NonTerm as argument!\ns"

getCost :: Prod -> Cost
getCost p = cost p

getRuleLabel :: Prod -> RuleLabel
getRuleLabel p = rulelab p

setRuleLabel :: Prod -> RuleLabel -> Prod
setRuleLabel p rl = p { rulelab = rl }

setResultLabel :: Prod -> ResultLabel -> Prod
setResultLabel p rl = p { resultlab = rl }

getResultLabel :: Prod -> ResultLabel
getResultLabel p = resultlab p

getArity :: Prod -> Int
getArity p = length (N.getChildren (node p))

isDefined :: [Prod] -> N.Node -> Bool
isDefined [] _ = False
isDefined prods n
	=  n `elem` (map (\p -> node p) prods)

-- | Retrieve all productions which have the same identifier
getProdsByIdent :: [Prod] -> N.Node -> [Prod]
getProdsByIdent [] _ = []
getProdsByIdent prods n
	= filter 
		(\p -> N.equalIdents (node p) n) 
		(prods)

-- | mergeProds. Merges productions iff productions with the same name,
--		have the same amount of parameters.
mergeProds :: [Prod] -> Prod -> Either (N.Node, N.Node) [Prod]
mergeProds [] _ = Right []
mergeProds prods p@(Prod {node = new})
	= let errprod = find
						(\(Prod {node = n}) -> n /= new) 
						(getProdsByIdent prods new)
		in
	case errprod of
		Nothing -> Right (p:prods)
		Just err -> Left (new, getNode err)

