-----------------------------------------------------------------------------
-- |
-- Module      :  Csa
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- This module contains various semantic analysis functions used
-- during the process of parsing the tree pattern matching language
-- specification.
--
-----------------------------------------------------------------------------

module Csa.Csa (
		-- * Operations
		updateEnv,
		checkEnv,
		checkDef,
	) where

import Maybe (fromJust, isJust)

import List (find)

import qualified Ast.Bind as B (getBinding)
import Ast.TermTy (TermTy, TermTyClass(..))
import Ast.Node (NodeClass(..), getLink, showAsFunction, mapPreOrder)
import Ast.Prod (getNode)
import Ast.Def (Definition, getProds, isNodeDefined)

import Env.Env(ElemClass(..), Elem, Env,
			mergeEnvs, newEnv, envElem, inEnv)

import Parser.ParseErr (parseErrElem)

------------------------------------------------------------------------------------

-- @TODO: We need a method which checks if all patterns have the
--	the correct type. We are only checking for the correct amount
--	of parameters but not if they produce what is expected in the
--	context in which they are used. The typeCheck function signature
--	should look smth. like the following:
-- typeCheck :: [Definition] -> Maybe [String]


-- | If Binding for TermTy is not in the Env return 'Right' with
--	updated Env, otherwise return 'Left' indicating the two culprits
updateEnv :: TermTy -> Env -> Either (Elem, Elem) Env
updateEnv ty env
	= if (hasBinding ty)
		then mergeEnvs env (newEnv (envElem (B.getBinding (getBinding ty))))
		else Right env


-- | Checks if a definition contains the following:
--		nt = nt1
--			| nt2
--			| nt <-- This is the error case since a definition is being defined in terms of itself
--			| nt3.
checkDef :: Definition -> Maybe String
checkDef d
	= let clash 
			= find
				(\p -> ((getId p) == (getId d))) 
				(filter (\p -> isNonTerm p) (getProds d)) 
		in
	case clash of
		Nothing -> Nothing
		Just p -> 
			Just (parseErrElem (envElem (getNode p)) 
				("'" ++ elemShow (envElem (getNode p)) ++ "' can not be defined in terms of itself."))


-- | checkEnv. Checks if all nodes have been defined. Returns 'Nothing'
--		if checks succeed, or a 'Just [String]' where '[String]' contains the
--		the error messages.
checkEnv :: [Definition] -> Env -> Maybe [String]
checkEnv [] env = Nothing
checkEnv defs env
	= -- Retrieve all production nodes
	let pnodes = concatMap (\x -> (map (\y -> getNode y) (getProds x))) (defs) in
	-- Go over each production node in preorder and collect CSA results
	let results =
		concatMap
			(\p ->
				mapPreOrder
					-- Apply the following to each node
					(\n -> -- 1. Check if node is in the Environment
						if (inEnv env (envElem n))
							then -- 2. Check if node is defined
								if (isNodeDefined defs n)
									then -- 3. Check link node if one is defined
										if (isNil (getLink n))
											then Nothing
											else if (inEnv env (envElem (getLink n)))
													then Nothing
													else -- Whoops! Check of link failed
														Just (parseErrElem (envElem (getLink n)) (show (elemType (getLink n)) ++ 
															" '" ++ elemShow (getLink n) ++ "' is undefined."))
									else -- Whoops! Check failed
										Just (parseErrElem (envElem n) (show (elemType n) ++ " '" ++ 
											showAsFunction n ++ "' is undefined."))
							else -- Whoops! Check failed
								Just (parseErrElem (envElem n) (show (elemType n) ++ " '" ++ 
									elemShow n ++ "' is undefined.")))
					(p))
				(pnodes)
		in
	-- Evaluate CSA results
	let errors = map
					(\x -> fromJust x)
					-- filter out only the 'Just' results
					(filter
						(\r -> isJust r)
						(results))
		in
	-- In case there are errors return them
	case errors of
		[] -> Nothing
		otherwise -> Just (reverse errors)
