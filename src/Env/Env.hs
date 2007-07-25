-----------------------------------------------------------------------------
-- |
-- Module      :  Env
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Env is used heavily during CSA. It is stuffed with Elements and helps
-- to discover things like duplicate or missing definitions and bindings.
--
-----------------------------------------------------------------------------

module Env.Env (
		-- * Introduction
		-- $intro
		Env, Elem,
		ElemClass(..), ElemType(..),
		-- *  Construction
        -- $construction
		newEnv,emptyEnv,
		-- *  Operations on environments
        -- $environment operations
		addToEnv,inEnv,getElem,envElem,mergeEnvs,
	) where

import qualified Data.Map as M

import Env.Elem(ElemClass(..), Elem, ElemType(..), envElem)

------------------------------------------------------------------------------------

-- | An Env holds all operator mappings thus we can
--		type check if all the operators we used (a.k.a. Terminals)
--		have been defined and thus are valid. The Environment is
--		implemented by using Data.Map.
data Env
	= Env (M.Map String Elem)

instance Show Env where
	show (Env m) 
		= "Env: \n" ++ 
			concatMap 
				(\key -> 
					let el = m M.! key in
					" " ++ show (elemType el) ++ show (elemShow el))
				(M.keys m)

-- | Create ADT Env
newEnv :: Elem -> Env
newEnv el = Env (M.singleton (elemId el) el)

-- | Create empty ADT Env
emptyEnv :: Env
emptyEnv = Env M.empty

-- | Add Elem to an environment. If an Elem
--		already exists it will be overwritten.
addToEnv :: Env -> Elem -> Env
addToEnv (Env m) el = Env (M.insert (elemId el) el m)

-- | Checks whether an Elem already exists in an Env
inEnv :: Env -> Elem -> Bool
inEnv (Env m) el = M.member (elemId el) m

-- | Checks whether an Elem already exists in an Env
--	and if it does it returns it.
getElem :: Env -> Elem -> Maybe Elem
getElem env@(Env m) el 
	= if (inEnv env el)
		then Just (m M.! (elemId el))
		else Nothing

-- | Merge two Envs. If there are clashes return Left Elem
--	which indicates which element was the culprit. Otherwise return 
--	Right Env.
mergeEnvs :: Env -> Env -> Either (Elem, Elem) (Env)
mergeEnvs (Env m1) (Env m2) 
	= let intersection = (M.intersection m1 m2) in
	if (M.null intersection)
		then Right (Env (M.union m1 m2))
		else 
			let key = head (M.keys intersection) in
			Left (m1 M.! key, m2 M.! key)
