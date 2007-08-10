-----------------------------------------------------------------------------
-- |
-- Module      :  Env
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Env is used heavily during CSA. It is stuffed with Elements and helps
-- to discover things like duplicate or missing definitions and bindings.
-----------------------------------------------------------------------------

module Env.Env (
        -- * Classes
        ElemClass(..), ElemType(..),
        -- * Types
        Env, Elem,
        -- * Construction
        newEnv,emptyEnv,
        -- * Functions
        addToEnv,inEnv,getElem,envElem,mergeEnvs,
    ) where

import qualified Data.Map as M

import Env.Elem (ElemClass(..), Elem, ElemType(..), envElem)
-----------------------------------------------------------------------------

-- | An Env holds all operator mappings thus we can
--  type check if all the operators we used (a.k.a. Terminals)
--  have been defined and thus are valid. The Environment is
--  implemented by using Data.Map.
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

-- | Constructor for creating an Env with one element
newEnv :: Elem -> Env
newEnv el = Env (M.singleton (elemId el) el)

-- | Constructor for creating an empty Env
emptyEnv :: Env
emptyEnv = Env M.empty

-- | Adds Elem to an environment. If Elem already exists it will be overwritten.
addToEnv :: Env -> Elem -> Env
addToEnv (Env m) el = Env (M.insert (elemId el) el m)

-- | Checks whether Elem already exists in Env
inEnv :: Env -> Elem -> Bool
inEnv (Env m) el = M.member (elemId el) m

-- | Checks whether Elem already exists in Env and if it does it returns it.
getElem :: Env -> Elem -> Maybe Elem
getElem env@(Env m) el 
    = if (inEnv env el)
        then Just (m M.! (elemId el))
        else Nothing

-- | Merges two Envs. If there are clashes return Left Elem which indicates
--  which element was the culprit. Otherwise return Right Env.
mergeEnvs :: Env -> Env -> Either (Elem, Elem) (Env)
mergeEnvs (Env m1) (Env m2) 
    = let intersection = (M.intersection m1 m2) in
    if (M.null intersection)
        then Right (Env (M.union m1 m2))
        else 
            let key = head (M.keys intersection) in
            Left (m1 M.! key, m2 M.! key)