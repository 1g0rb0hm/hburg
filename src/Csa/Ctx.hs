-----------------------------------------------------------------------------
-- |
-- Module      :  Ctx
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- This module represents a Context. It is stuffed with Elements and helps
-- to discover things like duplicate or missing definitions and bindings.
-----------------------------------------------------------------------------

module Csa.Ctx (
        -- * Types
        Ctx,
        -- * Construction
        new,empty,
        -- * Functions
        insert,member,(!),merge,
    ) where

import qualified Data.Map as M

import Csa.Elem (Elem, ElemClass(..))
-----------------------------------------------------------------------------

-- | A context holds mappings from String's to Elem's. It is used
-- to discover duplicate bindings, missing definitions, etc.
data Ctx
    = Ctx (M.Map String Elem)

instance Show Ctx where
    show (Ctx m)
        = "Context: \n" ++
            concatMap 
                (\key -> 
                    let el = m M.! key in
                    " " ++ show (elemType el) ++ show (elemShow el))
                (M.keys m)

-- | Constructor for creating a singleton Context
new :: Elem -> Ctx
new el = Ctx (M.singleton (elemId el) el)

-- | Constructor for creating an empty Context
empty :: Ctx
empty = Ctx M.empty

-- | Adds Elem to a Context. If Elem already exists it will be overwritten.
insert :: Ctx -> Elem -> Ctx
insert (Ctx m) el = Ctx (M.insert (elemId el) el m)

-- | Checks whether Elem already exists in a Context
member :: Ctx -> Elem -> Bool
member (Ctx m) el = M.member (elemId el) m

-- | Lookup the Elem in the Context
(!) :: Ctx -> Elem -> Maybe Elem
(!) ctx@(Ctx m) el 
    = if (member ctx el)
        then Just (m M.! (elemId el))
        else Nothing

-- | Merges two Contexts. If there are clashes return Left Elem which indicates
--  which element was the culprit. Otherwise return Right Ctx.
merge :: Ctx -> Ctx -> Either (Elem, Elem) (Ctx)
merge (Ctx m1) (Ctx m2) 
    = let intersection = (M.intersection m1 m2) in
    if (M.null intersection)
        then Right (Ctx (M.union m1 m2))
        else 
            let key = head (M.keys intersection) in
            Left (m1 M.! key, m2 M.! key)