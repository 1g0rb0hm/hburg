-----------------------------------------------------------------------------
-- |
-- Module      :  T
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Representation of a terminal in our tree pattern matching grammar.
-----------------------------------------------------------------------------

module Ast.T (
        -- * Types
        T,
        -- * Construction
        new,
        -- * Functions
        getIdent,
        getBinding,
        hasBinding,
    ) where

import Debug (Debug(..))

import qualified Ast.Ident as Id (Ident)
import qualified Ast.Bind as B (Binding, hasBinding)

import Env.Env(ElemClass(..),ElemType(ETerm))

------------------------------------------------------------------------------------

-- | Terminal Definition
data T
    = T 
        Id.Ident    -- ^ Id identifying this terminal (e.g. ADD, SUB, etc.)
        B.Binding   -- ^ binding for this terminsl (e.g. ADD a1)

instance Show T where
    show (T i b) 
        = "T[" ++ show i ++ (if (B.hasBinding b) then "->" else "") ++ show b ++ "]"

instance Debug T where
    debug (T i b)
        = "T[" ++ show i ++ (if (B.hasBinding b) then "->" else "") ++ show b ++ "]"

instance Eq T where
    (==) (T i1 _) (T i2 _) = i1 == i2

instance Ord T where
    compare (T i1 _) (T i2 _) = compare i1 i2

instance ElemClass T where
    elemShow (T i _) = show i
    elemType _ = ETerm
    elemL (T i _) = elemL i
    elemC (T i _) = elemC i

-- | Constructor for creating a T
new :: Id.Ident -> B.Binding -> T
new i b = T i b

getIdent :: T -> Id.Ident
getIdent (T i _) = i

hasBinding :: T -> Bool
hasBinding (T _ bind) = B.hasBinding bind

getBinding :: T -> B.Binding
getBinding (T _ bind) = bind