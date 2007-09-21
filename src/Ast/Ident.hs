-----------------------------------------------------------------------------
-- |
-- Module      :  Ident
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- User defined identifiers of all kinds are represented by this module.
-- Examples of identifiers:
--        * bindings
--        * terminal and non terminal names
-----------------------------------------------------------------------------

module Ast.Ident (
        -- * Types
        Ident,
        -- * Construction
        new,
        -- * Functions
        toIdent,
    ) where

import Parser.Lexer (Token(..))

import qualified Csa.Elem as E (ElemClass(..), ElemType(EIdent))
------------------------------------------------------------------------------------

-- | A simple Identifier.
data Ident 
    = MkTokId Token     -- ^ token as the lexer produces them can be identifiers
    | MkIdent String    -- ^ regular strings are identifiers as well

instance E.ElemClass Ident where
    elemShow (MkTokId (ConToken _ _ str)) = str
    elemShow (MkIdent str) = str
    elemType _ = E.EIdent
    elemL (MkTokId t@(ConToken _ _ _)) = E.elemL t
    elemL _ = -1
    elemC (MkTokId t@(ConToken _ _ _)) = E.elemC t
    elemC _ = -1

instance Eq Ident where
    (==) (MkTokId (ConToken _ _ i1)) (MkTokId (ConToken _ _ i2)) = i1 == i2
    (==) (MkTokId (ConToken _ _ i1)) (MkIdent i2) = i1 == i2
    (==) (MkIdent i1) (MkTokId (ConToken _ _ i2)) = i1 == i2
    (==) (MkIdent i1) (MkIdent i2) = i1 == i2

instance Ord Ident where
    compare (MkTokId (ConToken _ _ i1)) (MkTokId (ConToken _ _ i2)) = compare i1 i2
    compare (MkTokId (ConToken _ _ i1)) (MkIdent i2) = compare i1 i2
    compare (MkIdent i1) (MkTokId (ConToken _ _ i2)) = compare i1 i2
    compare (MkIdent i1) (MkIdent i2) = compare i1 i2

instance Show Ident where
    show (MkTokId (ConToken _ _ i)) = i
    show (MkIdent i) = i

new :: String -> Ident
new i = MkIdent i

toIdent :: Token -> Ident
toIdent t = MkTokId t
