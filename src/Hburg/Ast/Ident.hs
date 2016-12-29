-----------------------------------------------------------------------------
-- |
-- Module      :  Ident
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- User defined identifiers of all kinds are represented by this module.
-- Examples of identifiers:
--        * bindings
--        * terminal and non terminal names
-----------------------------------------------------------------------------

module Hburg.Ast.Ident (
  -- Types
  Ident,
  -- Functions
  new,
  toIdent,
) where

{- unqualified imports  -}
import Hburg.Parse.Lexer (Token(..))

{- qualified imports  -}
import qualified Hburg.Csa.Elem as E (ElemClass(..), ElemType(EIdent))

------------------------------------------------------------------------------------

{- | An Identifier. -}
data Ident =
  MkTokId Token     -- ^ token as the lexer produces them can be identifiers
  | MkIdent String  -- ^ regular strings are identifiers as well

instance E.ElemClass Ident where
  elemShow (MkTokId (MkToken _ tok)) = show tok
  elemShow (MkIdent str) = str
  elemType _ = E.EIdent
  elemL (MkTokId t) = E.elemL t
  elemL _ = -1
  elemC (MkTokId t) = E.elemC t
  elemC _ = -1

instance Eq Ident where
  (==) (MkTokId (MkToken _ t1)) (MkTokId (MkToken _ t2)) = t1 == t2
  (==) (MkTokId (MkToken _ t)) (MkIdent i) = show t == i
  (==) (MkIdent i) (MkTokId (MkToken _ t)) = i == show t
  (==) (MkIdent i1) (MkIdent i2) = i1 == i2

instance Ord Ident where
  compare (MkTokId (MkToken _ t1)) (MkTokId (MkToken _ t2)) = compare t1 t2
  compare (MkTokId (MkToken _ t)) (MkIdent i) = compare (show t) i
  compare (MkIdent i) (MkTokId (MkToken _ t)) = compare i (show t)
  compare (MkIdent i1) (MkIdent i2) = compare i1 i2

instance Show Ident where
  show (MkTokId (MkToken _ t)) = show t
  show (MkIdent i) = i

new :: String -> Ident
new i = MkIdent i

toIdent :: Token -> Ident
toIdent t = MkTokId t

------------------------------------------------------------------------------------