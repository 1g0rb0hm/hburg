-----------------------------------------------------------------------------
-- |
-- Module      :  Bind
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Terminals and NonTerminals can be refered to in Semantic Actions by
-- supplying binding identifiers:
--  * Example:  > nt = t t1 (: t1.toString(); :)
--    t1 is associated with the current incarnation of t and can
--    be used within a semantic action in order to refer to t.
-----------------------------------------------------------------------------

module Ast.Bind (
  -- Types
  Binding,
  -- Functions
  new,empty,
  hasBinding, getIdent,
) where

{- unqualified imports  -}

{- qualified imports  -}
import qualified Ast.Ident as Id (Ident)

------------------------------------------------------------------------------------

{- | Bindings for Terminals and NonTerminals -}
data Binding =
  MkBind Id.Ident
  | MkEmptyBind
  deriving (Eq)

instance Show Binding where
  show (MkBind i) = show i
  show (MkEmptyBind) = ""

new :: Id.Ident -> Binding
new i = MkBind i

empty :: Binding
empty = MkEmptyBind

hasBinding :: Binding -> Bool
hasBinding MkEmptyBind = False
hasBinding _ = True

getIdent :: Binding -> Id.Ident
getIdent (MkBind i) = i
getIdent MkEmptyBind = error "\nERROR: getBinding() called on empty Binding"

------------------------------------------------------------------------------------