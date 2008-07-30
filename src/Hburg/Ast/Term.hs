-----------------------------------------------------------------------------
-- |
-- Module      :  Term
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module provides the necessary abstractions to deal with terminals
-- and non-terminals in a uniform and generic way.
-----------------------------------------------------------------------------

module Hburg.Ast.Term (
  -- * Types
  TermClass(..),
  Term,
  -- * Functions
  terminal, nonTerminal,
) where

{- unqualified imports -}
import Hburg.Ast.Bind (Binding)
import Hburg.Ast.Attr (Attr)

{- qualified imports -}
import qualified Hburg.Ast.Ident as Id (Ident)
import qualified Hburg.Ast.T as T (T, getIdent, getBinding, hasBinding)
import qualified Hburg.Ast.Nt as Nt (Nt, getIdent, getAttr, getBinding, hasBinding)

import qualified Hburg.Csa.Elem as E (ElemClass(..))

-----------------------------------------------------------------------------

{- | All terminals and non terminals are instances of this class -}
class TermClass a where
  getId :: a -> Id.Ident
  
  isTerminal :: a -> Bool
  isNonTerminal :: a -> Bool
  
  getTerminal :: a -> T.T
  getNonTerminal :: a -> Nt.Nt
  
  hasAttr :: a -> Bool
  hasAttr a = not $ null $ getAttr a
  getAttr :: a -> [Attr]
  
  hasBinding :: a -> Bool
  getBinding :: a -> Binding
  
  equalBindings :: a -> a -> Bool
  equalBindings a1 a2 =
    if (hasBinding a1 && hasBinding a2)
      then (getBinding a1 == getBinding a2)
      else False

{- | Term data type -}
data Term =
  Terminal T.T  -- ^ terminal T is a Term
  | NonTerm Nt.Nt -- ^ non terminal Nt is a Term

instance Eq Term where
  (==) (Terminal t1) (Terminal t2) = t1 == t2
  (==) (NonTerm nt1) (NonTerm nt2) = nt1 == nt2
  (==) _ _ = False

instance Ord Term where
  compare (Terminal t1) (Terminal t2) = compare t1 t2
  compare (NonTerm nt1) (NonTerm nt2) = compare nt1 nt2
  compare (Terminal _) (NonTerm _) = LT
  compare (NonTerm _) (Terminal _) = GT

instance E.ElemClass Term where
  elemShow (Terminal t) = E.elemShow t
  elemShow (NonTerm nt) = E.elemShow nt
  elemType (Terminal t) = E.elemType t
  elemType (NonTerm nt) = E.elemType nt
  elemC (Terminal t) = E.elemC t
  elemC (NonTerm nt) = E.elemC nt
  elemL (Terminal t) = E.elemL t
  elemL (NonTerm nt) = E.elemL nt

instance Show Term where
  show (Terminal t) = show t
  show (NonTerm nt) = show nt

instance TermClass Term where
  getId (Terminal t) = T.getIdent t
  getId (NonTerm nt) = Nt.getIdent nt

  isTerminal (Terminal _) = True
  isTerminal _ = False

  isNonTerminal (NonTerm _) = True
  isNonTerminal _ = False

  getTerminal (Terminal t) = t
  getTerminal _ = error "\nERROR: getTerminal() called with NonTerm parameter!\n"

  getNonTerminal (NonTerm t) = t
  getNonTerminal _ = error "\nERROR: getNonTerminal() called with Term parameter!\n"

  getAttr (Terminal _) = []
  getAttr (NonTerm nt) = Nt.getAttr nt

  hasBinding (Terminal t) = T.hasBinding t
  hasBinding (NonTerm nt) = Nt.hasBinding nt

  getBinding (Terminal t) = T.getBinding t
  getBinding (NonTerm nt) = Nt.getBinding nt

{- | Construct Term out of T -}
terminal :: T.T -> Term
terminal t = Terminal t

{- | Construct Term out o f Nt -}
nonTerminal :: Nt.Nt -> Term
nonTerminal nt = NonTerm nt

-----------------------------------------------------------------------------