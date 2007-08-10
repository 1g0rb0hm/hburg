-----------------------------------------------------------------------------
-- |
-- Module      :  TermTy
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- This module provides the necessary abstractions to deal with terminals
-- and non terminals in a uniform and generic way. Thus a TermTy can either
-- be a terminal of type T or a non terminal of type Nt.
-----------------------------------------------------------------------------

module Ast.TermTy (
        -- * Classes
        TermTyClass(..),
        -- * Types
        TermTy,
        -- * Construction
        term, nonTerm,
    ) where

import Debug(Debug(..))

import qualified Ast.Ident as Id (Ident)
import Ast.Bind (Binding)
import Ast.Attr (Attr)
import qualified Ast.T as T (T, getIdent, getBinding, hasBinding)
import qualified Ast.Nt as Nt (Nt, getIdent, getAttr, getBinding, hasBinding)

import Env.Env(ElemClass(..))
-----------------------------------------------------------------------------

-- | All terminals and non terminals are instances of this class
class TermTyClass a where
    getId :: a -> Id.Ident

    isTerm :: a -> Bool
    isNonTerm :: a -> Bool

    getTerm :: a -> T.T
    getNonTerm :: a -> Nt.Nt

    hasAttr :: a -> Bool
    hasAttr a =
        case getAttr a of
            [] -> False
            otherwise -> True
    getAttr :: a -> [Attr]

    hasBinding :: a -> Bool
    getBinding :: a -> Binding

    equalBindings :: a -> a -> Bool
    equalBindings a1 a2
        = if (hasBinding a1 && hasBinding a2)
            then (getBinding a1 == getBinding a2)
            else False

-- | TermTy Type
data TermTy
    = Term T.T
    | NonTerm Nt.Nt

instance Eq TermTy where
    (==) (Term t1) (Term t2) = t1 == t2
    (==) (NonTerm nt1) (NonTerm nt2) = nt1 == nt2
    (==) _ _ = False

instance Ord TermTy where
    compare (Term t1) (Term t2) = compare t1 t2
    compare (NonTerm nt1) (NonTerm nt2) = compare nt1 nt2
    compare (Term _) (NonTerm _) = LT
    compare (NonTerm _) (Term _) = GT

instance ElemClass TermTy where
    elemShow (Term t) = elemShow t
    elemShow (NonTerm nt) = elemShow nt
    elemType (Term t) = elemType t
    elemType (NonTerm nt) = elemType nt
    elemC (Term t) = elemC t
    elemC (NonTerm nt) = elemC nt
    elemL (Term t) = elemL t
    elemL (NonTerm nt) = elemL nt

instance Show TermTy where
    show (Term t) = show t
    show (NonTerm nt) = show nt

instance Debug TermTy where
    debug (Term t) = debug t
    debug (NonTerm nt) = debug nt

instance TermTyClass TermTy where
    getId (Term t) = T.getIdent t
    getId (NonTerm nt) = Nt.getIdent nt
    
    isTerm (Term _) = True
    isTerm _ = False

    isNonTerm (NonTerm _) = True
    isNonTerm _ = False
        
    getTerm (Term t) = t
    getTerm _ = error "\nERROR: getTerm() called with NonTerm parameter!\n"
    
    getNonTerm (NonTerm t) = t
    getNonTerm _ = error "\nERROR: getNonTerm() called with Term parameter!\n"

    getAttr (Term _) = []
    getAttr (NonTerm nt) = Nt.getAttr nt
    
    hasBinding (Term t) = T.hasBinding t
    hasBinding (NonTerm nt) = Nt.hasBinding nt

    getBinding (Term t) = T.getBinding t
    getBinding (NonTerm nt) = Nt.getBinding nt

-- | Construct TermTy out of T
term :: T.T -> TermTy
term t = Term t

-- | Construct TermTy out o f Nt
nonTerm :: Nt.Nt -> TermTy
nonTerm nt = NonTerm nt
