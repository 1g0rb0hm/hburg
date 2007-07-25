-----------------------------------------------------------------------------
-- |
-- Module      :  Bind
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Terminals and NonTerminals can be refered to in Semantic Actions by
-- supplying binding identifiers:
--		* e.g.:	nt = t t1 (: t1.toString(); :)
--			t1 is associated with the current incarnation of t and can
--			be used within a semantic action in order to refer to t.
--
-----------------------------------------------------------------------------

module Ast.Bind (
		-- * Introduction
		-- $intro
		Binding,
        -- *  Construction
        -- $construction
		new,empty,
        -- *  Operations on attributes
        -- $attribute operations
		hasBinding,getBinding,
	) where

import qualified Ast.Ident as Id (Ident)

------------------------------------------------------------------------------------

-- | Bindings for Terminals and NonTerminals
data Binding 
	= MkBind Id.Ident
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
hasBinding b 
	= case b of 
		MkEmptyBind -> False
		otherwise -> True

getBinding :: Binding -> Id.Ident
getBinding (MkBind i) = i
getBinding MkEmptyBind = error "\nERROR: getBinding() called on empty Binding"