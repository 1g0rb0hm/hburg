-----------------------------------------------------------------------------
-- |
-- Module      :  Decl
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Target code specified in the declaration section of the tree pattern
-- matching language.
-- 
--
-----------------------------------------------------------------------------

module Ast.Decl (
		-- * Introduction
		-- $intro
		Declaration,
        -- *  Construction
        -- $construction
		new,
	) where

import Ast.Code(Code)

------------------------------------------------------------------------------------

data Declaration = Decl Code

instance Show Declaration where
	show (Decl c) = show c

new :: Code -> Declaration
new c = Decl c