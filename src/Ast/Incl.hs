-----------------------------------------------------------------------------
-- |
-- Module      :  Incl
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Target code specified in the include section of the tree pattern
-- matching language.
-- 
--
-----------------------------------------------------------------------------

module Ast.Incl (
		-- * Introduction
		-- $intro
		Include,
        -- *  Construction
        -- $construction
		new,
	) where

import Ast.Code(Code)

------------------------------------------------------------------------------------

data Include = Incl Code

new :: Code -> Include
new c = Incl c

instance Show Include where
	show (Incl c) = show c