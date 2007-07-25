-----------------------------------------------------------------------------
-- |
-- Module      :  Code
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- The target code within a semantic action is represented by this module:
--		* e.g. (: .... :)
--			the part between '(:' and ':)' is code.
--
-----------------------------------------------------------------------------

module Ast.Code (
		-- * Introduction
		-- $intro
		Code,
        -- *  Construction
        -- $construction
		new, empty,
        -- *  Operations on attributes
        -- $attribute operations
		isEmpty,
	) where

import Debug (Debug(..))

------------------------------------------------------------------------------------

-- | Code data type
data Code 
	= MkEmptyCode
	| MkCode String		-- This is usually what is written between (: :)
	deriving (Eq,Ord)

instance Show Code where
	show (MkEmptyCode)		= ""
	show (MkCode str)		= str

instance Debug Code where
	debug (MkEmptyCode)		= "Nil"
	debug (MkCode str)		= "Code: " ++ str

new :: String -> Code
new str = MkCode str

empty :: Code
empty = MkEmptyCode

isEmpty :: Code -> Bool
isEmpty (MkEmptyCode) = True
isEmpty _ = False
