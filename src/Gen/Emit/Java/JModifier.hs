-----------------------------------------------------------------------------
-- |
-- Module      :  JModifier
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Java modifiers.
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.Java.JModifier (
		-- * Introduction
		-- $intro
		JModifier(..),
	) where

------------------------------------------------------------------------------------

data JModifier 
	= Private 
	| Public 
	| Protected 
	| NoModifier
	deriving (Eq)

instance Show JModifier where
	show (Protected) = "protected"
	show (Public) = "public"
	show (Private) = "private"
	show (NoModifier) = ""