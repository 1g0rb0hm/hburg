-----------------------------------------------------------------------------
-- |
-- Module      :  JVariable
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- @TODO: Write short summary
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.Java.JVariable (
		-- * Introduction
		-- $intro
		JVariable,
		-- *  Construction
		-- $construction
		new,
	) where

import Gen.Emit.Java.JModifier (JModifier)

------------------------------------------------------------------------------------

type Type = String
type Constructor = String

data JVariable 
	= MkJVariable
		JModifier 	-- private|public etc.
		Bool		-- is it static
		Type		-- Variable type
		String		-- Identifier
		Constructor -- how to construct the variable (e.g. new EnumSet.of(blablabla))
	deriving (Eq)

instance Show JVariable where
	show (MkJVariable modifier isStat ty ident constructor) 
		= " " ++ (show modifier) ++ " " ++			-- Modifier
		(if (isStat) then "static " else " ") ++ 	-- is it static ?
		ty ++ " " ++								-- Type
		ident ++ 
		if (constructor /= [])
			then " = " ++							-- Identifier
				constructor ++ ";"					-- how to construct it?
			else ";"

new :: JModifier -> Bool -> Type -> String -> Constructor -> JVariable
new m stat ty i con = MkJVariable m stat ty i con
