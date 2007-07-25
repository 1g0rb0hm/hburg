-----------------------------------------------------------------------------
-- |
-- Module      :  JEnum
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Java enumeration.
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.Java.JEnum (
		-- * Introduction
		-- $intro
		JEnum,
		-- *  Construction
		-- $construction
		new,
	) where

import Util (stringFoldr)

import Gen.Emit.Java.JModifier(JModifier)

------------------------------------------------------------------------------------

data JEnum 
	= MkEnum
		JModifier
		String			-- enumeration name
		[String]		-- enum elements
	deriving (Eq)

instance Show JEnum where
	show (MkEnum modifier ident enums)
		= " " ++ (show modifier) ++ " enum " ++	-- Modifier
		ident ++ " {\n" ++					-- Identifier
		(stringFoldr
			(\x y -> "\t" ++ x ++ ",\n" ++ y)
			enums) ++ "};\n"					-- Enumeration

new :: JModifier -> String -> [String] -> JEnum
new m ident elems = MkEnum m ident elems