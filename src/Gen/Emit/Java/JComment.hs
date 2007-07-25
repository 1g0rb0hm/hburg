-----------------------------------------------------------------------------
-- |
-- Module      :  JComment
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Java comments.
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.Java.JComment (
		-- * Introduction
		-- $intro
		JComment,
		-- *  Construction
		-- $construction
		new,
	) where

import Util (stringFoldr)

------------------------------------------------------------------------------------

data JComment
	= MkJCommment
		[String]	-- Comments

instance Show JComment where
	show (MkJCommment []) = ""
	show (MkJCommment (x:[])) = "// " ++ x
	show (MkJCommment xs)
		= "/**\n * " ++
		(stringFoldr
			(\x y -> x ++ "\n * " ++ y)
			xs) ++ "\n */"


new :: [String] -> JComment
new comments = MkJCommment comments
