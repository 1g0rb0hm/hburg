-----------------------------------------------------------------------------
-- |
-- Module      :  JConstructor
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Java constructor.
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.Java.JConstructor (
		-- * Introduction
		-- $intro
		JConstructor,
        -- *  Construction
        -- $construction
		new,
	) where

import Util (stringFoldr)

import Gen.Emit.Java.JModifier (JModifier)
import qualified Gen.Emit.Java.JComment as Comment (JComment, new)

------------------------------------------------------------------------------------

type Parameter = String
type Body = String

data JConstructor 
	= MkConstructor
		Comment.JComment	-- comment
		JModifier			-- private|public etc
		String				-- constructor Identifier
		[Parameter]
		Body

instance Eq JConstructor where
	(==) (MkConstructor _ _ i1 params1 _) (MkConstructor _ _ i2 params2 _) 
		= ((i2 == i2) && (params1 == params2))


instance Show JConstructor where
	show (MkConstructor comments m ident params body)
		= 
		-- Comments
		show comments ++ "\n" ++
		-- Constructor
		show m ++ " " ++				-- public|private|...
		ident ++ " (" ++				-- method identifier
		(stringFoldr					-- method body
			(\x y -> x ++ ", " ++ y)
			params) ++ ") {\n" ++
		body ++ "\n} // END CONSTRUCTOR " ++ ident ++ "()"


new :: JModifier -> String -> [Parameter] -> Body -> JConstructor
new m str params body
	= MkConstructor (Comment.new []) m str params body
