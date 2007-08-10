-----------------------------------------------------------------------------
-- |
-- Module      :  JComment
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java comments.
-----------------------------------------------------------------------------

module Gen.Emit.Java.JComment (
        -- * Types
        JComment,
        -- * Construction
        new,
    ) where

import Util (stringFoldr)
------------------------------------------------------------------------------------

data JComment
    = MkJCommment
        [String]    -- Comments

instance Show JComment where
    show (MkJCommment []) = ""
    show (MkJCommment (x:[])) = "// " ++ x
    show (MkJCommment xs)
        = "/**\n * " ++
        (stringFoldr
            (\x y -> x ++ "\n * " ++ y)
            xs) ++ "\n */"

-- | Constructor for building a comment
new :: [String] -> JComment
new comments = MkJCommment comments