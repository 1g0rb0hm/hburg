-----------------------------------------------------------------------------
-- |
-- Module      :  Comment
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java comments.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Comment (
        -- * Types
        Comment,
        -- * Construction
        new,
    ) where

import Util (stringFoldr)
------------------------------------------------------------------------------------

data Comment
    = MkJCommment
        [String]    -- Comments

instance Show Comment where
    show (MkJCommment []) = ""
    show (MkJCommment (x:[])) = "// " ++ x
    show (MkJCommment xs)
        = "/**\n * " ++
        (stringFoldr
            (\x y -> x ++ "\n * " ++ y)
            xs) ++ "\n */"

-- | Constructor for building a comment
new :: [String] -> Comment
new comments = MkJCommment comments