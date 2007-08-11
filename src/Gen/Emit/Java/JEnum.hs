-----------------------------------------------------------------------------
-- |
-- Module      :  JEnum
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java enumeration.
-----------------------------------------------------------------------------

module Gen.Emit.Java.JEnum (
        -- * Types
        JEnum,
        -- * Construction
        new,
    ) where

import Util (stringFoldr)

import Gen.Emit.Java.JModifier(JModifier)
------------------------------------------------------------------------------------

data JEnum
    = MkEnum
        JModifier   -- public|private|protected modifier
        String      -- enumeration identifier
        [String]    -- enumeration elements
    deriving (Eq)

instance Show JEnum where
    show (MkEnum modifier ident enums)
        = " " ++ (show modifier) ++ " enum " ++ -- Modifier
        ident ++ " {\n" ++                      -- Identifier
        (stringFoldr
            (\x y -> "\t" ++ x ++ ",\n" ++ y)
            enums) ++ "};\n"                    -- Enumeration

-- | Constructor for building a JModifier
new :: JModifier -> String -> [String] -> JEnum
new m ident elems = MkEnum m ident elems