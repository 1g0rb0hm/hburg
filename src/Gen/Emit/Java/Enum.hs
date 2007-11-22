-----------------------------------------------------------------------------
-- |
-- Module      :  Enum
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java enumeration.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Enum (
        -- * Types
        Enum,
        -- * Construction
        new,
    ) where

import Prelude hiding (Enum)

import Util (stringFoldr)

import Gen.Emit.Java.Modifier(Modifier)
------------------------------------------------------------------------------------

data Enum
    = MkEnum
        Modifier   -- public|private|protected modifier
        String      -- enumeration identifier
        [String]    -- enumeration elements
    deriving (Eq)

instance Show Enum where
    show (MkEnum modifier ident enums)
        = " "++ (show modifier) ++" enum "++ -- Modifier
        ident ++" {\n"++                      -- Identifier
        (stringFoldr
            (\x y -> "\t"++ x ++",\n"++ y)
            enums) ++"};\n"                    -- Enumeration

-- | Constructor for building a Modifier
new :: Modifier -> String -> [String] -> Enum
new m ident elems = MkEnum m ident elems