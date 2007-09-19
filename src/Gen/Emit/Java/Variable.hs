-----------------------------------------------------------------------------
-- |
-- Module      :  Variable
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java variable type.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Variable (
        -- * Types
        Variable,
        -- * Construction
        new,
    ) where

import Gen.Emit.Java.Modifier (Modifier)

------------------------------------------------------------------------------------

type Type = String
type Constructor = String

data Variable 
    = MkVariable
        Modifier   -- private|public|protected modifier
        Bool        -- is it static?
        Type        -- variable type
        String      -- variable identifier
        Constructor -- how to construct the variable (e.g. new EnumSet.of(blablabla))
    deriving (Eq)

instance Show Variable where
    show (MkVariable modifier isStat ty ident constructor) 
        = " " ++ (show modifier) ++ " " ++          -- modifier
        (if (isStat) then "static " else " ") ++    -- is it static ?
        ty ++ " " ++                                -- type
        ident ++ 
        if (constructor /= [])
            then " = " ++                           -- identifier
                constructor ++ ";"                  -- how to construct it?
            else ";"

-- | Constructor for building a new Variable
new :: Modifier -> Bool -> Type -> String -> Constructor -> Variable
new m stat ty i con = MkVariable m stat ty i con