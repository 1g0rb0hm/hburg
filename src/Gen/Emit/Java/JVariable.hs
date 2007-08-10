-----------------------------------------------------------------------------
-- |
-- Module      :  JVariable
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java variable type.
-----------------------------------------------------------------------------

module Gen.Emit.Java.JVariable (
        -- * Types
        JVariable,
        -- * Construction
        new,
    ) where

import Gen.Emit.Java.JModifier (JModifier)

------------------------------------------------------------------------------------

type Type = String
type Constructor = String

data JVariable 
    = MkJVariable
        JModifier   -- ^ private|public|protected modifier
        Bool        -- ^ is it static?
        Type        -- ^ variable type
        String      -- ^ variable identifier
        Constructor -- ^ how to construct the variable (e.g. new EnumSet.of(blablabla))
    deriving (Eq)

instance Show JVariable where
    show (MkJVariable modifier isStat ty ident constructor) 
        = " " ++ (show modifier) ++ " " ++          -- modifier
        (if (isStat) then "static " else " ") ++    -- is it static ?
        ty ++ " " ++                                -- type
        ident ++ 
        if (constructor /= [])
            then " = " ++                           -- identifier
                constructor ++ ";"                  -- how to construct it?
            else ";"

-- | Constructor for building a new JVariable
new :: JModifier -> Bool -> Type -> String -> Constructor -> JVariable
new m stat ty i con = MkJVariable m stat ty i con