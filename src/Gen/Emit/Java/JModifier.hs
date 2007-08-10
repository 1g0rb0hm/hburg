-----------------------------------------------------------------------------
-- |
-- Module      :  JModifier
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java modifiers.
-----------------------------------------------------------------------------

module Gen.Emit.Java.JModifier (
        -- * Types
        JModifier(..),
    ) where

------------------------------------------------------------------------------------

-- | Java modifier types
data JModifier
    = Private
    | Public
    | Protected
    | NoModifier
    deriving (Eq)

instance Show JModifier where
    show (Protected) = "protected"
    show (Public) = "public"
    show (Private) = "private"
    show (NoModifier) = ""