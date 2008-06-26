-----------------------------------------------------------------------------
-- |
-- Module      :  Modifier
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Representation of Java modifiers.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Modifier (
  -- * Types
  Modifier(..),
) where

{- unqualified imports  -}

{- qualified imports  -}

-----------------------------------------------------------------------------

-- | Java modifier types
data Modifier =
  Private
  | Public
  | Protected
  | NoModifier
  deriving (Eq)

instance Show Modifier where
  show (Protected) = "protected"
  show (Public) = "public"
  show (Private) = "private"
  show (NoModifier) = ""

-----------------------------------------------------------------------------