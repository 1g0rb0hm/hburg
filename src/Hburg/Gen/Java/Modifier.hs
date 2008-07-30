-----------------------------------------------------------------------------
-- |
-- Module      :  Modifier
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Representation of Java modifiers.
-----------------------------------------------------------------------------

module Hburg.Gen.Java.Modifier (
  -- * Types
  Modifier(..),
) where

{- unqualified imports  -}
import Text.PrettyPrint

import Hburg.Gen.Doc (Document(..))

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
  show m = render . toDoc $ m

instance Document Modifier where
  toDoc (Protected) = text "protected"
  toDoc (Public) = text "public"
  toDoc (Private) = text "private"
  toDoc (NoModifier) = empty

-----------------------------------------------------------------------------