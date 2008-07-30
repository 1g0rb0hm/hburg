-----------------------------------------------------------------------------
-- |
-- Module      :  Document
-- Copyright   :  Copyright (c) 2008  - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :    <igor@bytelabs.org>
--
--  Any Type that should be used with the PrettyPrint module should
--  implement the toDoc function.
-----------------------------------------------------------------------------

module Hburg.Gen.Doc (
  -- * Types
  Document(..),
  -- * Functions
) where

{- unqualified imports -}
import Text.PrettyPrint (Doc)

{- qualified imports -}

-----------------------------------------------------------------------------

{- | Whatever is an instance of Document can be turned into a Doc -}
class Document a where
  toDoc :: a -> Doc

  toDocs :: [a] -> [Doc]
  toDocs ds = map toDoc ds

-----------------------------------------------------------------------------