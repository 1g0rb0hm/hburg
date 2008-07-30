-----------------------------------------------------------------------------
-- |
-- Module      :  Debug
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Store and manipulate debugging output.
-----------------------------------------------------------------------------

module Hburg.Debug (
  -- Types
  Level(..),Entry,
  -- Functions
  new, filter, format,
) where

{- unqualified imports  -}
import Prelude hiding (filter)

{- qualified imports  -}
import qualified Data.List as List (filter)

-----------------------------------------------------------------------------

{- | Debugging level -}
data Level =
  Info
  | Debug
  | Warn
  | Error
  | All
  deriving (Eq, Ord, Show)

{- | Debugging entry -}
data Entry = Entry Level String
  deriving (Eq,Ord,Show)

{- | Construct a debug entry -}
new :: Level -> String -> Entry
new lvl e = Entry lvl e

{- | Filter debug entries according to level -}
filter :: Level -> [Entry] -> [Entry]
filter All es = es
filter lvl es = List.filter (\(Entry l _) -> l == lvl) es

{- | Formats debug entries -}
format :: [Entry] -> String
format [] = ""
format es = "\n" ++ concatMap (\(Entry _ msg) -> msg ++ "\n") es

-----------------------------------------------------------------------------