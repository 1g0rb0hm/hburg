-----------------------------------------------------------------------------
-- |
-- Module      :  Code
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- The target code within a semantic action is represented by this module:
--        * Example: '(: .... :)'
--            the stuff between '(:' and ':)' is target code.
-----------------------------------------------------------------------------

module Hburg.Ast.Code (
  -- Types
  Code,
  -- Functions
  new, empty,
  isEmpty,
) where

{- unqualified imports  -}

{- qualified imports  -}

------------------------------------------------------------------------------------

{- | Code data type -}
data Code =
  MkEmptyCode
  | MkCode String     -- ^ the content of semantic actions (the stuff between '(.' '.)')
  deriving (Eq,Ord)

instance Show Code where
  show (MkEmptyCode) = ""
  show (MkCode str) = str

new :: String -> Code
new str = MkCode str

empty :: Code
empty = MkEmptyCode

isEmpty :: Code -> Bool
isEmpty (MkEmptyCode) = True
isEmpty _ = False

------------------------------------------------------------------------------------