-----------------------------------------------------------------------------
-- |
-- Module      :  Incl
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Target code specified in the include section of the tree pattern
-- matching language.
-----------------------------------------------------------------------------

module Hburg.Ast.Incl (
  -- Types
  Include,
  -- Functions
  new,
) where

{- unqualified imports  -}
import Hburg.Ast.Code(Code)

{- qualified imports  -}

------------------------------------------------------------------------------------

data Include = MkIncl Code

new :: Code -> Include
new c = MkIncl c

instance Show Include where
  show (MkIncl c) = show c

------------------------------------------------------------------------------------