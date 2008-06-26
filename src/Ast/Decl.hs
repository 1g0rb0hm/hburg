-----------------------------------------------------------------------------
-- |
-- Module      :  Decl
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Target code specified in the declaration section of the tree pattern
-- matching language.
-----------------------------------------------------------------------------

module Ast.Decl (
  -- Types
  Declaration,
  -- Functions
  new,
) where

{- unqualified imports  -}
import Ast.Code(Code)

{- qualified imports  -}

-----------------------------------------------------------------------------

data Declaration = Decl Code

instance Show Declaration where
  show (Decl c) = show c

new :: Code -> Declaration
new c = Decl c

-----------------------------------------------------------------------------