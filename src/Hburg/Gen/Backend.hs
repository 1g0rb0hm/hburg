-----------------------------------------------------------------------------
-- |
-- Module      :  Backend
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module creates all the necessary 'code' given an AST of definitions
-- from the tree pattern matching language. The emit() function is the main
-- interface to the outside, abstracting away from the business of code generation.
-----------------------------------------------------------------------------

module Hburg.Gen.Backend (
  -- * Functions
  emit,
) where

{- unqualified imports  -}

{- qualified imports  -}
import qualified Hburg.Ast.Ir as Ir (Ir(..))

import qualified Hburg.Gen.Ident as I (new)

import qualified Hburg.Gen.Java.Gen as J (generate)
import qualified Hburg.Gen.Java.Class as C (Class)

-----------------------------------------------------------------------------

type Class = String
type Package = String
type NodeKind = String

-- | Generates all the code which is necessary in order to make our code generator work.
emit :: Class -> Package -> NodeKind -> Ir.Ir -> [C.Class]
emit cname pkg nkind ir = J.generate cname (I.new pkg) nkind ir

-----------------------------------------------------------------------------