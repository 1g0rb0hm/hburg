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

module Gen.Backend (
  -- * Functions
  emit,
) where

{- unqualified imports  -}

{- qualified imports  -}
import qualified Ast.Ir as Ir (Ir(..), baseRuleMap, linkSet)

import qualified Gen.Ident as I (new)

import qualified Gen.Emit.Java.Generator as Java (generate)
import qualified Gen.Emit.Java.Class as Class (Class)

-----------------------------------------------------------------------------

type ClassName = String
type PackageName = String
type NodeKind = String

-- | Generates all the code which is necessary in order to make our code generator work.
emit :: ClassName -> PackageName -> NodeKind -> Ir.Ir -> [Class.Class]
emit cname pkg nkind ir = Java.generate cname (I.new pkg) nkind ir

-----------------------------------------------------------------------------