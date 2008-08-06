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
  -- * Types
  Language(..),
  -- * Functions
  emit,
  toLang,
) where

{- unqualified imports  -}
import Hburg.Util (die)

{- qualified imports  -}
import qualified Hburg.Ast.Ir as Ir (Ir(..))

import qualified Hburg.Gen.Ident as I (new)

import qualified Hburg.Gen.Java.Gen as J (generate)
import qualified Hburg.Gen.Java.Class as C (Class)

-----------------------------------------------------------------------------

type Class = String
type Package = String
type NodeKind = String

{- | Supported target languages -}
data Language =
  Java 
  | Csharp
  | Undefined String
  deriving (Eq)

{- | Convert a String to a supported language -}
toLang :: String -> Language
toLang "Java" =  Java
toLang "C#" = Csharp
toLang s = Undefined s

-- | Generates all the code which is necessary in order to make our code generator work.
emit :: Language -> Class -> Package -> NodeKind -> Ir.Ir -> IO [C.Class]
emit lang cname pkg nkind ir =
  case lang of
    Java ->
      return $ J.generate cname (I.new pkg) nkind ir
    Csharp ->
      die "C# target language not supported!"
    Undefined s ->
      die $ s ++ " target language not supported!"

-----------------------------------------------------------------------------