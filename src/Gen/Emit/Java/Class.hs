-----------------------------------------------------------------------------
-- |
-- Module      :  Class
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module implements the JavaClass type class and
-- represents a Java class (e.g. regular class, interface, enumration)
-----------------------------------------------------------------------------

module Gen.Emit.Java.Class (
  -- * Types
  Class(..),
  -- * Functions
  new,
) where

{- unqualified imports  -}

import Text.PrettyPrint

import System.FilePath.Posix (pathSeparator)

import Gen.Document (Document(..))

import Gen.Emit (Emit(emit,emitTo))
import Gen.Emit.Java.Enum as E (Enum)
import Gen.Emit.Java.Var (Var)
import Gen.Emit.Java.Modifier (Modifier(..))

{- qualified imports  -}
import qualified Gen.Emit.Java.Method as M (Method(..))

-----------------------------------------------------------------------------

type Package = String
type Name = String
type Import = Doc

-- | Java Class Type
data Class =
  Class { package       :: Package      -- ^ package in which this Java file resides
        , imports       :: [Import]     -- ^ java import statements
        , enumerations  :: [E.Enum]     -- ^ defined enumerations
        , modifier      :: Modifier     -- ^ private|public|protected modifier
        , isStatic      :: Bool         
        , isFinal       :: Bool         
        , isIface       :: Bool         
        , name          :: Name         -- ^ class name
        , constructors  :: [M.Method]   -- ^ constructors
        , staticInit    :: Doc          -- ^ static initializer block
        , variables     :: [Var]        -- ^ java variables
        , methods       :: [M.Method]   -- ^ java methods
        , nestedClasses :: [Class]      -- ^ nested classes
        , moreClasses   :: [Class]      -- ^ additional classes
        , userCode      :: Doc}         -- ^ user code as defined in the 'declerations' section

{- | Shortcut -}
t :: String -> Doc
t = text

{- | new. Smart constructor. -}
new :: Package -> Name -> Class
new p n =
  Class { package = p
        , imports = []
        , enumerations = []
        , modifier = Public
        , isStatic = False
        , isFinal  = False
        , isIface  = False
        , name = n
        , constructors = []
        , staticInit = empty
        , variables = []
        , methods = []
        , nestedClasses = []
        , moreClasses = []
        , userCode = empty}

instance Emit Class where
  emit c = show c
  emitTo c =
    let dir = map
                (\c' ->
                  if (c' == '.')
                    then pathSeparator
                    else c')
                (package c) in
    if (null dir)
      then (name c) ++".java"
      else dir ++ [pathSeparator] ++ (name c) ++".java"

instance Show Class where
  show c = render . toDoc $ c

instance Document Class where
  -- Interface
  toDoc c
    | isIface c =
      packageDef c
      $+$ (vcat . imports $ c)
      $+$ toDoc (modifier c) <+> t "interface" <+> t (name c)
      <+> lbrace
        $+$ nest 2 (
          vcat (map
                (\z -> toDoc z { M.isIface = True})
                (methods c)))
      $+$ rbrace <+> t "// END INTERFACE"
      
  -- Enumeration
  toDoc c
    | not . null . enumerations $ c =
      packageDef c
      $+$ (vcat . imports $ c)
      $+$ vcat (map (toDoc) (enumerations c))
  
  -- Class
  toDoc c = packageDef c
    $+$ (vcat . imports $ c)
    -- Class name
    $+$ (toDoc . modifier $ c)
    <+> (if (isStatic c)
      then t "static"
      else empty)
    <+> t "class" <+> t (name c)
    <+> lbrace
      $+$ nest 2 (
        -- Class constructorcs
        (vcat . toDocs . constructors $ c)
        -- Static initializers
        $+$ staticInit c
        -- User code
        $+$ (if (isEmpty $ userCode c)
          then empty
          else t "// @USER CODE START"
            $+$ userCode c 
            $+$ t "// @USER CODE END")
        -- Class and instance variables
        $+$ (vcat . toDocs . variables $ c)
        -- Methods
        $+$ (vcat . toDocs . methods $ c)
        -- Nested classes
        $+$ (vcat . toDocs . nestedClasses $ c))
    $+$ rbrace <+> t "// END CLASS" <+> t (name c)
    -- Additional classes
    $+$ (vcat . toDocs . moreClasses $ c)

-- | packageDef.
packageDef :: Class -> Doc
packageDef c =
  if (null $ package c)
    then empty
    else t "package" <+> t (package c) <> semi

-----------------------------------------------------------------------------