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
import Util (stringFoldr)

import System.FilePath.Posix (pathSeparator)

import Gen.Emit (Emit(emit,emitTo))
import Gen.Emit.Java.Enum as E (Enum)
import Gen.Emit.Java.Var (Var)
import Gen.Emit.Java.Modifier (Modifier(..))

{- qualified imports  -}
import qualified Gen.Emit.Java.Method as M (Method(..))

-----------------------------------------------------------------------------

type Package = String
type Name = String
type Import = String
type StaticInit = String
type NestedClass = Class
type AdditionalClass = Class
type UserCode = String

-- | Java Class Type
data Class =
  Class { package       :: Package            -- ^ package in which this Java file resides
        , imports       :: [Import]           -- ^ java import statements
        , enumerations  :: [E.Enum]           -- ^ defined enumerations
        , modifier      :: Modifier           -- ^ private|public|protected modifier
        , isStatic      :: Bool
        , isFinal       :: Bool
        , isIface       :: Bool
        , name          :: Name               -- ^ class name
        , constructors  :: [M.Method]         -- ^ constructors
        , staticInit    :: StaticInit         -- ^ static initializer block
        , variables     :: [Var]              -- ^ java variables
        , methods       :: [M.Method]         -- ^ java methods
        , nestedClasses :: [NestedClass]      -- ^ nested classes
        , moreClasses   :: [AdditionalClass]  -- ^ additional classes
        , userCode      :: UserCode}          -- ^ user code as defined in the 'declerations' section

-- | new. Smart constructor.
new :: Package -> Name -> Class
new pack n =
  Class { package = pack
        , imports = []
        , enumerations = []
        , modifier = Public
        , isStatic = False
        , isFinal  = False
        , isIface  = False
        , name = n
        , constructors = []
        , staticInit = ""
        , variables = []
        , methods = []
        , nestedClasses = []
        , moreClasses = []
        , userCode = ""
        }

instance Emit Class where
  emit clazz = show clazz
  emitTo clazz =
    let dir = map
                (\c -> if (c == '.') then pathSeparator else c)
                (package clazz) in
    if (not $ null dir)
      then dir ++ [pathSeparator] ++ (name clazz) ++".java"
      else (name clazz) ++".java"

instance Show Class where
  -- Interface.
  show clazz | (isIface clazz) =
    packageDef clazz ++ importDefs clazz ++
    show (modifier clazz) ++" interface "++
    name clazz ++" {\n"++
    (foldWith "\n" [ "\t"++ show (z { M.isIface = True }) | z <- methods clazz])
    ++"\n\n} // END INTERFACE "++ name clazz ++"\n"
  
  -- Enumeration.
  show clazz | (enumerations clazz /= []) =
    packageDef clazz ++ importDefs clazz ++
    -- Enumerations
    (foldWith "\n" [ show z | z <- enumerations clazz ]) ++"\n"
  
  -- Regular class.
  show clazz =
    packageDef clazz ++ importDefs clazz ++
    -- Class name
    show (modifier clazz) ++
    (\b -> if (b) then " static " else " " ) (isStatic clazz)  ++"class "++
    name clazz ++" {\n"++
    -- Class Constructors
    (foldWith "\n"  [ show z | z <- constructors clazz ]) ++"\n"++
    -- Static initializers
    staticInit clazz ++"\n"++
    -- User Code
    userCode clazz ++
    -- Class and instance variables
    (foldWith "\n" [ show z | z <- variables clazz ]) ++"\n\n"++
    -- Methods
    (foldWith "\n\n" [ show z | z <- methods clazz ]) ++"\n"++
    -- Nested Classes
    (foldWith "\n" [ show z | z <- nestedClasses clazz ]) ++"\n"++
    -- Class End
    "\n} // END CLASS "++ name clazz ++"\n"++
    -- Additional Classes
    (foldWith "\n" [ show z | z <- moreClasses clazz ]) ++"\n"

-- | foldWith.
foldWith :: String -> [String] -> String
foldWith z strs = stringFoldr (\x y -> x ++ z ++ y) (strs)

-- | packageDef.
packageDef :: Class -> String
packageDef clazz = if (null $ package clazz)
        then ""
        else "package "++ package clazz ++";\n\n"

-- | importDefs.
importDefs :: Class -> String
importDefs clazz = (foldWith "\n" (imports clazz)) ++"\n\n"

-----------------------------------------------------------------------------