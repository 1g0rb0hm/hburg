-----------------------------------------------------------------------------
-- |
-- Module      :  Class
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- This module implements the JavaClass type class and
-- represents a Java class (e.g. regular class, interface, enumration)
-----------------------------------------------------------------------------

module Gen.Emit.Java.Class (
        -- * Types
        Java,
        -- * Construction
        java,
    ) where

import Util (stringFoldr)

import System.FilePath.Posix (pathSeparator)

import Gen.Emit (Emit(emit,emitTo))
import Gen.Emit.Class (JavaClass(..))
import Gen.Emit.Java.Enum as E (Enum)
import qualified Gen.Emit.Java.Method as Method (Method, setIfaceDef)
import Gen.Emit.Java.Variable (Variable)
import Gen.Emit.Java.Constructor (Constructor)
import Gen.Emit.Java.Modifier (Modifier(..))
import qualified Gen.Emit.Java.Comment as Comment (Comment, new)
-----------------------------------------------------------------------------

type Package = String
type Name = String
type Import = String
type StaticInit = String
type NestedClass = Java
type AdditionalClass = Java
type UserCode = String

-- | Java Type
data Java
    = Class {
        package         :: Package,             -- ^ package in which this Java file resides
        imports         :: [Import],            -- ^ java import statements
        enumerations    :: [E.Enum],             -- ^ defined enumerations
        modifier        :: Modifier,           -- ^ private|public|protected modifier
        isStatic        :: Bool,
        isFinal         :: Bool,
        isIface         :: Bool,
        comments        :: Comment.Comment,    -- ^ class comments
        name            :: Name,                -- ^ class name
        constructors    :: [Constructor],      -- ^ constructors
        staticInit      :: StaticInit,          -- ^ static initializer block
        variables       :: [Variable],         -- ^ java variables
        methods         :: [Method.Method],    -- ^ java methods
        nestedClasses   :: [NestedClass],       -- ^ nested classes
        moreClasses     :: [AdditionalClass],   -- ^ additional classes
        userCode        :: UserCode             -- ^ user code as defined in the 'declerations' section
    }

-- | java. Smart constructor.
java :: Package -> Name -> Java
java pack n
    = Class {package = pack,
            imports = [],
            enumerations = [],
            modifier = Public,
            isStatic = False,
            isFinal = False,
            isIface = False,
            comments = Comment.new [n ++ " Class"],
            name = n,
            constructors = [],
            staticInit = "",
            variables = [],
            methods = [],
            nestedClasses = [],
            moreClasses = [],
            userCode = ""
        }

instance JavaClass Java where
    jSetPackage c pkg = c { package = pkg }
    jGetPackage c = package c

    jSetImports c imp = c { imports = imp }
    jGetImports c = imports c

    jSetComments c com = c { comments = com }
    jGetComments c = comments c

    jSetClassName c name1 = c { name = name1 }
    jGetClassName c = name c

    jIsStatic c = isStatic c
    jSetStatic c bool = c { isStatic = bool }

    jIsFinal c = isFinal c
    jSetFinal c bool = c { isFinal = bool }

    jIsIface c = isIface c
    jSetIface c bool = c { isIface = bool }

    jSetModifier c m = c { modifier = m }
    jGetModifier c = modifier c

    jSetStaticInitializer c i = c { staticInit = i }
    jGetStaticInitializer c =  staticInit c

    jSetConstructors c cnst = c { constructors = cnst }
    jGetConstructors c = constructors c

    jSetVariables c vars = c { variables = vars }
    jGetVariables c = variables c

    jSetMethods c meths = c { methods = meths }
    jGetMethods c = methods c

    jSetNestedClasses c nested = c { nestedClasses = nested }
    jGetNestedClasses c = nestedClasses c

    jSetEnumClasses c enum = c { enumerations = enum }
    jGetEnumClasses c = enumerations c

    jSetAdditionalClasses c more = c { moreClasses = more }
    jGetAdditionalClasses c = moreClasses c

    jSetUserCode c uc = c { userCode = uc }
    jGetUserCode (Class { userCode = uc })
        = if (uc /= "")
            then "// @USER CODE START\n" ++ uc ++ "\n// @USER CODE END\n"
            else uc

instance Emit Java where
    emit clazz = show clazz
    emitTo clazz
        = let dir
                = (map
                    (\c -> if (c == '.') then pathSeparator else c)
                    (package clazz)) 
            in
        if (dir /= [])
            then dir ++ [pathSeparator] ++ (name clazz) ++ ".java"
            else (name clazz) ++ ".java"

instance Show Java where
        -- Interface.
        show clazz | (jIsIface clazz)
            = packageDef clazz ++ importDefs clazz ++
            show (jGetComments clazz) ++ "\n" ++
            show (jGetModifier clazz) ++ " interface " ++
            jGetClassName clazz ++ " {\n" ++
            (foldWith "\n" [ "\t" ++ show (Method.setIfaceDef z True) | z <- jGetMethods clazz])
            ++ "\n\n} // END INTERFACE " ++ jGetClassName clazz ++ "\n"

        -- Enumeration.
        show clazz | (jGetEnumClasses clazz /= [])
            = packageDef clazz ++ importDefs clazz ++
            show (jGetComments clazz) ++ "\n" ++
            -- Enumerations
            (foldWith "\n" [ show z | z <- jGetEnumClasses clazz ]) ++ "\n"

        -- Regular class.
        show clazz 
            = packageDef clazz ++ importDefs clazz ++
            show (jGetComments clazz) ++ "\n" ++
            -- Class name
            show (jGetModifier clazz) ++
            (\b -> if (b) then " static " else " " ) (jIsStatic clazz)  ++ "class " ++
            jGetClassName clazz ++ " {\n" ++
            -- Class Constructors
            (foldWith "\n"  [ show z | z <- jGetConstructors clazz ]) ++ "\n" ++
            -- Static initializers
            jGetStaticInitializer clazz ++ "\n" ++
            -- User Code
            jGetUserCode clazz ++
            -- Class and instance variables
            (foldWith "\n" [ show z | z <- jGetVariables clazz ]) ++ "\n\n" ++
            -- Methods
            (foldWith "\n\n" [ show z | z <- jGetMethods clazz ]) ++ "\n" ++
            -- Nested Classes
            (foldWith "\n" [ show z | z <- jGetNestedClasses clazz ]) ++ "\n" ++
            -- Class End
            "\n} // END CLASS " ++ jGetClassName clazz ++ "\n" ++
            -- Additional Classes
            (foldWith "\n" [ show z | z <- jGetAdditionalClasses clazz ]) ++ "\n"

-- | foldWith.
foldWith :: String -> [String] -> String
foldWith z strs = stringFoldr (\x y -> x ++ z ++ y) (strs)

-- | packageDef.
packageDef :: Java -> String
packageDef clazz 
    = if (jGetPackage clazz /= "")
        then "package " ++ jGetPackage clazz ++ ";\n\n"
        else ""

-- | importDefs.
importDefs :: Java -> String
importDefs clazz = (foldWith "\n" (jGetImports clazz)) ++ "\n\n"