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
    = Class {   package         :: Package              -- ^ package in which this Java file resides
            ,   imports         :: [Import]             -- ^ java import statements
            ,   enumerations    :: [E.Enum]             -- ^ defined enumerations
            ,   modifier        :: Modifier             -- ^ private|public|protected modifier
            ,   isStatic'       :: Bool
            ,   isFinal'        :: Bool
            ,   isIface'        :: Bool
            ,   comments        :: Comment.Comment      -- ^ class comments
            ,   name            :: Name                 -- ^ class name
            ,   constructors    :: [Constructor]        -- ^ constructors
            ,   staticInit      :: StaticInit           -- ^ static initializer block
            ,   variables       :: [Variable]           -- ^ java variables
            ,   methods         :: [Method.Method]      -- ^ java methods
            ,   nestedClasses   :: [NestedClass]        -- ^ nested classes
            ,   moreClasses     :: [AdditionalClass]    -- ^ additional classes
            ,   userCode        :: UserCode}            -- ^ user code as defined in the 'declerations' section

-- | java. Smart constructor.
java :: Package -> Name -> Java
java pack n
    = Class {package = pack,
            imports = [],
            enumerations = [],
            modifier = Public,
            isStatic' = False,
            isFinal' = False,
            isIface' = False,
            comments = Comment.new [n ++" Class"],
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
    setPackage c pkg = c { package = pkg }
    getPackage c = package c

    setImports c imp = c { imports = imp }
    getImports c = imports c

    setComments c com = c { comments = com }
    getComments c = comments c

    setClassName c name1 = c { name = name1 }
    getClassName c = name c

    isStatic c = isStatic' c
    setStatic c bool = c { isStatic' = bool }

    isFinal c = isFinal' c
    setFinal c bool = c { isFinal' = bool }

    isIface c = isIface' c
    setIface c bool = c { isIface' = bool }

    setModifier c m = c { modifier = m }
    getModifier c = modifier c

    setStaticInitializer c i = c { staticInit = i }
    getStaticInitializer c =  staticInit c

    setConstructors c cnst = c { constructors = cnst }
    getConstructors c = constructors c

    setVariables c vars = c { variables = vars }
    getVariables c = variables c

    setMethods c meths = c { methods = meths }
    getMethods c = methods c

    setNestedClasses c nested = c { nestedClasses = nested }
    getNestedClasses c = nestedClasses c

    setEnumClasses c enum = c { enumerations = enum }
    getEnumClasses c = enumerations c

    setAdditionalClasses c more = c { moreClasses = more }
    getAdditionalClasses c = moreClasses c

    setUserCode c uc = c { userCode = uc }
    getUserCode (Class { userCode = uc })
        = if (uc /= "")
            then "// @USER CODE START\n"++ uc ++"\n// @USER CODE END\n"
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
            then dir ++ [pathSeparator] ++ (name clazz) ++".java"
            else (name clazz) ++".java"

instance Show Java where
        -- Interface.
        show clazz | (isIface clazz)
            = packageDef clazz ++ importDefs clazz ++
            show (getComments clazz) ++"\n"++
            show (getModifier clazz) ++" interface "++
            getClassName clazz ++" {\n"++
            (foldWith "\n" [ "\t"++ show (Method.setIfaceDef z True) | z <- getMethods clazz])
            ++"\n\n} // END INTERFACE "++ getClassName clazz ++"\n"

        -- Enumeration.
        show clazz | (getEnumClasses clazz /= [])
            = packageDef clazz ++ importDefs clazz ++
            show (getComments clazz) ++"\n"++
            -- Enumerations
            (foldWith "\n" [ show z | z <- getEnumClasses clazz ]) ++"\n"

        -- Regular class.
        show clazz 
            = packageDef clazz ++ importDefs clazz ++
            show (getComments clazz) ++"\n"++
            -- Class name
            show (getModifier clazz) ++
            (\b -> if (b) then " static " else " " ) (isStatic clazz)  ++"class "++
            getClassName clazz ++" {\n"++
            -- Class Constructors
            (foldWith "\n"  [ show z | z <- getConstructors clazz ]) ++"\n"++
            -- Static initializers
            getStaticInitializer clazz ++"\n"++
            -- User Code
            getUserCode clazz ++
            -- Class and instance variables
            (foldWith "\n" [ show z | z <- getVariables clazz ]) ++"\n\n"++
            -- Methods
            (foldWith "\n\n" [ show z | z <- getMethods clazz ]) ++"\n"++
            -- Nested Classes
            (foldWith "\n" [ show z | z <- getNestedClasses clazz ]) ++"\n"++
            -- Class End
            "\n} // END CLASS "++ getClassName clazz ++"\n"++
            -- Additional Classes
            (foldWith "\n" [ show z | z <- getAdditionalClasses clazz ]) ++"\n"

-- | foldWith.
foldWith :: String -> [String] -> String
foldWith z strs = stringFoldr (\x y -> x ++ z ++ y) (strs)

-- | packageDef.
packageDef :: Java -> String
packageDef clazz 
    = if (getPackage clazz /= "")
        then "package "++ getPackage clazz ++";\n\n"
        else ""

-- | importDefs.
importDefs :: Java -> String
importDefs clazz = (foldWith "\n" (getImports clazz)) ++"\n\n"