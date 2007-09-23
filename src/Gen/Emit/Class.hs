-----------------------------------------------------------------------------
-- |
-- Module      :  Class
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Type Class which captures most of the features a Java Class provides.
--
-- More information on how to nest Java Classes in various ways
--    can be found at:
--    * <http://en.wikibooks.org/wiki/Java_Programming/Nested_Classes>
--    * <http://java.sun.com/docs/books/tutorial/java/javaOO/nested.html>
-----------------------------------------------------------------------------

module Gen.Emit.Class (
        -- * Classes
        JavaClass(..)
    ) where

import Prelude hiding (Enum)

import Gen.Emit (Emit)
import qualified Gen.Emit.Java.Enum as E (Enum)
import Gen.Emit.Java.Method (Method)
import Gen.Emit.Java.Variable (Variable)
import Gen.Emit.Java.Constructor (Constructor)
import Gen.Emit.Java.Modifier (Modifier)
import Gen.Emit.Java.Comment (Comment)
-----------------------------------------------------------------------------

class (Emit a) => JavaClass a where
    -- | set package name in which this class resides
    setPackage :: a -> String -> a
    -- | retrieve class package name
    getPackage :: a -> String
    -- | set imports for this class
    setImports :: a -> [String] -> a
    -- | get imports for this class
    getImports :: a ->  [String]
    -- | get class comments
    getComments :: a -> Comment
    -- | get class comments
    setComments :: a -> Comment -> a
    -- | set class name
    setClassName :: a -> String -> a
    -- | get class name
    getClassName :: a -> String
    -- | set static initializer block
    setStaticInitializer :: a -> String -> a
    -- | get static initializer block
    getStaticInitializer :: a -> String
    -- | set constructors
    setConstructors :: a -> [Constructor] -> a
    -- | get constructors
    getConstructors :: a -> [Constructor]
    -- | set class variables
    setVariables :: a -> [Variable] -> a
    -- | get class variables
    getVariables :: a -> [Variable]
    -- | set methods
    setMethods :: a -> [Method] -> a
    -- | get methods
    getMethods :: a -> [Method]
    -- | set class modifier
    setModifier :: a -> Modifier -> a
    -- | get class modifier
    getModifier :: a -> Modifier
    -- | is the class a static class
    isStatic :: a -> Bool
    setStatic :: a -> Bool -> a
    -- | is the class a final class
    isFinal :: a -> Bool
    setFinal :: a -> Bool -> a
    -- | is this an interface
    isIface :: a -> Bool
    setIface :: a -> Bool -> a

    -- | add nested classes to this class
    setNestedClasses :: a -> [a] -> a
    -- | get nested classes from this class
    getNestedClasses :: a -> [a]
    -- | add enumeration classes to this class
    setEnumClasses :: a -> [E.Enum] -> a
    -- | get enumeration classes from this class
    getEnumClasses :: a -> [E.Enum]
    -- | add additional class to this class: 
    --      * A Java file can have one and only one public Java class.
    --          But the file can contain additional non public classes.
    setAdditionalClasses :: a -> [a] -> a
    -- | get additional java classes
    getAdditionalClasses :: a -> [a]
    -- | set user defined code which gets included right after the class declaration.
    --      This code is defined in the 'declerations' part of the tree pattern matching
    --      grammar.
    setUserCode :: a -> String -> a
    -- | get user defined code
    getUserCode :: a -> String