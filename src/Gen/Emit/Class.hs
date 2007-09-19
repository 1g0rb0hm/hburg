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
    jSetPackage :: a -> String -> a
    -- | retrieve class package name
    jGetPackage :: a -> String
    -- | set imports for this class
    jSetImports :: a -> [String] -> a
    -- | get imports for this class
    jGetImports :: a->  [String]
    -- | get class comments
    jGetComments :: a -> Comment
    -- | get class comments
    jSetComments :: a -> Comment -> a
    -- | set class name
    jSetClassName :: a -> String -> a
    -- | get class name
    jGetClassName :: a -> String
    -- | set static initializer block
    jSetStaticInitializer :: a -> String -> a
    -- | get static initializer block
    jGetStaticInitializer :: a -> String
    -- | set constructors
    jSetConstructors :: a -> [Constructor] -> a
    -- | get constructors
    jGetConstructors :: a -> [Constructor]
    -- | set class variables
    jSetVariables :: a -> [Variable] -> a
    -- | get class variables
    jGetVariables :: a -> [Variable]
    -- | set methods
    jSetMethods :: a -> [Method] -> a
    -- | get methods
    jGetMethods :: a -> [Method]
    -- | set class modifier
    jSetModifier :: a -> Modifier -> a
    -- | get class modifier
    jGetModifier :: a -> Modifier
    -- | is the class a static class
    jIsStatic :: a -> Bool
    jSetStatic :: a -> Bool -> a
    -- | is the class a final class
    jIsFinal :: a -> Bool
    jSetFinal :: a -> Bool -> a
    -- | is this an interface
    jIsIface :: a -> Bool
    jSetIface :: a -> Bool -> a

    -- | add nested classes to this class
    jSetNestedClasses :: a -> [a] -> a
    -- | get nested classes from this class
    jGetNestedClasses :: a -> [a]
    -- | add enumeration classes to this class
    jSetEnumClasses :: a -> [E.Enum] -> a
    -- | get enumeration classes from this class
    jGetEnumClasses :: a -> [E.Enum]
    -- | add additional class to this class: 
    --      * A Java file can have one and only one public Java class.
    --          But the file can contain additional non public classes.
    jSetAdditionalClasses :: a -> [a] -> a
    -- | get additional java classes
    jGetAdditionalClasses :: a -> [a]
    -- | set user defined code which gets included right after the class declaration.
    --      This code is defined in the 'declerations' part of the tree pattern matching
    --      grammar.
    jSetUserCode :: a -> String -> a
    -- | get user defined code
    jGetUserCode :: a -> String