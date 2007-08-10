-----------------------------------------------------------------------------
-- |
-- Module      :  JavaClass
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

module Gen.Emit.JavaClass (
        -- * Classes
        JavaClass(..)
    ) where

import Gen.Emit.EmitClass (EmitClass)
import Gen.Emit.Java.JEnum (JEnum)
import Gen.Emit.Java.JMethod (JMethod)
import Gen.Emit.Java.JVariable (JVariable)
import Gen.Emit.Java.JConstructor (JConstructor)
import Gen.Emit.Java.JModifier (JModifier)
import Gen.Emit.Java.JComment (JComment)
-----------------------------------------------------------------------------

class (EmitClass a) => JavaClass a where
    -- | set package name in which this class resides
    jSetPackage :: a -> String -> a
    -- | retrieve class package name
    jGetPackage :: a -> String
    -- | set imports for this class
    jSetImports :: a -> [String] -> a
    -- | get imports for this class
    jGetImports :: a->  [String]
    -- | get class comments
    jGetComments :: a -> JComment
    -- | get class comments
    jSetComments :: a -> JComment -> a
    -- | set class name
    jSetClassName :: a -> String -> a
    -- | get class name
    jGetClassName :: a -> String
    -- | set static initializer block
    jSetStaticInitializer :: a -> String -> a
    -- | get static initializer block
    jGetStaticInitializer :: a -> String
    -- | set constructors
    jSetConstructors :: a -> [JConstructor] -> a
    -- | get constructors
    jGetConstructors :: a -> [JConstructor]
    -- | set class variables
    jSetVariables :: a -> [JVariable] -> a
    -- | get class variables
    jGetVariables :: a -> [JVariable]
    -- | set methods
    jSetMethods :: a -> [JMethod] -> a
    -- | get methods
    jGetMethods :: a -> [JMethod]
    -- | set class modifier
    jSetModifier :: a -> JModifier -> a
    -- | get class modifier
    jGetModifier :: a -> JModifier
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
    jSetEnumClasses :: a -> [JEnum] -> a
    -- | get enumeration classes from this class
    jGetEnumClasses :: a -> [JEnum]
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