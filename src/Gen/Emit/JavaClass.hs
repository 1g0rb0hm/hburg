-----------------------------------------------------------------------------
-- |
-- Module      :  JavaClass
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Type Class which captures most of the features a
-- Java Class provides.
--
-----------------------------------------------------------------------------

module Gen.Emit.JavaClass (
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
	-- | get|set package name
	jSetPackage :: a -> String -> a
	jGetPackage :: a -> String
	-- | get|set imports
	jSetImports :: a -> [String] -> a
	jGetImports :: a->  [String]
	-- | get|set class comments
	jGetComments :: a -> JComment
	jSetComments :: a -> JComment -> a
	-- | get|set class name
	jSetClassName :: a -> String -> a
	jGetClassName :: a -> String
	-- | get|set static initializer
	jSetStaticInitializer :: a -> String -> a
	jGetStaticInitializer :: a -> String
	-- | get|set constructors
	jSetConstructors :: a -> [JConstructor] -> a
	jGetConstructors :: a -> [JConstructor]
	-- | get|set class variables
	jSetVariables :: a -> [JVariable] -> a
	jGetVariables :: a -> [JVariable]
	-- | get|set methods
	jSetMethods :: a -> [JMethod] -> a
	jGetMethods :: a -> [JMethod]
	-- | get|set modifiers
	jSetModifier :: a -> JModifier -> a
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

	-- | The following functions all provide methods to nest and add
	--	Java classes in various ways. For more information
	--	@see http://en.wikibooks.org/wiki/Java_Programming/Nested_Classes
	--	@see http://java.sun.com/docs/books/tutorial/java/javaOO/nested.html

	-- | get|set a nested class
	jSetNestedClasses :: a -> [a] -> a
	jGetNestedClasses :: a -> [a]
	-- | get|set enumeration class to class
	jSetEnumClasses :: a -> [JEnum] -> a
	jGetEnumClasses :: a -> [JEnum]
	-- | get|set additional class: A Java file can have one and
	--	only one public Java class. But the file can contain additional non
	--	public classes.
	jSetAdditionalClasses :: a -> [a] -> a
	jGetAdditionalClasses :: a -> [a]
	-- | get|set user defined code which gets included right after the class 
	--	class declaration.
	jSetUserCode :: a -> String -> a
	jGetUserCode :: a -> String
