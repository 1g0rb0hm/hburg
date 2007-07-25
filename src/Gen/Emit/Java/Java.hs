-----------------------------------------------------------------------------
-- |
-- Module      :  Java
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- This module implements the JavaClass type class and
-- represents a Java class (e.g. regular class, interface, enumration)
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.Java.Java (
		-- * Introduction
		-- $intro
		Java,
		-- *  Construction
        -- $construction
		java,
	) where

import Util (stringFoldr)

import System.FilePath.Posix (pathSeparator)

import Gen.Emit.EmitClass (EmitClass(..))
import Gen.Emit.JavaClass (JavaClass(..))
import Gen.Emit.Java.JEnum (JEnum)
import qualified Gen.Emit.Java.JMethod as Method (JMethod, setIfaceDef)
import Gen.Emit.Java.JVariable (JVariable)
import Gen.Emit.Java.JConstructor (JConstructor)
import Gen.Emit.Java.JModifier (JModifier(..))
import qualified Gen.Emit.Java.JComment as Comment (JComment, new)

------------------------------------------------------------------------------------

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
		package 		:: Package,				-- Package in which this Java file resides
		imports 		:: [Import],
		enumerations	:: [JEnum],
		modifier		:: JModifier,			-- private|public|protected
		isStatic		:: Bool,
		isFinal			:: Bool,
		isIface			:: Bool,
		comments		:: Comment.JComment,
		name			:: Name,
		constructors	:: [JConstructor],
		staticInit		:: StaticInit,			-- Static initializers
		variables		:: [JVariable],
		methods			:: [Method.JMethod],
		nestedClasses	:: [NestedClass],		-- Nested classes
		moreClasses		:: [AdditionalClass],	-- Additional classes
		userCode		:: UserCode				-- Additional user code
	}

-- | java. Smart constructor.
java :: Package -> Name -> Java
java pack n
	= Class 
		{	package = pack,
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
	-- | get and set a package name
	jSetPackage c pkg = c { package = pkg }
	jGetPackage c = package c

	-- | set an import and retrieve them all
	jSetImports c imp = c { imports = imp }
	jGetImports c = imports c

	-- | set or get class commentss
	jSetComments c com = c { comments = com }
	jGetComments c = comments c

	-- | set and get class name
	jSetClassName c name1 = c { name = name1 }
	jGetClassName c = name c

	-- | is this class static
	jIsStatic c = isStatic c
	jSetStatic c bool = c { isStatic = bool }

	-- | is this class a final class
	jIsFinal c = isFinal c
	jSetFinal c bool = c { isFinal = bool }

	-- | is this an interface definition?
	jIsIface c = isIface c
	jSetIface c bool = c { isIface = bool }

	-- | class modifier (public, private, protected)
	jSetModifier c m = c { modifier = m }
	jGetModifier c = modifier c

	-- | set and get init initializer
	jSetStaticInitializer c i = c { staticInit = i }
	jGetStaticInitializer c =  staticInit c

	-- | add constructors
	jSetConstructors c cnst = c { constructors = cnst }
	jGetConstructors c = constructors c

	-- | add class variables
	jSetVariables c vars = c { variables = vars }
	jGetVariables c = variables c

	-- | add methods
	jSetMethods c meths = c { methods = meths }
	jGetMethods c = methods c

	-- | add a nested class
	jSetNestedClasses c nested = c { nestedClasses = nested }
	jGetNestedClasses c = nestedClasses c

	-- | add enumeration class to class
	jSetEnumClasses c enum = c { enumerations = enum }
	jGetEnumClasses c = enumerations c

	-- | add additional class to a class: A Java file can have one and
	--	only one public Java class. But the file can contain additional non
	--	public classes.
	jSetAdditionalClasses c more = c { moreClasses = more }
	jGetAdditionalClasses c = moreClasses c

	jSetUserCode c uc = c { userCode = uc }
	jGetUserCode (Class { userCode = uc })
		= if (uc /= "")
			then "// @USER CODE START\n" ++ uc ++ "\n// @USER CODE END\n"
			else uc

instance EmitClass Java where
	emit clazz = show clazz
	emitTo clazz 
		= let dir = (map
						(\c -> if (c == '.') then pathSeparator else c)
						(package clazz)) 
			in
		if (dir /= [])
			then dir ++ [pathSeparator] ++ (name clazz) ++ ".java"
			else (name clazz) ++ ".java"

instance Show Java where
		-- | Interface.
		show clazz | (jIsIface clazz)
			= packageDef clazz ++ importDefs clazz ++
			show (jGetComments clazz) ++ "\n" ++
			show (jGetModifier clazz) ++ " interface " ++
			jGetClassName clazz ++ " {\n" ++
			(foldWith "\n" [ "\t" ++ show (Method.setIfaceDef z True) | z <- jGetMethods clazz]) ++ "\n" ++
			"\n} // END INTERFACE " ++ jGetClassName clazz ++ "\n"

		-- | Enumeration.
		show clazz | (jGetEnumClasses clazz /= [])
			= packageDef clazz ++ importDefs clazz ++
			show (jGetComments clazz) ++ "\n" ++
			-- Enumerations
			(foldWith "\n" [ show z | z <- jGetEnumClasses clazz ]) ++ "\n"

		-- | Regular class.
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
importDefs clazz 
	= (foldWith "\n" (jGetImports clazz)) ++ "\n\n"