-----------------------------------------------------------------------------
-- |
-- Module      :  Emit
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- This module creates all the necessary 'code' given an AST of definitions
-- from the tree pattern matching language. The emit() function is the main
-- interface to the outside, abstracting away from the business of code generation.
--
-----------------------------------------------------------------------------

module Gen.Emit (
		-- Functions
		emit,
	) where

import Util (stringFoldr)

import Ast.Op (Operator)
import Ast.Def (Definition)
import Ast.Incl(Include)
import Ast.Decl(Declaration)

import Gen.Emit.EmitEnums (genEnums)
import Gen.Emit.EmitTiling (genTiling)
import Gen.Emit.EmitEval (genEval)
import Gen.Emit.EmitMapEntry (genMapEntry)

import Gen.Emit.JavaClass (JavaClass(..))
import Gen.Emit.Java.Java (Java, java)
import qualified Gen.Emit.Java.JMethod as Method (JMethod, new, setComment, getParams, getName, getRetTy)
import Gen.Emit.Java.JModifier (JModifier(..))
import qualified Gen.Emit.Java.JParameter as Parameter (getIdent)
import qualified Gen.Emit.Java.JComment as Comment (new)

------------------------------------------------------------------------------------

type Class = String
type Package = String
type Import = String
type NodeKind = String

-- | emit. Generate all the code which is necessary in order to make our code
--		generator work.
emit :: Class -> Package -> NodeKind -> Include -> Declaration -> [Operator] -> [Definition] -> [Java]
emit clazz pkg nkind incl decl ops ds
	= 
	let j0 = java pkg clazz in						-- 1. Create Java Class
	let (defs, js) = genEnums pkg ds in				-- 2. Generate Enumerations: Definitions are altered during this step since
													--		productions are labelled with rules and their result labels
	let j1 = jSetImports 							-- 3. Add appropriate imports
				j0 [genImport pkg "NT.*" True, 
					genImport pkg "RuleEnum.*" True,
					genImport pkg "NT" False, 
					genImport pkg "RuleEnum" False,
					genImport pkg "MapEntry" False,
					genImport "java.util" "EnumSet" False,
					"// @USER INCLUDES START", (show incl), "// @USER INCLUDES END"]
		in
	let (j2, nodeIf) = genTiling ops defs nkind j1 in	-- 4. Generate Tiling Class
	let j3 = genEval decl defs j2 in					-- 5. Generate Eval Class
	let j4 = jSetMethods j3 [genEmitFun j3] in
	let j5 = genMapEntry pkg in							-- 6. Generate MapEntry Tuple Type
	[j4, j5, nodeIf] ++ js								-- 7. Return finished classes
	where
		-- | genImport. Generates import statements
		genImport :: Package -> Class -> Bool -> Import
		genImport pkg clazz static
			= let prefix = if (pkg /= "")
							then pkg ++ "."
							else ""
				in
			if (static)
				then "import static " ++ prefix ++ clazz ++ ";"
				else "import " ++ prefix ++ clazz ++ ";"

		-- | genEmitFun. The method in our code generator which is public and callable 
		--		from the outside.
		genEmitFun :: Java -> Method.JMethod
		genEmitFun j
			= let evalClass					-- retrieve class which does the Evaluation
					= case (jGetNestedClasses j) of
						[] -> error ("\nERROR: Class " ++ jGetClassName j ++ " has no nested classes!\n")
						list -> last list
				in
			let m1							-- retrieve the entry method for evaluation
					= case (jGetMethods evalClass) of
						[] -> error ("\nERROR: Class " ++ jGetClassName evalClass ++ " has no methods!\n")
						list -> head list 
				in
			let m2 							-- given the entry method for evaluation, its parameters and return
											-- type, generate the emit method which serves as an entry point
											-- to our code generator
					= Method.new Public True (Method.getRetTy m1) "emit" (Method.getParams m1) (genBody m1) in
			Method.setComment m2 (Comment.new ["emit():", "  Generate Code for AST starting with root node."])
			where
				-- | genBody. Method body of emit()
				genBody :: Method.JMethod -> String
				genBody m
					= "\t" ++ clazz ++ ".Tiling.tile(n);\n" ++
					(if (Method.getRetTy m == "void")
						then "\t"
						else "\treturn ") ++
					clazz ++ ".Eval." ++ (Method.getName m) ++ 
					"(" ++
					(stringFoldr
						(\x y -> x ++ ", " ++ y)
						(map (\p -> Parameter.getIdent p) (Method.getParams m))) ++
					");"