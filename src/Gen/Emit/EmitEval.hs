-----------------------------------------------------------------------------
-- |
-- Module      :  EmitEval
-- Copyright   :  Copyright (c) 2007 Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- @TODO: Write short summary
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.EmitEval (
		-- Types

		-- Functions
		genEval,
	) where

import Maybe (fromJust, isJust)

import Ast.Attr (attrGetIn, attrGetOut, attrId, attrTy)
import Ast.TermTy (TermTy, TermTyClass(..))
import qualified Ast.Code as C (Code, isEmpty)
import Ast.Node (Node, mapPreOrder3, getSem1, getSem2, getSem3, getSem4,
		getSem5, getSem6, getTy, hasLink, getLink)
import Ast.Prod (getRuleLabel, getNode)
import Ast.Def (Definition, getProds, getCode)
import Ast.Decl (Declaration)

import Gen.Emit.Label (defToEvalLabel, defToEnumLabel, childCallLabel, tTyToEvalLabel)

import Gen.Emit.JavaClass (JavaClass(..))
import Gen.Emit.Java.Java (Java, java)
import Gen.Emit.Java.JModifier (JModifier(..))
import qualified Gen.Emit.Java.JMethod as Method (JMethod, new)
import qualified Gen.Emit.Java.JParameter as Parameter (JParameter, new)


-----------------------------------------------------------------------------


-- | genEval. This function is the top level function for 
--		generating the Target Source Code of the code emission.
genEval :: Declaration -> [Definition] -> Java -> Java
genEval decl defs clazz
	=
	let j0 = jSetModifier (java "" "Eval") Private in	-- Create new class which will hold eval stuff
	let j1 = jSetStatic j0 True in
	let nestedClass = jSetMethods j1 (genEvalMethods defs) in
	let parentClass = jSetUserCode clazz (show decl) in
	jSetNestedClasses									-- the Eval class is nested into another class
		parentClass 
		(jGetNestedClasses parentClass ++ [nestedClass])

-- | returnType.
returnType :: Definition -> String
returnType d 
	= case attrGetOut (getAttr d) of
		[] -> "void"
		(x:_) -> show (attrTy x)

-- | defineReturnVar. If a definition returns something (e.g. has an out parameter),
--		this function defines the variable which holds the result.
defineReturnVar :: Definition -> String -> String
defineReturnVar d indent
	= case attrGetOut (getAttr d) of
		[] -> "";
		list -> indent ++ concatMap (\x -> show x ++ ";\n") list

-- | returnStmt. Generates the return statement given a definition.
returnStmt :: Definition -> String
returnStmt d
	= case attrGetOut (getAttr d) of
		[] -> "";
		list -> concatMap (\x -> "return " ++ show (attrId x) ++ ";\n") list


-- | genParameters. Calculate parameters for each evaluation method.
genParameters :: Definition -> [Parameter.JParameter]
genParameters d
	= case attrGetIn (getAttr d) of
		[] -> []
		list -> map (\x -> Parameter.new (show (attrTy x)) (show (attrId x))) (list)

-- | genEvalMethods. Generates all evaluations methods which emit code supplied
--		by the user in semantic actions.
genEvalMethods :: [Definition] -> [Method.JMethod]
genEvalMethods defs
	= map
		(\d ->
			let params = [Parameter.new "Node" "n"] ++ (genParameters d) in
			Method.new Private True (returnType d) (defToEvalLabel d) params (funBody d))
		(defs)
	where
		-- | funBody. Body of each evaluation methods.
		--		Body structure:
		--			1. retrieve rule label for current node
		--			2. define possible return variable if present
		--			3. emit semantic action defined at nt definition level
		--			4. emit cases which emit user supplied code for the various rule labels.
		--			5. return result of this definition if present
		funBody :: Definition ->  String
		funBody d
			= "\tRuleEnum r = n.rule(" ++ defToEnumLabel d ++ ");\n" ++
			defineReturnVar d "\t\n\t" ++
			wrapUserCode "\t" (getCode d) ++
			genCases d "\t\n\t" ++
			returnStmt d

		-- | wrapUserCode. Wraps up user code with '(:' and ':)' so in case
		--		of compile errors, it will be a bit easier to identify automatically
		--		generated from user specified code
		wrapUserCode :: String -> C.Code -> String
		wrapUserCode _ code | C.isEmpty code
		 	= ""
		wrapUserCode indent code
			= indent ++ "// (:\n" ++
			show code ++
			"\n" ++ indent ++ "// :)\n"

		-- | genCases. Generate case statements for rule labels which. Within
		--		each case statement the semantic actions specifed by the user
		--		are inserted.
		genCases :: Definition -> String -> String
		genCases def indent
			= indent ++ "switch (r) {\n" ++
			cases def ++
			indent ++ "\tdefault: {\n" ++
			indent ++ "\t\tthrow new AssertionError(\"ERROR: Unhandeled semantic rule - \" + r +\".\");\n" ++
			indent ++ "\t}\n" ++
			indent ++ "}"
			where
				-- | cases. Maps all child nodes of a definition AST to code.
				cases :: Definition -> String
				cases def
					= concatMap
						(\p ->
							let childCalls 
									= mapPreOrder3
										(\pos n -> "." ++ childCallLabel pos ++ "()")
										(\path n -> genPreCode path n)
										(\path n -> genPostCode path n)
										(getNode p)
								in
							indent ++ "\tcase " ++ getRuleLabel p ++ ": {\n" ++
							nodeBody (getNode p) (childCalls) ++
							indent ++ "\t\tbreak;\n" ++
							indent ++ "\t}\n")
						(getProds def)
					where
						-- | nodeBody. src/Ast/Node contains the data structure of
						--		a node. Such is filled up with various information like:
						--			- if it is a T or Nt
						--			- semantic actions which can be defined at various places
						--			- bindings
						--			- child nodes (a.k.a. tree patterns)
						--		For each node we need to emit the information stored during parsing
						--		in the correct order in order to preserve the semantics intended by
						--		the user.
						nodeBody :: Node -> [(String, String, Node)] -> String
						nodeBody root nodes
							= genPreCode "" root ++			-- code which goes before processing children (evaluation methods)
							(concatMap						-- process children by calling the appropriate evaluation methods
								(\(path, code, n) -> code)
								(nodes)) ++
							genPostCode "" root				-- code which goes after processing children

						-- | genPreCode.
						genPreCode :: String -> Node -> String
						genPreCode path n
							= let ty = getTy n in
							-- First Semantic action 
							wrapUserCode "\t\t\t" (getSem1 n) ++
							-- Emit Eval and Binding TermTy
							(if (isJust ty)
								then
									-- If this is a Nt emit a function call to respective eval method
									(if (isNonTerm (fromJust ty))
										then
											let ret = (genFunRetVal (fromJust ty)) in
											"\t\t\t" ++ 
											-- If there are out parameters we assign the fun call to them
											(if (isJust ret)
												then (fst (fromJust ret)) ++ " " ++ (snd (fromJust ret)) ++ " = "
												else "") ++
											genFunCall (fromJust ty) path ++ ";\n"
										else "") ++
									-- Generate binding code if present
									(if (hasBinding (fromJust ty))
										then "\t\t\t" ++ genBinding (fromJust ty) path
										else "")
								else "") ++
							-- Second Semantic action
							wrapUserCode "\t\t\t" (getSem2 n)

						-- | genPostCode.
						genPostCode :: String -> Node -> String
						genPostCode path n
							= let ty = (getTy (getLink n)) in
							-- Third semantic action
							wrapUserCode "\t\t\t" (getSem3 n) ++
							-- Emit code for link evaluation
							(if (hasLink n && isJust ty)
								then 
									let ret = (genFunRetVal (fromJust ty)) in
									"\t\t\tif (n.link() != null) {\n" ++
									wrapUserCode "\t\t\t\t" (getSem5 n) ++
									(if (isJust ret)
										then
											"\t\t\t\t" ++ (fst (fromJust ret)) ++ " " ++ (snd (fromJust ret)) ++ " = "
										else
											"\t\t\t\t") ++
									(genFunCall (fromJust ty) ".link()") ++ ";\n" ++
									wrapUserCode "\t\t\t" (getSem6 n) ++ "\t\t\t}\n"
								else "") ++
							-- Fourth semantic action
							wrapUserCode "\t\t\t" (getSem4 n)

						-- | genBinding.
						genBinding :: TermTy -> String -> String
						genBinding ty path | hasBinding ty
							= "Node " ++ (show (getBinding ty)) ++ " = n" ++ path ++ ";\n"
						genBinding _ _ = ""

						-- | genFunRetVal. Given a NonTerm, this function gives the 
						--		the return value as a definition (e.g. List<String> list)
						--		in the form of a tuple where fst is the type and snd is
						--		the identifier.
						genFunRetVal :: TermTy -> Maybe (String, String)
						genFunRetVal ty | (isNonTerm ty)
							= let outattr 
									= map
										(\a -> (show (attrTy a), show (attrId a))) 
										(attrGetOut (getAttr ty))
								in
							case outattr of
								[] -> Nothing
								(ret:_) -> Just ret
						genFunRetVal _ = Nothing

						-- | genFunCall.
						genFunCall :: TermTy -> String ->  String
						genFunCall ty path | (isNonTerm ty)
							= let inattrs 
									= concatMap 
										(\a -> ", " ++ show (attrId a)) 
										(attrGetIn (getAttr ty)) 
								in
							let funname = tTyToEvalLabel ty in
							let nodearg = "n" ++ path in
							funname ++ "(" ++ nodearg ++ inattrs ++ ")"
						genFunCall _ _ = ""