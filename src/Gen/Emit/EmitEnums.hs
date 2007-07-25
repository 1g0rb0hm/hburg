-----------------------------------------------------------------------------
-- |
-- Module      :  EmitEnums
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- The resulting code uses enumerations for non terminals and rule labels. This
-- module is responsible for the creation of such Enumerations. It also modifies
-- the definition AST since the Rule labels used during the tiling and evaluation
-- phase are assigned here.
--
-----------------------------------------------------------------------------

module Gen.Emit.EmitEnums (
		genEnums,
	) where


import Ast.Def (Definition, getProds, setProds)

import Ast.Prod (Prod, setRuleLabel, setResultLabel)

import Gen.Emit.Label (Label, prodToEnumLabel, defToEnumLabel)

import Gen.Emit.JavaClass (JavaClass(..))
import Gen.Emit.Java.Java (Java, java)
import qualified Gen.Emit.Java.JEnum as Enum (JEnum, new)
import Gen.Emit.Java.JModifier (JModifier(..))


------------------------------------------------------------------------------------

type Package = String

-- | genEnums. Generates all necessary enumerations.
genEnums :: Package -> [Definition] -> ([Definition], [Java])
genEnums pkg defs
	= let ntenums = genNtEnums defs in				-- 1. Generate NT Enumeration
	let (ndefs, rulenums) = genRuleEnums defs in	-- 2. Generate RuleEnum Enumeration
	let jNtEnum = jSetEnumClasses (java pkg "NT") [ntenums] in
	let jRuleEnum = jSetEnumClasses (java pkg "RuleEnum") [rulenums] in
	(ndefs, [jNtEnum, jRuleEnum])


-- | genNtEnums. Generate Enum for NT's
genNtEnums :: [Definition] -> Enum.JEnum
genNtEnums defs
	= Enum.new Public "NT" (map (\d -> defToEnumLabel d) defs)

-- | genRuleEnums. Generate Enum for Rules and store rule labels
--		with productions.
genRuleEnums :: [Definition] -> ([Definition], Enum.JEnum)
genRuleEnums defs
	= let (ndefs, labels)
			= unzip (map
						(\d -> let (prods, labs)  
								= unzip (labelProds d (getProds d) 0) in
								(setProds d prods, labs))
						(defs))
		in
	(ndefs, Enum.new Public "RuleEnum" (concat labels))
	where
		labelProds :: Definition -> [Prod] -> Int -> [(Prod, Label)]
		labelProds d [] _ = []
		labelProds d (p:ps) num
			= let label = prodToEnumLabel d p (show num) in
			let prod = setResultLabel (setRuleLabel p label) (defToEnumLabel d) in
			(prod, label) : (labelProds d ps (succ num))

