-----------------------------------------------------------------------------
-- |
-- Module      :  Label
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Non terminals defined in our tree pattern matching language, as well as
-- rule labels generated during the tiling phase, and others use these 
-- functions in order to guarantee consistent naming.
--
-----------------------------------------------------------------------------

module Gen.Emit.Label (
		-- * Introduction
		-- $intro
		Label,
		-- *  Operations on labels
		-- $labels operations
		tTyToEnumLabel, ntToEnumLabel,
		defToEnumLabel, defToEvalLabel,
		tTyToEvalLabel, prodToEnumLabel,
		childCallLabel,
	) where

import Util (stringToUpper)

import Ast.Nt (Nt, getIdent)
import Ast.TermTy (TermTy, TermTyClass(..))
import Ast.Def (Definition)
import Ast.Prod (Prod, getName)

------------------------------------------------------------------------------------

type Label = String
type Suffix = String

tTyToEnumLabel :: TermTy -> Label
tTyToEnumLabel t = "NT_" ++ stringToUpper (show (getId t))

ntToEnumLabel :: Nt -> Label
ntToEnumLabel nt = "NT_" ++ stringToUpper (show (getIdent nt))

defToEnumLabel :: Definition -> Label
defToEnumLabel d = "NT_" ++ stringToUpper (show (getId d))

prodToEnumLabel :: Definition -> Prod -> Suffix -> Label
prodToEnumLabel def prod suffix
	= "R_" ++ 
		stringToUpper (show (getId def)) ++ 
		"_" ++ stringToUpper (getName prod) ++
		"_" ++ suffix

-- | childCallLabel. Generate a name which accesses the child:
--		n.child0(); or n.child1(); for left and right child.
childCallLabel :: Int -> String
childCallLabel pos = "child" ++ (show pos)

defToEvalLabel :: Definition -> Label
defToEvalLabel d = "eval_" ++ (show (getId d))

tTyToEvalLabel :: TermTy -> Label
tTyToEvalLabel ty = "eval_" ++ (show (getId ty))

