-----------------------------------------------------------------------------
-- |
-- Module      :  Label
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Non terminals defined in our tree pattern matching language, as well as
-- rule labels generated during the tiling phase, and others use these 
-- functions in order to guarantee consistent naming.
--
-----------------------------------------------------------------------------

module Gen.Emit.Label (
        -- * Types
        Label,
        -- * Functions
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

-- | Generate label for TermTy
tTyToEnumLabel :: TermTy -> Label
tTyToEnumLabel t = "NT_" ++ stringToUpper (show (getId t))

-- | Generate label for Nt
ntToEnumLabel :: Nt -> Label
ntToEnumLabel nt = "NT_" ++ stringToUpper (show (getIdent nt))

-- | Generate label for Definition
defToEnumLabel :: Definition -> Label
defToEnumLabel d = "NT_" ++ stringToUpper (show (getId d))

-- | Generate label for Production
prodToEnumLabel :: Definition -> Prod -> Suffix -> Label
prodToEnumLabel def prod suffix
    = "R_" ++ 
        stringToUpper (show (getId def)) ++ 
        "_" ++ stringToUpper (getName prod) ++
        "_" ++ suffix

-- | Generates node child access label:
--      * 'n.child0()' or 'n.child1()' for left and right child
childCallLabel :: Int -> String
childCallLabel pos = "child" ++ (show pos)

-- | Generate label for evaluation methods given a definition
defToEvalLabel :: Definition -> Label
defToEvalLabel d = "eval_" ++ (show (getId d))

-- | Generate label for evaluation methods given a TermTy
tTyToEvalLabel :: TermTy -> Label
tTyToEvalLabel ty = "eval_" ++ (show (getId ty))

