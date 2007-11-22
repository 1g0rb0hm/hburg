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
        termToEnumLab, prodToEnumLab,
        termToEvalLab,
        childCallLab,
    ) where

import Util (stringToUpper)

import Ast.Term (TermClass(..))
import Ast.Def (Definition)
import Ast.Prod (Production)
------------------------------------------------------------------------------------

type Label = String
type Suffix = String

-- | Non terminal labels
termToEnumLab :: TermClass a => a -> Label
termToEnumLab t | (isTerminal t) = stringToUpper (show $ getId t)
termToEnumLab t = "NT_"++ stringToUpper (show $ getId t)

-- | evaluation method labels
termToEvalLab :: TermClass a => a -> Label
termToEvalLab t | (isTerminal t) = error "\nERROR: Can not generate EVAL label for Terminal: "++ (show (getId t))
termToEvalLab t = "eval_"++ (show $ getId t)


-- | Rule labels
prodToEnumLab :: Definition -> Production -> Suffix -> Label
prodToEnumLab def prod suffix
    = "R_"++
        stringToUpper (show $ getId def) ++
        "_"++ stringToUpper (show $ getId prod) ++
        "_"++ suffix

-- | Labels for accessing child nodes:
--      * 'n.child0()' or 'n.child1()' for left and right child
childCallLab :: Show a => a -> String
childCallLab str = "child"++ (show str)


