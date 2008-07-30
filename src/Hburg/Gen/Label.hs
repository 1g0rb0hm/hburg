-----------------------------------------------------------------------------
-- |
-- Module      :  Label
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Non terminals defined in our tree pattern matching language, as well as
-- rule labels generated during the tiling phase, and others use these 
-- functions in order to guarantee consistent naming conventions.
-----------------------------------------------------------------------------

module Hburg.Gen.Label (
        -- * Types
        Label,
        -- * Functions
        termToEnum,
        prodToEnum,
        termToEval,
        childCall,
) where

{- unqualified imports  -}
import Hburg.Util (toUpper)

import Hburg.Ast.Term (TermClass(..))
import Hburg.Ast.Def (Definition)
import Hburg.Ast.Prod (Production)

{- qualified imports  -}

------------------------------------------------------------------------------------

type Label = String
type Suffix = String

{- | Non terminal labels. -}
termToEnum :: TermClass a => a -> Label
termToEnum t | (isTerminal t) = toUpper . show $ getId t
termToEnum t = "NT_"++ (toUpper . show $ getId t)

{- | Evaluation method labels. -}
termToEval :: TermClass a => a -> Label
termToEval t | (isTerminal t) =
  error "\nERROR: Can not generate EVAL label for Terminal: "++ (show $ getId t)
termToEval t = "eval_"++ (show $ getId t)

{- | Rule labels. -}
prodToEnum :: Definition -> Production -> Suffix -> Label
prodToEnum def prod suffix =
  "R_"++
  (toUpper . show $ getId def) ++
  "_"++ (toUpper . show $ getId prod) ++
  "_"++ suffix

{- | Labels for accessing child nodes:
      * 'n.child0()' or 'n.child1()' for left and right child -}
childCall :: Show a => a -> String
childCall str = "child"++ (show str)

------------------------------------------------------------------------------------