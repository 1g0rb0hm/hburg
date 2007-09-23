-----------------------------------------------------------------------------
-- |
-- Module      :  Enums
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- The resulting code uses enumerations for non terminals and rule labels. This
-- module is responsible for the creation of such Enumerations. It also modifies
-- the definition AST since the Rule labels used during the tiling and evaluation
-- phase are assigned here.
--
-----------------------------------------------------------------------------

module Gen.Emit.Enums (
        -- * Functions
        genEnums,
    ) where

import Control.Monad.State

import Ast.Def (Definition, getProds, setProds)
import Ast.Prod (Production, setRuleLabel, setResultLabel)
import qualified Ast.Ir as Ir (Ir(..))

import Gen.Emit.Label (Label, prodToEnumLab, termToEnumLab)

import Gen.Emit.Class (JavaClass(..))
import Gen.Emit.Java.Class (Java, java)
import qualified Gen.Emit.Java.Enum as Enum (Enum, new)
import Gen.Emit.Java.Modifier (Modifier(..))
-----------------------------------------------------------------------------

type Package = String

-- | Generates all necessary enumerations.
genEnums :: Package -> Ir.Ir -> (Ir.Ir, [Java])
genEnums pkg ir
    = -- Generate Nt enmus
    let ntenums = Enum.new Public "NT" (map (\d -> termToEnumLab d) (Ir.definitions ir)) in
    -- Generate Rule enums - this operation also modifies definitions
    let (ir', rulenums) = genRuleEnums in
    -- Create the appropriate classes
    let ntEnumClass = setEnumClasses (java pkg "NT") [ntenums] in
    let ruleEnumClass = setEnumClasses (java pkg "RuleEnum") [rulenums] in
    (ir', [ntEnumClass, ruleEnumClass])
    where
        -- | Generates Java Enumeration for rules and store srule labels with productions.
        genRuleEnums :: (Ir.Ir, Enum.Enum)
        genRuleEnums
            = let (ndefs, labels)
                    = unzip
                        (map
                            (\d ->
                                let (prods, labs) = unzip (labelProds d (getProds d) 0) in
                                (setProds d prods, labs))
                            (Ir.definitions ir))
                in
            (ir { Ir.definitions = ndefs }, Enum.new Public "RuleEnum" (concat labels))
            where
                labelProds :: Definition -> [Production] -> Int -> [(Production, Label)]
                labelProds d [] _ = []
                labelProds d (p:ps) num
                    = let label = prodToEnumLab d p (show num) in
                    let prod = evalState
                                    (do
                                        p <- get
                                        put (setRuleLabel p label)
                                        p <- get
                                        put (setResultLabel p $ termToEnumLab d)
                                        get)
                                    (p)
                        in
                    (prod, label) : (labelProds d ps (succ num))