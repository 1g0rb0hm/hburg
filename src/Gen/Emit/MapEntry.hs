-----------------------------------------------------------------------------
-- |
-- Module      :  MapEntry
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Since Java does not support tuples, it is necessary to wrap up data (which one
-- would normally put into a Tuple) in a class. This is what this module emits,
-- a class which holds two values, the cost and the rule number.
--
-- Each AST node in the target language has a map mapping non terminals
-- defined in the tree pattern matching language grammar to costs 
-- and values, it looks something like:
--        * EnumMap<NT, MapEntry> table = new EnumMap<NT, MapEntry>();
--
-- Thus a MapEntry serves as a Tuple holding cost und rule number.
-----------------------------------------------------------------------------

module Gen.Emit.MapEntry (
        -- * Functions
        genMapEntry,
    ) where

import Gen.Emit.Class (JavaClass(..))
import Gen.Emit.Java.Class (Java, java)
import Gen.Emit.Java.Modifier (Modifier(..))
import qualified Gen.Emit.Java.Variable as Variable (new)
import qualified Gen.Emit.Java.Constructor as Constructor (new)
-----------------------------------------------------------------------------

type Package = String

-- | Generates 'MapEntry' Java class
genMapEntry :: Package -> Java
genMapEntry pkg
    = let c1 = Constructor.new Public "MapEntry" [] [] in
    let c2 = Constructor.new Public "MapEntry" ["int c", "RuleEnum r"]
            ( "\tthis.cost = c;\n\tthis.rule = r;")
        in
    let v1 = Variable.new Public False "int" "cost" "" in
    let v2 = Variable.new Public False "RuleEnum" "rule" "" in
    let j0 = setConstructors (java pkg "MapEntry") [c1, c2] in
    setVariables j0 [v1, v2]