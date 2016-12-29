-----------------------------------------------------------------------------
-- |
-- Module      :  Closure
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- In order to be able to generate code that fires chain rules we want
-- to know which non-terminals fire chain rules, and for a given non-terminal
-- that fires a chain rule we want to know the set of other non-terminals
-- that can be derived via those chain rules. This module contains the data
-- structure (i.e. Closure) that holds such information.
--
-----------------------------------------------------------------------------

module Hburg.Ast.Closure (
  -- * Types
  Closure, Label(..),
  -- * Functions
  closure, empty,
  fromLabels, toLabels,
) where

{- unqualified imports  -}
import Data.Maybe (isJust, fromJust)

{- qualified imports  -}
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Hburg.Ast.Term as Term (TermClass(..))
import qualified Hburg.Ast.Def as Def (Definition, getProds)
import qualified Hburg.Ast.Cost as C (Cost)
import qualified Hburg.Ast.Prod as P (getResultLabel, getRuleLabel, getCost)
import qualified Hburg.Gen.Label as L (termToEnum)

-----------------------------------------------------------------------------

{- | Non-terminal that fires chain rules -}
type Key = String

{- | This record encodes a a chain rules -}
data Label =
  L { toL   :: String     -- ^ to label
    , ruleL :: String     -- ^ rule label
    , cost  :: C.Cost }   -- ^ cost
  deriving(Eq,Ord)

{- | A closure is a map containing the non-terminal that fires a chain rule as key
     and contains a set of chain rules that are fired by the non-terminal as value -}
data Closure = Closure (M.Map Key (S.Set Label))

{- | Calculate necessary values for target code closure function (i.e. final label()
     method) that fires chain rules. -}
closure :: [Def.Definition] -> Closure
closure defs =
  Closure
    (foldr
      (\p m ->
        M.alter
          (\a -> -- alter function
            let r = L { toL = P.getResultLabel p
                      , ruleL = P.getRuleLabel p
                      , cost = P.getCost p}
            in
            if (isJust a)
                then Just $ S.insert r (fromJust a)
                else Just $ S.singleton r)
          (L.termToEnum p) -- alter at key
          m)  -- map to alter
      M.empty -- start with empty map
      -- filter out productions that form chain rules
      (concatMap (\d -> filter (Term.isNonTerminal) (Def.getProds d)) defs))

{- | Given a closure, return non-terminals that derive other non-terminals. -}
fromLabels :: Closure -> [Key]
fromLabels (Closure m) = M.keys m

{- | Given a non-terminal as key and a closure, return Set of non-terminals it derives -}
toLabels :: Key -> Closure -> S.Set Label
toLabels key (Closure m)
    = if (M.member key m)
        then m M.! key
        else S.empty

{- | Are there any closures? -}
empty :: Closure -> Bool
empty (Closure m) = M.null m

-----------------------------------------------------------------------------