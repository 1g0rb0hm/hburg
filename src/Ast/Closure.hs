-----------------------------------------------------------------------------
-- |
-- Module      :  Closure
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- @TODO: Write short summary
-- 
--
-----------------------------------------------------------------------------

module Ast.Closure (
		-- * Types
        Closure, Label(..),
		-- * Functions
        closure, empty,
        fromLabels, toLabels,
	) where

import Maybe (isJust, fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Ast.Term as Term (TermClass(..))
import qualified Ast.Def as Def (Definition, getProds)
import qualified Ast.Cost as C (Cost)
import qualified Ast.Prod as P (getResultLabel, getRuleLabel, getCost)
import qualified Gen.Emit.Label as L (termToEnumLab)


-----------------------------------------------------------------------------

type Key = String

data Label = L { toL   :: String     -- ^ to label
               , ruleL :: String     -- ^ rule label
               , cost  :: C.Cost }   -- ^ cost
        deriving(Eq,Ord)

-- | A closure consists of three labels and a cost
data Closure = Closure (M.Map Key (S.Set Label))

-- | Calculate necessary values for target code closure function.
closure :: [Def.Definition] -> Closure
closure defs
    = Closure
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
                    (L.termToEnumLab p) -- alter at key
                    m) -- map to alter
            M.empty -- start with empty map
            (concatMap (\d -> filter (Term.isNonTerminal) (Def.getProds d)) defs))

-- | Given a closure, return non-terminals that derive other non-terminals
fromLabels :: Closure -> [Key]
fromLabels (Closure m) = M.keys m

-- | Given a non-terminal as key and a closure, return Set of non-terminals it derives
toLabels :: Key -> Closure -> S.Set Label
toLabels key (Closure m)
    = if (M.member key m)
        then m M.! key
        else S.empty

-- | Are there any closures?
empty :: Closure -> Bool
empty (Closure m) = M.null m