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
        Closure(..),
		-- * Functions
        closure
	) where

import qualified Ast.Def as Def (Definition, getClosures)
import qualified Ast.Cost as C (Cost)
import qualified Ast.Prod as P (getResultLabel, getRuleLabel, getCost)
import qualified Gen.Emit.Label as L (termToEnumLab)


-----------------------------------------------------------------------------

type Label = String

-- | A closure consists of three labels and a cost
data Closure
  = Closure   { fromL :: Label      -- ^ from label
              , toL   :: Label      -- ^ to label
              , ruleL :: Label      -- ^ rule label
              , cost  :: C.Cost }   -- ^ cost
  deriving (Eq, Show)

-- | Calculate necessary values for target code closure function.
closure :: [Def.Definition] -> [Closure]
closure [] = []
closure defs
    = map
        (\p -> Closure { fromL = L.termToEnumLab p
                       , toL = P.getResultLabel p
                       , ruleL = P.getRuleLabel p
                       , cost = P.getCost p } )
        (concatMap (Def.getClosures) defs)