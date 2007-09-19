-----------------------------------------------------------------------------
-- |
-- Module      :  Cost
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
-- Costs for productions can either be static (i.e. an integer number), or they
-- can be dynamic and include arbitrary expressions.
-----------------------------------------------------------------------------

module Ast.Cost (
		-- * Types
        Cost,
		-- * Construction
        static, dynamic,
	) where

import Ast.Code (Code)
-----------------------------------------------------------------------------

-- | Cost Type
data Cost 
    = Static Int
    | Dynamic Code
    deriving (Eq)

instance Show Cost where
    show (Static i) = show i
    show (Dynamic c) = show c

-- | Create static cost consisting of a constant.
static :: Int -> Cost
static i = (Static i)

-- | Create dynamic cost consisting of an arbitrary expression.
dynamic :: Code -> Cost
dynamic c = (Dynamic c)