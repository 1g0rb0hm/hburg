-----------------------------------------------------------------------------
-- |
-- Module      :  Debug
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Data which can output debugging information about itself should implement
-- the Debug(..) class.
-----------------------------------------------------------------------------

module Debug (
        -- * Classes
        Debug(..),
    ) where

-----------------------------------------------------------------------------

class (Show a) => Debug a where
    debug :: a -> String