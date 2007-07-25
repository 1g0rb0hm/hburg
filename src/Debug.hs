-----------------------------------------------------------------------------
-- |
-- Module      :  Debug
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- @TODO: Write short summary
-- 
--
-----------------------------------------------------------------------------

module Debug (
		Debug(..),
	) where

------------------------------------------------------------------------------------

class (Show a) => Debug a where
	debug :: a -> String