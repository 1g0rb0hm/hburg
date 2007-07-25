-----------------------------------------------------------------------------
-- |
-- Module      :  EmitClass
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Every class should know how to emit itself and where it belongs.
-- 
--
-----------------------------------------------------------------------------

module Gen.Emit.EmitClass (
		-- * Introduction
		-- $intro
		EmitClass(..),
	) where

import System.FilePath.Posix (FilePath)

------------------------------------------------------------------------------------

-- | Whatever is an instance of Emit
--	must obviously be able to Emit itself
--	and to tell where it wants to emit itself
class EmitClass a where
	-- Emit yourself!
	emit :: a -> String
	-- Where shoud the output go (e.g. filename)
	emitTo :: a -> FilePath
