-----------------------------------------------------------------------------
-- |
-- Module      :  EmitClass
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Every class should know how to emit itself and where it belongs.
-----------------------------------------------------------------------------

module Gen.Emit.EmitClass (
        -- * Classes
        EmitClass(..),
    ) where

import System.FilePath.Posix (FilePath)
------------------------------------------------------------------------------------

-- | Whatever is an instance of Emit must obviously be able to emit itself
--    and to tell where it wants to emit itself
class EmitClass a where
    emit :: a -> String
    emitTo :: a -> FilePath
