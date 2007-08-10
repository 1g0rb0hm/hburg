-----------------------------------------------------------------------------
-- |
-- Module      :  Incl
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Target code specified in the include section of the tree pattern
-- matching language.
-----------------------------------------------------------------------------

module Ast.Incl (
        -- * Types
        Include,
        -- * Construction
        new,
    ) where

import Ast.Code(Code)
------------------------------------------------------------------------------------

data Include = Incl Code

new :: Code -> Include
new c = Incl c

instance Show Include where
    show (Incl c) = show c