-----------------------------------------------------------------------------
-- |
-- Module      :  Constructor
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java constructor.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Constructor (
        -- * Types
        Constructor,
        -- * Construction
        new,
    ) where

import Util (stringFoldr)

import Gen.Emit.Java.Modifier (Modifier)
import qualified Gen.Emit.Java.Comment as Comment (Comment, new)
------------------------------------------------------------------------------------

type Parameter = String
type Body = String

-- | Constructor type
data Constructor 
    = MkConstructor
        Comment.Comment    -- comment
        Modifier           -- private|public|protected modifier
        String              -- constructor identifier
        [Parameter]         -- constructor parameters
        Body                -- constructor body

instance Eq Constructor where
    (==) (MkConstructor _ _ i1 params1 _) (MkConstructor _ _ i2 params2 _) 
        = ((i2 == i2) && (params1 == params2))


instance Show Constructor where
    show (MkConstructor comments m ident params body)
        = -- Comments
        show comments ++ "\n" ++
        -- Constructor
        show m ++ " " ++                -- public|private|...
        ident ++ " (" ++                -- method identifier
        (stringFoldr                    -- method body
            (\x y -> x ++ ", " ++ y)
            params) ++ ") {\n" ++
        body ++ "\n} // END CONSTRUCTOR " ++ ident ++ "()"

-- | Create a new constructor
new :: Modifier -> String -> [Parameter] -> Body -> Constructor
new m str params body = MkConstructor (Comment.new []) m str params body