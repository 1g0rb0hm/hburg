-----------------------------------------------------------------------------
-- |
-- Module      :  JConstructor
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java constructor.
-----------------------------------------------------------------------------

module Gen.Emit.Java.JConstructor (
        -- * Types
        JConstructor,
        -- * Construction
        new,
    ) where

import Util (stringFoldr)

import Gen.Emit.Java.JModifier (JModifier)
import qualified Gen.Emit.Java.JComment as Comment (JComment, new)
------------------------------------------------------------------------------------

type Parameter = String
type Body = String

-- | JConstructor type
data JConstructor 
    = MkConstructor
        Comment.JComment    -- comment
        JModifier           -- private|public|protected modifier
        String              -- constructor identifier
        [Parameter]         -- constructor parameters
        Body                -- constructor body

instance Eq JConstructor where
    (==) (MkConstructor _ _ i1 params1 _) (MkConstructor _ _ i2 params2 _) 
        = ((i2 == i2) && (params1 == params2))


instance Show JConstructor where
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

-- | Constructor for building a JConstructor
new :: JModifier -> String -> [Parameter] -> Body -> JConstructor
new m str params body = MkConstructor (Comment.new []) m str params body