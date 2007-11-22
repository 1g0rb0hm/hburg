-----------------------------------------------------------------------------
-- |
-- Module      :  Method
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java method.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Method (
        -- * Types
        Method,
        -- * Construction
        new,
        -- * Functions
        getName,getRetTy,
        getParams,setComment,
        setIfaceDef,
    ) where

import Util (stringFoldr)

import Gen.Emit.Java.Modifier (Modifier)
import qualified Gen.Emit.Java.Comment as Comment (Comment, new)
import Gen.Emit.Java.Parameter (Parameter)
------------------------------------------------------------------------------------

type Type = String
type Body = String

data Method
    = Method {
        comment     :: Comment.Comment,-- ^ method comments
        modifier    :: Modifier,       -- ^ public|private|protected modifiers
        isStatic    :: Bool,
        isIface     :: Bool,
        retTy       :: Type,            -- ^ method return type
        name        :: String,          -- ^ method Identifier
        params      :: [Parameter],    -- ^ method parameters
        body        :: Body             -- ^ method body
    }

instance Eq Method where
    (==)
        (Method {retTy = ty1, name = i1, params = params1}) 
        (Method {retTy = ty2, name = i2, params = params2}) 
            = (((ty1 == ty2) && (i2 == i2)) && (params1 == params2))

instance Show Method where
    -- Interface definition.
    show m | (isIface m)
        = genMethodSig m ++";"

    -- Regular method.
    show m
        = genMethodSig m ++" {\n"++
        (body m) ++                            -- method body
        "\n} // END METHOD "++ (name m) ++"()"

-- | Constructor for building a Method.
new :: Modifier -> Bool -> Type -> String -> [Parameter] -> Body -> Method
new m static ty str ps b
    = Method {
        comment = Comment.new [],
        modifier = m,
        isStatic = static,
        isIface = False,
        retTy = ty,
        name = str,
        params = ps,
        body = b
    }

setComment :: Method -> Comment.Comment -> Method
setComment m c = m { comment = c }

getName :: Method -> String
getName m = name m

getRetTy :: Method -> Type
getRetTy m = retTy m

getParams :: Method -> [Parameter]
getParams m = params m

setIfaceDef :: Method -> Bool -> Method
setIfaceDef m bool = m { isIface = bool }

-- | genMethodSig. Generate method signature.
genMethodSig :: Method -> String
genMethodSig m
    = -- Method comments
    show (comment m) ++"\n"++
    -- Actual method
    show (modifier m) ++                     -- public|private|...
    (if (isStatic m) then " static " else " ") ++
    (retTy m) ++" "++                        -- return type
    (name m) ++" ("++                        -- method identifier
    (stringFoldr                            -- parameters
        (\x y -> x ++", "++ y)
        (map (\z -> show z) (params m))) ++")"