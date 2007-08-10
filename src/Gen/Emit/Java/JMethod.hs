-----------------------------------------------------------------------------
-- |
-- Module      :  JMethod
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java method.
-----------------------------------------------------------------------------

module Gen.Emit.Java.JMethod (
        -- * Types
        JMethod,
        -- * Construction
        new,
        -- * Functions
        getName,getRetTy,
        getParams,setComment,
        setIfaceDef,
    ) where

import Util (stringFoldr)

import Gen.Emit.Java.JModifier (JModifier)
import qualified Gen.Emit.Java.JComment as Comment (JComment, new)
import Gen.Emit.Java.JParameter (JParameter)
------------------------------------------------------------------------------------

type Type = String
type Body = String

data JMethod
    = Method {
        comment     :: Comment.JComment,-- ^ method comments
        modifier    :: JModifier,       -- ^ public|private|protected modifiers
        isStatic    :: Bool,
        isIface     :: Bool,
        retTy       :: Type,            -- ^ method return type
        name        :: String,          -- ^ method Identifier
        params      :: [JParameter],    -- ^ method parameters
        body        :: Body             -- ^ method body
    }

instance Eq JMethod where
    (==)
        (Method {retTy = ty1, name = i1, params = params1}) 
        (Method {retTy = ty2, name = i2, params = params2}) 
            = (((ty1 == ty2) && (i2 == i2)) && (params1 == params2))

instance Show JMethod where
    -- Interface definition.
    show m | (isIface m)
        = genMethodSig m ++ ";"

    -- Regular method.
    show m
        = genMethodSig m ++ " {\n" ++
        (body m) ++                            -- method body
        "\n} // END METHOD " ++ (name m) ++ "()"

-- | Constructor for building a JMethod.
new :: JModifier -> Bool -> Type -> String -> [JParameter] -> Body -> JMethod
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

setComment :: JMethod -> Comment.JComment -> JMethod
setComment m c = m { comment = c }

getName :: JMethod -> String
getName m = name m

getRetTy :: JMethod -> Type
getRetTy m = retTy m

getParams :: JMethod -> [JParameter]
getParams m = params m

setIfaceDef :: JMethod -> Bool -> JMethod
setIfaceDef m bool = m { isIface = bool }

-- | genMethodSig. Generate method signature.
genMethodSig :: JMethod -> String
genMethodSig m
    = -- Method comments
    show (comment m) ++ "\n" ++
    -- Actual method
    show (modifier m) ++                     -- public|private|...
    (if (isStatic m) then " static " else " ") ++
    (retTy m) ++ " " ++                        -- return type
    (name m) ++ " (" ++                        -- method identifier
    (stringFoldr                            -- parameters
        (\x y -> x ++ ", " ++ y)
        (map (\z -> show z) (params m))) ++ ")"