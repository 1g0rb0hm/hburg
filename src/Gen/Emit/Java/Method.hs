-----------------------------------------------------------------------------
-- |
-- Module      :  Method
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Representation of Java methods.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Method (
  -- * Types
  Method(..),
  -- * Functions
  new,
) where

{- unqualified imports  -}
import Util (stringFoldr)

import Gen.Emit.Java.Modifier (Modifier)
import Gen.Emit.Java.Param (Param)

{- qualified imports  -}

-----------------------------------------------------------------------------

type Type = String
type Body = String

data Method =
  Method { modifier    :: Modifier -- ^ public|private|protected modifiers
         , isStatic    :: Bool
         , isIface     :: Bool
         , retTy       :: Type     -- ^ method return type
         , name        :: String   -- ^ method Identifier
         , params      :: [Param]  -- ^ method parameters
         , body        :: Body}    -- ^ method body

instance Eq Method where
  (==) (Method {retTy = ty1, name = n1, params = p1})
       (Method {retTy = ty2, name = n2, params = p2}) =
    ((ty1 == ty2 && n1 == n2) && p1 == p2)

instance Show Method where
  -- Interface definition.
  show m | (isIface m) = getSignature m ++";"
  
  -- Regular method.
  show m =
    getSignature m ++" {\n"++
    -- method body
    (body m) ++
    "\n} // END METHOD "++ (name m) ++"()"

-- | Constructor for building a Method.
new :: Modifier -> Bool -> Type -> String -> [Param] -> Body -> Method
new m static ty str ps b =
  Method { modifier = m
         , isStatic = static
         , isIface = False
         , retTy = ty
         , name = str
         , params = ps
         , body = b}

-- | getSignature. Generate method signature.
getSignature :: Method -> String
getSignature m =
  show (modifier m) ++                -- public|private|...
  (if (isStatic m) then " static " else " ") ++
  (retTy m) ++" "++                   -- return type
  (name m) ++" ("++                   -- method identifier
  (stringFoldr                        -- parameters
    (\x y -> x ++", "++ y)
    (map (show) $ params m)) ++")"

-----------------------------------------------------------------------------