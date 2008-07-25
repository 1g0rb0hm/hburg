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
import Text.PrettyPrint

import Gen.Document (Document(..))
import Gen.Emit.Java.Modifier (Modifier)
import Gen.Emit.Java.Param (Param)

{- qualified imports  -}

-----------------------------------------------------------------------------

type Type = String
type Body = Doc

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
  show m = render . toDoc $ m

instance Document Method where
  -- Interface definition.
  toDoc m | (isIface m) = getSignature m <> semi
  -- Regular method.
  toDoc m =
      getSignature m
      <+>
        lbrace -- method body
          $+$ (nest 2 $ body $ m) $+$
        rbrace
      <+> text "// END METHOD"
      <+> text (name m)

-- | getSignature. Generate method signature.
getSignature :: Method -> Doc
getSignature m =
  (text . show $ modifier m)    -- public|private|...
  <+> (if (isStatic m)
    then text "static"
    else empty)
  <+> text (retTy m)            -- return type
  <+> text (name  m)            -- method identifier
  <>  lparen <>                 -- parameters
        (if (null $ params m)
          then empty
          else
            foldr1
              (\p1 p2 -> p1 <> comma <+> p2)
              (map (toDoc) $ params m)) <>
      rparen

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

-----------------------------------------------------------------------------