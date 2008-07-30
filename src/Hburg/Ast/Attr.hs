-----------------------------------------------------------------------------
-- |
-- Module      :  Attr
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module includes the necessary abstractions in order to allow
-- for an attribute grammar formalism in our tree pattern language.
-- It represents an attribute with its operations:
--    * e.g.:  nt <: (in|out) Type ident :>
-----------------------------------------------------------------------------

module Hburg.Ast.Attr (
  -- Types
  Attr, Ty, AttrTy(..),
  -- Functions
  new, ty, emptyTy,
  attrTy, attrId, attrIsOut, attrGetIn, attrGetOut,
  attrEqualInOut,
) where

{- unqualified imports  -}

{- qualified imports  -}
import qualified Hburg.Ast.Ident as Id (Ident)

------------------------------------------------------------------------------------

{- | Specifies whether the attribute is an input or an output attribute -}
data AttrTy =
  InAttr
  | OutAttr
  deriving (Eq)

{- | Attribute datatype -}
data Attr =
  Attr { ident  :: Id.Ident -- ^ attribute identifier
       , isOut  :: Bool     -- ^ is it an output type
       , aty    :: Ty}      -- ^ language specific type

instance Show AttrTy where
  show InAttr = "in"
  show OutAttr = "out"
    
{- | Types in the host language (e.g. Jave, C#, etc.) -}
data Ty =
  MkTy Id.Ident
  | MkEmptyTy

instance Eq Ty where
  (==) (MkTy i1) (MkTy i2) = (i1 == i2)
  (==) (MkEmptyTy) (MkEmptyTy) = True
  (==) _ _ = False

instance Show Ty where
  show (MkTy i) = show i
  show MkEmptyTy = ""

instance Eq Attr where
  (==) a1 a2 = ((isOut a1 == isOut a2) && (aty a1 == aty a2))

instance Show Attr where
  show a = show (aty a) ++" "++ show (ident a)

{- | Create new attribute -}
new :: Id.Ident -> AttrTy -> Ty -> Attr
new i InAttr t =
  Attr { ident = i
       , isOut = False
       , aty = t}
new i _ t =
  Attr { ident = i
       , isOut = True
       , aty = t}

{- | Create attribute type -}
ty :: Id.Ident -> Ty
ty i = MkTy i

{- | Create empty attribute type -}
emptyTy :: Ty
emptyTy = MkEmptyTy

attrId :: Attr -> Id.Ident
attrId a = ident a

attrIsOut :: Attr -> Bool
attrIsOut a = isOut a

attrTy :: Attr -> Ty
attrTy a = aty a

attrGetIn :: [Attr] -> [Attr]
attrGetIn attrs = filter (\a -> not (isOut a)) (attrs)

attrGetOut :: [Attr] -> [Attr]
attrGetOut attrs = filter (isOut) (attrs)

attrEqualInOut :: [Attr] -> [Attr] -> Bool
attrEqualInOut a1 a2 | (length a1 == length a2)
    = False `notElem` [(isOut (fst x) == isOut (snd x)) | x <- zip a1 a2]
attrEqualInOut _ _ = False

------------------------------------------------------------------------------------