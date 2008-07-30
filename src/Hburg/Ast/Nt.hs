-----------------------------------------------------------------------------
-- |
-- Module      :  Nt
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Representation of a non terminal in our tree pattern matching grammar.
-----------------------------------------------------------------------------

module Hburg.Ast.Nt (
  -- Types
  Nt,
  -- * Functions
  new,
  getIdent, getAttr, getBinding, hasBinding,
) where

{- unqualified imports  -}

import Hburg.Ast.Attr(Attr, attrIsOut)

{- qualified imports  -}
import qualified Hburg.Ast.Ident as Id (Ident)
import qualified Hburg.Ast.Bind as B (Binding, hasBinding)

import qualified Hburg.Csa.Elem as E (ElemClass(..),ElemType(ENonTerm))

------------------------------------------------------------------------------------

{- | Non Terminal Type -}
data Nt =
  Nt
    Id.Ident    -- Id identifying this non terminal (e.g. reg, stmt)
    B.Binding   -- binding for this non terminal (e.g. reg r1)
    [Attr]      -- list of attributes for this non terminal

instance Eq Nt where
  (==) (Nt i1 _ at1) (Nt i2 _ at2) = ((i1 == i2) && (at1 == at2))

instance Ord Nt where
  compare nt1@(Nt i1 _ at1) nt2@(Nt i2 _ at2) =
    if (nt1 == nt2)
      then EQ
      else
        if (i1 /= i2)
          then
            if (length (show i1) < length (show i2))
              then LT
              else GT
          else
            if (length (show at1) < length (show at2))
              then LT
              else GT

instance Show Nt where
  show (Nt i b []) = "Nt["++ (show i) ++
    (if (B.hasBinding b)
      then " "
      else "") ++
    show b ++"]"
  
  show (Nt i b atts) = "Nt["++ (show i) ++
    (if (B.hasBinding b)
      then " "
      else "") ++
    show b ++"]<."++
    (foldr1
      (\x y -> x ++", "++ y)
      (map (show) atts))  ++".>"

{- | A Non Terminal is also an Elem since we need to be able to type check it -}
instance E.ElemClass Nt where
  elemShow (Nt i _ []) = show i
  elemShow (Nt i _ atts)  =
    show i ++"<."++
    (foldr1
      (\x y -> x ++" param, "++ y ++" param")
      (map (\z -> show (attrIsOut z)) atts)) ++".>"
  elemType _ = E.ENonTerm
  elemL (Nt i _ _) = E.elemL i
  elemC (Nt i _ _) = E.elemC i

{- | Construct a non terminal -}
new :: Id.Ident -> B.Binding -> [Attr] -> Nt
new i b attr = Nt i b attr

getIdent:: Nt -> Id.Ident
getIdent(Nt i _ _) = i

getAttr :: Nt -> [Attr]
getAttr (Nt _ _ attr) = attr

getBinding :: Nt -> B.Binding
getBinding (Nt _ bind _ ) = bind

hasBinding :: Nt -> Bool
hasBinding (Nt _ bind _) = B.hasBinding bind

------------------------------------------------------------------------------------