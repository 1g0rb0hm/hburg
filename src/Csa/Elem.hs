-----------------------------------------------------------------------------
-- |
-- Module      :  Elem
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Am Elem is defined as an existential type in order to hold heterogeneous
-- values.
-----------------------------------------------------------------------------

module Csa.Elem (
  -- * Types
  ElemClass(..),ElemType(..),
  Elem,
  -- * Functions
  new,
) where

{- qualified imports  -}

{- unqualified imports  -}

-----------------------------------------------------------------------------

data ElemType =
  EUnknown
  | EDef
  | EProd
  | EOp
  | EIdent
  | ETerm
  | ENonTerm
  deriving (Eq)

instance Show ElemType where
  show EUnknown = "Unknown"
  show EDef = "Definition"
  show EProd = "Production"
  show EOp = "Operator"
  show EIdent = "Ident"
  show ETerm = "Term"
  show ENonTerm = "NonTerm"

{- | Elements in an ElemClass must implement the following -}
class (Eq a, Ord a,  Show a) => ElemClass a where
  -- | uniqueley identifies an Element
  elemId :: a -> String
  elemId a = elemShow a
  -- | converts an element into a string
  elemShow :: a -> String
  -- | convey the type of this element
  elemType :: a -> ElemType
  elemType _ = EUnknown
  -- | convey at which line this element was defined
  elemL :: a -> Int
  -- | convey at which column this element was defined
  elemC :: a -> Int

{- | We want to stick different 'things' into contexts, and all these
     'things' implement ElemClass functions. First we used the following Existential Type
     definition (<http://www.haskell.org/haskellwiki/Existential_types>):
        * data Elem = forall a. ElemClass a => Elem a
     But with the advent of GADTs we can write the following: -}
data Elem
  where Elem :: ElemClass a => a -> Elem

instance Eq Elem where
  (==) a1 a2 = elemId a1 == elemId a2

instance Ord Elem where
  compare a1 a2 = compare a1 a2

instance Show Elem where
  show (Elem e) = show e

instance ElemClass Elem where
  elemShow (Elem e) = elemShow e
  elemType (Elem e) = elemType e
  elemL (Elem e) = elemL e
  elemC (Elem e) = elemC e

{- | Constructor for creating Elems -}
new :: (ElemClass a) => a -> Elem
new a = Elem a

-----------------------------------------------------------------------------