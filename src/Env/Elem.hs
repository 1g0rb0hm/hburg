-----------------------------------------------------------------------------
-- |
-- Module      :  Elem
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Am Elem is defined as an existential type in order to hold heterogeneous
-- values which then get stuffed into an Environment (Env).
-----------------------------------------------------------------------------

module Env.Elem (
        -- * Classes
        ElemClass(..),ElemType(..),
        -- * Types
        Elem,
        -- * Functions
        envElem,
    ) where
-----------------------------------------------------------------------------

data ElemType
    = EUnknown
    | EDef
    | EOp
    | EIdent
    | ETerm
    | ENonTerm
    deriving (Eq)

instance Show ElemType where
    show EUnknown = "Unknown"
    show EDef = "Definition"
    show EOp = "Operator"
    show EIdent = "Ident"
    show ETerm = "Term"
    show ENonTerm = "NonTerm"

-- | Elements in an ElemClass must implement the following
class (Eq a, Ord a, Show a) => ElemClass a where
    -- | uniqueley identifies an Element
    elemId :: a -> String
    elemId a = elemShow a
    -- | converts an element into a string
    elemShow :: a -> String
    -- | Env can hold heterogeneous elements. This function
    --    should be used to convey which element it is.
    elemType :: a -> ElemType
    elemType _ = EUnknown
    -- | every element in the environment must be declared at some
    --    point and thus must reside on a line and column
    elemL :: a -> Int
    elemC :: a -> Int

-- | Use 'existential type' in order to be able to package heterogenous values
--    together with a bunch of functions that manipulate them, and then treat that
--    collection of packages in a uniform manner. In our case we want to stick different
--    'things' into Envs, and all these 'things' implement ElemClass functions.
--    This is how it would work in the OO world and of course Haskell can do this as well ;-)
--    <http://www.haskell.org/haskellwiki/Existential_types>
data Elem
    = forall a. ElemClass a => Elem a

instance Eq Elem where
    (==) a1 a2 = elemId a1 == elemId a2

instance Ord Elem where
    compare a1 a2 = compare a1 a2

instance Show Elem where
    show (Elem e) = show e

-- | An ElemClass must implement the elemShow function which is used in the
--    environment in order to uniquely identify elements. The rest are default
--    dummy implementations.
instance ElemClass Elem where
    elemShow (Elem e) = elemShow e
    elemType (Elem e) = elemType e
    elemL (Elem e) = elemL e
    elemC (Elem e) = elemC e

-- | Constructor for creating Elems
envElem :: (ElemClass a) => a -> Elem
envElem a = Elem a