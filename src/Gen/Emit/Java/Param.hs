-----------------------------------------------------------------------------
-- |
-- Module      :  Param
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Represenation of Java Parameters (e.g. method parameters).
-----------------------------------------------------------------------------

module Gen.Emit.Java.Param (
  -- * Types
  Param,
  -- * Functions
  new, newFromList,
  getIdent, getType,
) where

{- unqualified imports  -}

{- qualified imports  -}

-----------------------------------------------------------------------------

type Type = String
type Ident = String

data Param = Param (Type, Ident)
  deriving (Eq)

instance Show Param where
  show (Param (ty,i)) | (ty == "") = i
  show (Param (ty,i)) | (i == "") = ty
  show (Param (ty,i)) = ty ++" "++ i

-- | Constructor for building a new Param
new :: Type -> Ident -> Param
new ty ident = Param (ty, ident)

newFromList :: [(Type, Ident)] -> [Param]
newFromList ps = map (\(ty, i) -> new ty i) (ps)

getIdent :: Param -> Ident
getIdent (Param p) = snd p

getType :: Param -> Type
getType (Param p) = fst p

-----------------------------------------------------------------------------