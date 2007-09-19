-----------------------------------------------------------------------------
-- |
-- Module      :  Parameter
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Java parameter type.
-----------------------------------------------------------------------------

module Gen.Emit.Java.Parameter (
        -- * Types
        Parameter,
        -- * Construction
        new, newFromList,
        -- * Functions
        getIdent, getType,
    ) where

-----------------------------------------------------------------------------

type Type = String
type Ident = String

data Parameter
    = Parameter (Type, Ident)
    deriving (Eq)

instance Show Parameter where
    show (Parameter (ty,i)) | ty == "" = i
    show (Parameter (ty,i)) | i == "" = ty
    show (Parameter (ty,i)) = ty ++ " " ++ i
    
-- | Constructor for building a new Parameter
new :: Type -> Ident -> Parameter
new ty ident = Parameter (ty, ident)

newFromList :: [(Type, Ident)] -> [Parameter]
newFromList ps = map (\(ty, i) -> new ty i) (ps)

getIdent :: Parameter -> Ident
getIdent (Parameter p) = snd p

getType :: Parameter -> Type
getType (Parameter p) = fst p