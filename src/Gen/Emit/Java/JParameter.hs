-----------------------------------------------------------------------------
-- |
-- Module      :  JParameter
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- Java parameter type.
--
--
-----------------------------------------------------------------------------

module Gen.Emit.Java.JParameter (
		-- * Introduction
		-- $intro
		JParameter,
		-- *  Construction
		-- $construction
		new, newFromList,
		-- *  Operations on parameters
		-- $parameter operations
		getIdent, getType,
	) where


-----------------------------------------------------------------------------

type Type = String
type Ident = String

data JParameter
	= Parameter (Type, Ident)
	deriving (Eq)

instance Show JParameter where
	show (Parameter (ty,i)) | ty == "" = i
	show (Parameter (ty,i)) | i == "" = ty
	show (Parameter (ty,i)) = ty ++ " " ++ i
	
-- | new. Smart Constructor
new :: Type -> Ident -> JParameter
new ty ident = Parameter (ty, ident)

newFromList :: [(Type, Ident)] -> [JParameter]
newFromList ps = map (\(ty, i) -> new ty i) (ps)

getIdent :: JParameter -> Ident
getIdent (Parameter p) = snd p

getType :: JParameter -> Type
getType (Parameter p) = fst p