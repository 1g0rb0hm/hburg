-----------------------------------------------------------------------------
-- |
-- Module      :  Attr
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- This module includes the necessary abstractions in order to allow
-- for an attribute grammar formalism in our tree pattern language.
-- It represents an Attribute with its operations:
--	* e.g.:  nt <: (in|out) Type ident :>
--
-----------------------------------------------------------------------------

module Ast.Attr (
		-- * Introduction
		-- $intro
		Attr, Ty, AttrTy(..),
        -- *  Construction
        -- $construction
		new, ty, emptyTy,
        -- *  Operations on attributes
        -- $attribute operations
		attrTy, attrId, attrIsOut, attrGetIn, attrGetOut,
		attrEqualInOut,
	) where

import Debug (Debug(..))

import qualified Ast.Ident as Id (Ident)

------------------------------------------------------------------------------------

-- | Specifies whether the attribute is an input or an output attribute
data AttrTy = InAttr | OutAttr deriving (Eq)

instance Show AttrTy where
	show InAttr = "in"
	show OutAttr = "out"
	
-- | Types in the host language used in Attribute definitions
data Ty
	= MkTy Id.Ident
	| MkEmptyTy

instance Eq Ty where
	(==) (MkTy i1) (MkTy i2) = (i1 == i2)
	(==) (MkEmptyTy) (MkEmptyTy) = True
	(==) _ _ = False

instance Show Ty where
	show (MkTy i) = show i
	show MkEmptyTy = ""

-- | Attribute datatype used in NonTerminals
data Attr 
	= Attr { ident	:: Id.Ident	-- attribute identifier
		   , isOut	:: Bool		-- is it an output type
		   , aty    :: Ty		-- the language specific type
		   }

instance Eq Attr where
	(==) a1 a2 = ((isOut a1 == isOut a2) && (aty a1 == aty a2))

instance Show Attr where
	show a = show (aty a) ++ " " ++ show (ident a)

instance Debug Attr where
	debug a = show (isOut a) ++ " " ++ show (aty a) ++ " " ++ show (ident a)

-- | new. Create new Attribute
new :: Id.Ident -> AttrTy -> Ty -> Attr
new i InAttr t = Attr { ident = i
 				  	  , isOut = False
 				  	  , aty = t}
new i _ t = Attr { ident = i
 				 , isOut = True
 				 , aty = t}

-- | ty. Create Attribute Type
ty :: Id.Ident -> Ty
ty i = MkTy i

-- | emptyTy. Create empty Attribute Type
emptyTy :: Ty
emptyTy = MkEmptyTy

--
-- Getters and Setters
--
attrId :: Attr -> Id.Ident
attrId a  = ident a

attrIsOut :: Attr -> Bool
attrIsOut a = isOut a

attrTy :: Attr -> Ty
attrTy a = aty a

attrGetIn :: [Attr] -> [Attr]
attrGetIn attrs = filter (\a -> not (isOut a)) (attrs)

attrGetOut :: [Attr] -> [Attr]
attrGetOut attrs = filter (\a -> isOut a) (attrs)

attrEqualInOut :: [Attr] -> [Attr] -> Bool
attrEqualInOut [] [] = True
attrEqualInOut [] _ = False
attrEqualInOut _ [] = False
attrEqualInOut (a1:rest1) (a2:rest2)
	= if isOut a1 == isOut a2
		then attrEqualInOut rest1 rest2
		else False