-----------------------------------------------------------------------------
-- |
-- Module      :  Op
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module represents an operator. Operators are defined in the operator
-- section of the tree pattern matching language. Operators need to be defined
-- for two reasons:
--        * CSA
--        * the ability to abstract away from the target language specific representation
--        of an operator and the name used in the tree pattern matching grammar
--            e.g.: ADD (: E_ADD :) would define the operator ADD which has a
--            target language specific definition, namely 'E_ADD'.
-----------------------------------------------------------------------------

module Hburg.Ast.Op (
  -- * Types
  Operator,
  -- * Functions
  op,opMap,opSem,
  opHasSem,opId,
) where

{- unqualified imports  -}

{- qualified imports  -}
import qualified Hburg.Ast.Ident as Id (Ident)
import qualified Hburg.Ast.Code as C (Code, new)

import qualified Hburg.Csa.Elem as E (ElemClass(..), ElemType(EOp))

-----------------------------------------------------------------------------

{- | Operator Definition -}
data Operator =
  OpMap Id.Ident C.Code
  | Op Id.Ident

instance Show Operator where
  show (OpMap i _) = show i
  show (Op i) = show i

instance Eq Operator where
  (==) (OpMap i1 _) (OpMap i2 _) = i1 == i2
  (==) (Op i1) (OpMap i2 _) = i1 == i2
  (==) (OpMap i1 _) (Op i2) = i1 == i2
  (==) (Op i1) (Op i2) = i1 == i2

instance Ord Operator where
  compare (OpMap i1 _) (OpMap i2 _) = compare i1 i2
  compare (Op i1) (OpMap i2 _) = compare i1 i2
  compare (OpMap i1 _) (Op i2) = compare i1 i2
  compare (Op i1) (Op i2) = compare i1 i2

instance E.ElemClass Operator where
  elemShow o = show (opId o)
  elemType _ = E.EOp
  elemL o = E.elemL (opId o)
  elemC o = E.elemC (opId o)

{- | Create Operator from Ident -}
op :: Id.Ident -> Operator
op t = Op t

{- | Create Operator from Ident and Code -}
opMap :: Id.Ident -> C.Code -> Operator
opMap t a  = OpMap t a

{- | retrieve semantic action possibly attached to operator -}
opSem :: Operator -> C.Code
opSem (OpMap _ a) = a
opSem (Op i) = C.new (show i)

{- | query if operator has a semantic action attached to it -}
opHasSem :: Operator -> Bool
opHasSem (OpMap _ m) = True
opHasSem _ = False

{- | retrieve operator token -}
opId :: Operator -> Id.Ident
opId (OpMap t _) = t
opId (Op t) = t

-----------------------------------------------------------------------------