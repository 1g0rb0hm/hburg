-----------------------------------------------------------------------------
-- |
-- Module      :  Var
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Representation of Java Variables
-----------------------------------------------------------------------------

module Gen.Emit.Java.Var (
  -- * Types
  Var,
  -- * Functions
  new,
) where

{- unqualified imports  -}
import Text.PrettyPrint

import Gen.Document (Document(..))
import Gen.Emit.Java.Modifier (Modifier)

{- qualified imports  -}

------------------------------------------------------------------------------------

type Type = String
type Constructor = String

data Var =
  Var { modifier  :: Modifier     -- private|public|protected modifier
      , isStatic  :: Bool         -- is it static?
      , type'     :: Type         -- variable type
      , ident     :: String       -- variable identifier
      , construct :: Constructor} -- how to construct the variable (e.g. new EnumSet.of(blablabla))
  deriving (Eq)

instance Show Var where
    show v = render . toDoc $ v

instance Document Var where
  toDoc v = toDoc (modifier v)    -- modifier
    <+> (if (isStatic v)          -- is it statis?
      then text "static"
      else empty)
    <+> (text $ type' v)          -- type
    <+> (text $ ident v)          -- identifier
    <+> (if (null $ construct v)  -- do we need to construct it?
      then semi
      else equals <+> (text . construct $ v) <> semi)

-- | Constructor for building a new Var
new :: Modifier -> Bool -> Type -> String -> Constructor -> Var
new m stat ty i con =
  Var { modifier = m
      , isStatic = stat
      , type' = ty
      , ident = i
      , construct = con}

------------------------------------------------------------------------------------