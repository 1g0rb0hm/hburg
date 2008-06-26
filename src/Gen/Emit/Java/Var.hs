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
import Gen.Emit.Java.Modifier (Modifier)

{- qualified imports  -}

------------------------------------------------------------------------------------

type Type = String
type Constructor = String

data Var =
  MkVar
    Modifier    -- private|public|protected modifier
    Bool        -- is it static?
    Type        -- variable type
    String      -- variable identifier
    Constructor -- how to construct the variable (e.g. new EnumSet.of(blablabla))
  deriving (Eq)

instance Show Var where
    show (MkVar modifier isStat ty ident constructor) =
      " "++ (show modifier) ++" "++           -- modifier
      (if (isStat) then "static " else " ") ++  -- is it static ?
      ty ++" "++                                -- type
      ident ++                                  -- identifier
      if (null constructor)
        then ";"
        else " = "++ constructor ++";"        -- how to construct it?

-- | Constructor for building a new Var
new :: Modifier -> Bool -> Type -> String -> Constructor -> Var
new m stat ty i con = MkVar m stat ty i con

------------------------------------------------------------------------------------