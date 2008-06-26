-----------------------------------------------------------------------------
-- |
-- Module      :  Ident
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
-- This module provides access to type and identifier names
-- used in the backend.
-----------------------------------------------------------------------------

module Gen.Ident (
  -- * Types
  Ident,
  -- * Functions
  new,
  pkgId,nId,ntId,rId,cId,
  nTy,ntTy,rTy,cTy,eTy,
  nN,ntN,rN,cN,eN,
) where

{- unqualified imports  -}

{- qualified imports  -}

-----------------------------------------------------------------------------

data Ident =
  Id { pkgid     :: String   -- package identifier
     , nid       :: String   -- node identifier
     , nn        :: String   -- name of node moduel
     , nty       :: String   -- node type
     , ntid      :: String   -- nt identifier
     , ntn       :: String   -- name of nt module
     , ntty      :: String   -- nt type
     , rid       :: String   -- rule identifier
     , rn        :: String   -- name of rule module
     , rty       :: String   -- rule type
     , cid       :: String   -- cost identifier
     , cn        :: String   -- name of cost module
     , cty       :: String   -- cost type
     , en        :: String   -- name of Entry module
     , ety       :: String } -- entry type

new :: String -> Ident
new p =
  let sep =
        if (not (null p) && last p /= '.')
          then "."
          else "" in
  Id { pkgid = p
     , nid = "_n"
     , nn  = "Node"
     , nty = p ++ sep ++ "Node"
     , ntid = "_nt"
     , ntn = "Nt"
     , ntty = p ++ sep ++ "Nt"
     , rid = "_r"
     , rn = "Rule"
     , rty = p ++ sep ++ "Rule"
     , cid = "_c"
     , cn = ""
     , cty = "int"
     , en = "Entry"
     , ety = p ++ sep ++ "Entry" }


pkgId,nId,ntId,rId,cId :: Ident -> String
pkgId = pkgid
nId = nid
ntId = ntid
rId = rid
cId = cid

nTy,ntTy,rTy,cTy,eTy :: Ident -> String
nTy = nty
ntTy = ntty
rTy = rty
cTy = cty
eTy = ety

nN,ntN,rN,cN,eN :: Ident -> String
nN = nn
ntN = ntn
rN = rn
cN = cn
eN = en

-----------------------------------------------------------------------------