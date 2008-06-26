-----------------------------------------------------------------------------
-- |
-- Module      :  Ir
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
-- This module contains an intermediate representation of a
-- code generator grammar specification.
-----------------------------------------------------------------------------

module Ast.Ir (
  -- Types
  Ir(..), OperatorMap,
  -- Functions
  baseRuleMap, linkSet,
) where

{- unqualified imports  -}
import Maybe (fromJust, isJust)

import Ast.Term (TermClass(..))

{- qualified imports  -}
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Debug as Debug (Entry)
import qualified Ast.Incl as Incl (Include)
import qualified Ast.Op as Op (Operator)
import qualified Ast.Decl as Decl (Declaration)
import qualified Ast.Def as Def (Definition, getProds, getDefForProd)
import qualified Ast.Prod as Prod (Production, getArity, toOp, getNode)
import qualified Ast.Node as N (hasLink)

-----------------------------------------------------------------------------

{- | Map holding operators keyed by arity -}
type OperatorMap = M.Map Int (S.Set Op.Operator)

{- | Result record holding intermediate representation of input -}
data Ir =
  Ir  { include       :: Incl.Include         -- ^ imports and includes
      , declaration   :: Decl.Declaration     -- ^ declarations
      , operators     :: [Op.Operator]        -- ^ operator definitions
      , definitions   :: [Def.Definition]     -- ^ rewrite rule definitions
      , debug         :: [Debug.Entry]        -- ^ debug entries
      , operatorMap   :: OperatorMap }        -- ^ map holding operators keyed by arity


{- | Map from arities to productions -}
baseRuleMap :: Ir -> M.Map Int [Prod.Production]
baseRuleMap ir =
  foldr
    (\p m ->
      M.alter
        (\a -> -- alter function
          if (isJust a)
            then Just $ p:(fromJust a)
            else Just $ [p])
        (Prod.getArity p) -- alter at key
        m)  -- map to alter
    M.empty -- start with empty map
    (filter (isTerminal) $ concatMap (Def.getProds) $ definitions ir) -- productions

{- | Computes set of operators that have 'links' -}
linkSet :: Ir -> S.Set Op.Operator
linkSet ir =
  foldr
    (\(d, p) s -> -- for each production in a definitions
      if (N.hasLink $ Prod.getNode p)
        then S.union s $ computeLinkSet d $ definitions ir
        else s)
    S.empty -- start with empty set
    (concatMap (\d -> map (\p -> (d,p)) (Def.getProds d)) $ definitions ir)
  where
    {- | Calculates link set for productions like:
          * stmtseq = stmt [ stmtseq ]
            stmt = ADD
                | fun
                | ...
            Given a definition, this function calculates all possible
            non terminals which may follow it as a link. -}
    computeLinkSet :: Def.Definition -> [Def.Definition] -> S.Set Op.Operator
    computeLinkSet def defs =
      let (ops, workset) = divideProdTypes (Def.getProds def) in
      S.fold
        (\ndef set' -> S.union (computeLinkSet ndef defs)  set')
        (ops)
        (workset)
      where
        divideProdTypes :: [Prod.Production] -> (S.Set Op.Operator, S.Set Def.Definition)
        divideProdTypes prods =
          foldr
            (\prod (ops, ds) ->
              if (isTerminal prod)
                then (S.insert (Prod.toOp prod) ops, ds)
                else (ops, S.insert (fromJust (Def.getDefForProd defs prod)) ds))
            (S.empty, S.empty)
            prods

-----------------------------------------------------------------------------