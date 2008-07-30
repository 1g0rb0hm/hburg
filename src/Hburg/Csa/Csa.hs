-----------------------------------------------------------------------------
-- |
-- Module      :  Csa (Context Sensitive Analysis)
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--  This module contains various semantic analysis functions used
--  during the process of parsing the tree pattern matching language
--  specification. Tree pattern typechecking is also implemented in
--  this module.
-----------------------------------------------------------------------------

module Hburg.Csa.Csa (
  -- Functions
  updateCtx,
  checkCtx,
  checkDef,
  checkProd,
) where

{- unqualified imports  -}
import Maybe (fromJust, isJust, isNothing)
import List (find)
import Hburg.Ast.Term (Term, TermClass(..))
import Hburg.Parse.Msg (parseErrElem, typeErr)

{- qualified imports  -}
import qualified List as L (find)
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Hburg.Ast.Ident as Id (Ident)
import qualified Hburg.Ast.Node as N (Node, TreeClass(..), getLink, showAsFun, mapPreOrder)
import qualified Hburg.Ast.Bind as B (getIdent)
import qualified Hburg.Ast.Prod as P (Production, getNode)
import qualified Hburg.Ast.Def as D (Definition, getProds, isNodeDefined)

import qualified Hburg.Csa.Ctx as Ctx (Ctx, merge, new, member)
import qualified Hburg.Csa.Elem as E (ElemClass(..), Elem, new)

-----------------------------------------------------------------------------

type IdSet = S.Set Id.Ident

{- | A type entry represents an entry in a Symbol Table -}
data TyEntry =
  Entry { node    :: N.Node   -- ^ AST node where first definition was encountered
        , returns :: IdSet    -- ^ set of Id's a Term produces, possibly through chain rules
        , params  :: [IdSet]} -- ^ parameter list where the elements are Sets of Id's a T expects

{- | Mapping of Identifiers to TypeEntries (a.k.a. Symbol Table) -}
type TyMap =
  (M.Map Id.Ident  -- Key: Id.Ident
         TyEntry)  -- Value: TyEntry

{- | Constructor for building TypeEntries -}
tyEntry :: N.Node -> IdSet -> [IdSet] -> TyEntry
tyEntry n rs ps =
  Entry { node = n
        , returns = rs
        , params = ps}

{- | Compute map from Term's (or rather their Id's) to TypeEntries where a 
     TyEntry represents the return and parameter types of Term's. -}
computeTyMap :: [D.Definition] -> TyMap
computeTyMap ds =
  let tmap = -- 1. Calculate 'return' types (a.k.a. set Nt's which a T or Nt produces)
        foldr
          (\d m' ->
            -- 1.0 Calculate return type set based on current definition
            let typeSet =
                  S.insert
                    (getId d)
                    (computeTypeSet d (filter (/= d) ds) S.empty)
            -- 1.1 Insert non terminal being defined into TyMap
                m'' =
                  if (M.notMember (getId d) m')
                    then M.insert (getId d) (tyEntry N.empty typeSet []) m'
                    else let ent = m' M.! (getId d) in
                      M.insert
                        (getId d)
                        (tyEntry (node ent) (S.union typeSet (returns ent)) (params ent)) m'
            in
            -- 1.2 Insert productions (a.k.a. right hand sides of definitions) into TyMap
            foldr
              (\p m''' ->
                if (M.notMember (getId p) m''')
                  then M.insert (getId p) (tyEntry (P.getNode p) typeSet []) m'''
                  else let ent = m''' M.! (getId p) in
                    M.insert
                      (getId p)
                      (tyEntry (node ent) (S.union typeSet (returns ent)) (params ent)) m''')
              m''
              (D.getProds d))
          M.empty
          ds in
  M.map -- 2. Calculate parameter types of T's (parameter types are calculated based on return types)
    (\ent ->
      if (isTerminal (node ent))
        then -- Only T's can have parameters (a.k.a. patterns)
          let paramList =
                map
                  (\child -> returns (tmap M.! (getId child)))
                  (N.getChildren (node ent)) in
          tyEntry (node ent) (returns ent) paramList
        else ent)
    tmap
  where
    {- | For a given Definition, calculate the set of Id's it produces. -}
    computeTypeSet :: D.Definition -> [D.Definition] -> IdSet -> IdSet
    computeTypeSet _ [] set = set
    computeTypeSet d ds set =
      let result = L.find (\d' -> isJust (L.find
                        (\p -> getId d' == getId p)
                        (D.getProds d)))
                        ds in
      case result of
        Just d' -> let set' = S.insert (getId d) set in
                computeTypeSet d' (filter (/= d') ds) set'
        Nothing -> S.insert (getId d) set


{- | If Binding for Term is not in the context return 'Right' with
    updated Ctx, otherwise return 'Left' indicating the two culprits -}
updateCtx :: Term -> Ctx.Ctx -> Either (E.Elem, E.Elem) Ctx.Ctx
updateCtx ty ctx =
  if (hasBinding ty)
    then Ctx.merge ctx (Ctx.new (E.new (B.getIdent (getBinding ty))))
    else Right ctx


{- | Checks if a definition contains the following:
        nt = nt1
          | nt2
          | nt <-- This is the error case since a definition is being defined in terms of itself
          | nt3. -}
checkDef :: D.Definition -> Maybe String
checkDef d =
  let clash = L.find
                (\p -> ((getId p) == (getId d))) 
                (filter (isNonTerminal) (D.getProds d)) in
  case clash of
      Nothing -> Nothing
      Just p -> Just (parseErrElem (E.new (P.getNode p))
              ("'"++ E.elemShow (E.new (P.getNode p)) ++"' can not be defined in terms of itself."))


{- | Checks whether all uses of a production have the same amount of parameters
     as has been defined. The following error case should be captured:
        reg = ADD (reg, reg)
            | SUB (ADD(reg), reg)
                      ^^^
     ADD was defined as a tree pattern with 2 children but 'used' with only one child.
     In other words ADD was defined with two parameters but was called with only one. -}
checkProd :: [P.Production] -> P.Production -> Either (N.Node, N.Node) [P.Production]
checkProd [] _ = Right []
checkProd prods p =
  let new = P.getNode p
      errProd = find
                  (\n -> (P.getNode n) /= new)
                  -- productions that have the same identifier
                  (filter (\p -> getId p == getId new) (prods))
  in
  case errProd of
    Nothing -> Right (p:prods)
    Just err -> Left (new, P.getNode err)


{- | Checks if all nodes have been defined. Returns 'Nothing' if checks succeed,
     or a 'Just [String]' where '[String]' contains the the error messages. -}
checkCtx :: [D.Definition] -> Ctx.Ctx -> Maybe [String]
checkCtx [] ctx = Nothing
checkCtx defs ctx =
  let tymap = computeTyMap defs
      pnodes = concatMap (\x -> (map (P.getNode) (D.getProds x))) (defs)
      results =
        concatMap -- traverse AST in preorder and collect CSA results
          (\p ->
            N.mapPreOrder
              (\n -> -- 1. Check if node is in Context
                if (Ctx.member ctx $ E.new n)
                  then -- 2. Check if node is defined
                    if (D.isNodeDefined defs n)
                      then -- 3. Type Check node parameters
                        let typeErr = typeCheck n tymap in
                        if (isNothing typeErr)
                          then -- 4. Check link node if one is defined
                            let link = N.getLink n in
                            if (N.isNil link)
                              then Nothing
                              else -- link node is defined
                                if (Ctx.member ctx (E.new link))
                                  then Nothing
                                  else -- Whoops! Check of link failed
                                    Just (parseErrElem (E.new link)
                                        (show (E.elemType link) ++
                                        " '"++ E.elemShow link ++"' is undefined."))
                          else -- Whoops! Type Check failed
                            typeErr
                      else -- Whoops! Node is not defined
                        Just (parseErrElem (E.new n) (show (E.elemType n) ++" '"++
                            N.showAsFun n ++"' is undefined."))
                  else -- Whoops! Node is not in context
                    Just (parseErrElem (E.new n) (show (E.elemType n) ++" '"++
                        E.elemShow n ++"' is undefined.")))
              p)
          pnodes
      -- Evaluate CSA results
      errors = map (fromJust) (filter (isJust) (results))
  in
  -- In case there are errors return them
  if (null errors)
    then Nothing
    else Just $ reverse errors


{- | Type check a node by looking it up in the TyMap (our SymbolTable). -}
typeCheck :: N.Node -> TyMap -> Maybe String
typeCheck n _ | (N.isNil n) = Nothing
typeCheck n tymap =
  if (isTerminal n) -- Only T's are of interest for type checking.
    then
      let ent = tymap M.! (getId n)
          {- A Terminal can be regarded as a function that has parameters. We must
            check that the supplied parameters are in line with what the Terminal (i.e. function)
            expects. We do this by looking at the child nodes of the Terminal in our
            and comparing their return types to the expected parameter types of the
            Terminal. -}
          results =
            map
              (\(p, n', i) -> -- triple containing parameter, actual node in AST, its index
                if (M.member (getId n') tymap)
                  then
                    let ent' = tymap M.! (getId n') in
                    -- Intersection of parameter and return set must not be empty
                    if (S.null (S.intersection p (returns ent')))
                      then Just (typeErr (E.new n) i (show (E.elemType n) ++" '"++
                          N.showAsFun n ++
                          "' - expected type"++ (if (S.size p > 1) then "s" else "") ++
                          " '"++ show (S.toList p) ++
                          "' but found '"++ show (S.toList (returns ent')) ++"'"))
                      else Nothing
                  else
                    {- If we got in here we have encountered an undefined
                       node. Checking if all nodes are defined is NOT the
                       the task of this function - this happens in checkCtx
                       so typeCheck just returns Nothing in this case. -}
                    Nothing)
              (zip3 (params ent)
                    (N.getChildren n)
                    ((\x -> reverse $ take (length x) [1..]) (params ent)))
          -- Evaluate errors
          errors = map (fromJust) (filter (isJust) (results))
      in
      -- In case there are errors return them
      if (null errors)
        then Nothing
        else Just $ concat errors
    else Nothing

-----------------------------------------------------------------------------