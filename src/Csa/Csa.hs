-----------------------------------------------------------------------------
-- |
-- Module      :  Csa
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- This module contains various semantic analysis functions used
-- during the process of parsing the tree pattern matching language
-- specification. Tree pattern typechecking is also implemented in
-- this module.
-----------------------------------------------------------------------------

module Csa.Csa (
        -- * Functions
        updateCtx,
        checkCtx,
        checkDef,
        checkProd,
    ) where

import Maybe (fromJust, isJust, isNothing)
import List (find)

import qualified List as L (find)
import qualified Data.Set as S
import qualified Data.Map as M

import Ast.Term (Term, TermClass(..))

import qualified Ast.Ident as Id (Ident)
import qualified Ast.Node as N (Node, TreeClass(..), getLink, showAsFun, mapPreOrder)
import qualified Ast.Bind as B (getIdent)
import qualified Ast.Prod as P (Production, getNode, getProdsByIdent)
import qualified Ast.Def as D (Definition, getProds, isNodeDefined)

import qualified Csa.Ctx as Ctx (Ctx, merge, new, member)
import qualified Csa.Elem as Elem (ElemClass(..), Elem, new)

import Parser.ParseErr (parseErrElem, typeError)
-----------------------------------------------------------------------------

type IdSet = S.Set Id.Ident

-- | A type entry represents an entry in a Symbol Table
data TyEntry
    = Entry {   node    :: N.Node   -- ^ AST node where first definition was encountered
            ,   returns :: IdSet    -- ^ set of Id's a Term produces, possibly through chain rules
            ,   params  :: [IdSet]} -- ^ parameter list where the elements are Sets of Id's a T expects

-- | Mapping of Identifiers to TypeEntries (a.k.a. Symbol Table)
type TyMap
    = (M.Map Id.Ident    -- Key: Id.Ident
             TyEntry)  -- Value: TyEntry

-- | Constructor for building TypeEntries
tyEntry :: N.Node -> IdSet -> [IdSet] -> TyEntry
tyEntry n rs ps = Entry {node = n,
                        returns = rs,
                        params = ps}

-- | Compute map from Term's (or rather their Id's) to TypeEntries where a 
--      TyEntry represents the return and parameter types of Term's.
computeTyMap :: [D.Definition] -> TyMap
computeTyMap ds
    = let tmap = -- 1. Calculate 'return' types (a.k.a. set Nt's which a T or Nt produces)
            foldr
                (\d m' ->
                    -- 1.0 Calculate return type set based on current definition
                    let typeSet
                            = S.insert
                                (getId d)
                                (computeTypeSet d (filter (\x -> x /= d) ds) S.empty)
                        in
                    -- 1.1 Insert non terminal being defined into TyMap
                    let m'' = if (M.notMember (getId d) m')
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
                        (m'')
                        (D.getProds d))
                (M.empty)
                (ds)
        in
    M.map -- 2. Calculate parameter types of T's (parameter types are calculated based on return types)
        (\ent ->
            if (isTerminal (node ent))
                then -- Only T's can have parameters (a.k.a. patterns)
                    let paramList
                            = map
                                (\child -> returns (tmap M.! (getId child)))
                                (N.getChildren (node ent))
                        in
                    tyEntry (node ent) (returns ent) paramList
                else ent)
        (tmap)
    where
        -- | For a given Definition, calculate the set of Id's it produces.
        computeTypeSet :: D.Definition -> [D.Definition] -> IdSet -> IdSet
        computeTypeSet _ [] set = set
        computeTypeSet d ds set
            = let result 
                    = L.find
                        (\d' ->
                            isJust (L.find
                                    (\p -> getId d' == getId p)
                                    (D.getProds d)))
                        (ds)
                in
            case result of
                Just d' -> let set' = S.insert (getId d) set in
                        computeTypeSet d' (filter (\x -> x /= d') ds) set'
                Nothing -> S.insert (getId d) set


-- | If Binding for Term is not in the context return 'Right' with
--    updated Ctx, otherwise return 'Left' indicating the two culprits
updateCtx :: Term -> Ctx.Ctx -> Either (Elem.Elem, Elem.Elem) Ctx.Ctx
updateCtx ty ctx
    = if (hasBinding ty)
        then Ctx.merge ctx (Ctx.new (Elem.new (B.getIdent (getBinding ty))))
        else Right ctx


-- | Checks if a definition contains the following:
--        nt = nt1
--            | nt2
--            | nt <-- This is the error case since a definition is being defined in terms of itself
--            | nt3.
checkDef :: D.Definition -> Maybe String
checkDef d
    = let clash 
            = L.find
                (\p -> ((getId p) == (getId d))) 
                (filter (\p -> isNonTerminal p) (D.getProds d))
        in
    case clash of
        Nothing -> Nothing
        Just p -> Just (parseErrElem (Elem.new (P.getNode p))
                ("'"++ Elem.elemShow (Elem.new (P.getNode p)) ++"' can not be defined in terms of itself."))


-- | Checks whether all uses of a production have the same amount of parameters
-- as has been defined. The following error case should be captured:
--      reg = ADD (reg, reg)
--          | SUB (ADD(reg), reg)
--                 ^^^ 
-- ADD was defined as a tree pattern with 2 children but 'used' with only one child.
-- In other words ADD was defined with two parameters but was called with only one.
checkProd :: [P.Production] -> P.Production -> Either (N.Node, N.Node) [P.Production]
checkProd [] _ = Right []
checkProd prods p
    = let new = P.getNode p in
    let errprod = find
                        (\n -> (P.getNode n) /= new) 
                        (P.getProdsByIdent prods new)
        in
    case errprod of
        Nothing -> Right (p:prods)
        Just err -> Left (new, P.getNode err)


-- | Checks if all nodes have been defined. Returns 'Nothing' if checks succeed, 
--      or a 'Just [String]' where '[String]' contains the the error messages.
checkCtx :: [D.Definition] -> Ctx.Ctx -> Maybe [String]
checkCtx [] ctx = Nothing
checkCtx defs ctx
    = let tymap = computeTyMap defs in
    let pnodes = concatMap (\x -> (map (\y -> P.getNode y) (D.getProds x))) (defs) in
    let results = -- go over each production node in preorder and collect CSA results
            concatMap
                (\p ->
                    N.mapPreOrder
                        -- Apply the following to each node
                        (\n -> -- 1. Check if node is in Context
                            if (Ctx.member ctx (Elem.new n))
                                then -- 2. Check if node is defined
                                    if (D.isNodeDefined defs n)
                                        then
                                            -- 3. Type Check node parameters
                                            let typeError = typeCheck n tymap in
                                            if (isNothing typeError)
                                                then
                                                    -- 4. Check link node if one is defined
                                                    if (N.isNil (N.getLink n))
                                                        then Nothing
                                                        else -- link node is defined
                                                            if (Ctx.member ctx (Elem.new (N.getLink n)))
                                                                then Nothing
                                                                else -- Whoops! Check of link failed
                                                                    Just (parseErrElem (Elem.new (N.getLink n)) (show (Elem.elemType (N.getLink n)) ++
                                                                        " '"++ Elem.elemShow (N.getLink n) ++"' is undefined."))
                                                else -- Whoops! Type Check failed
                                                    typeError
                                        else -- Whoops! Check failed
                                            Just (parseErrElem (Elem.new n) (show (Elem.elemType n) ++" '"++
                                                N.showAsFun n ++"' is undefined."))
                                else -- Whoops! Check failed
                                    Just (parseErrElem (Elem.new n) (show (Elem.elemType n) ++" '"++
                                        Elem.elemShow n ++"' is undefined.")))
                        (p))
                (pnodes)
        in
    -- Evaluate CSA results
    let errors
            = map
                (\x -> fromJust x)
                -- filter out only the 'Just' results
                (filter (\r -> isJust r) (results))
        in
    -- In case there are errors return them
    case errors of
        [] -> Nothing
        otherwise -> Just (reverse errors)

-- | Type check a node by looking it up in the TyMap (our SymbolTable).
typeCheck :: N.Node -> TyMap -> Maybe String
typeCheck n _ | (N.isNil n) = Nothing
typeCheck n tymap
    = if (isTerminal n) -- Only T's are of interest for type checking.
        then
            let ent = tymap M.! (getId n) in
            -- Check return type of child nodes
            let results =
                    map
                        (\(p, n', i) ->
                            if (M.member (getId n') tymap)
                                then
                                    let ent' = tymap M.! (getId n') in
                                    -- Intersection of parameter and return set must not be empty
                                    if (S.null (S.intersection p (returns ent')))
                                        then Just (typeError (Elem.new n) i (show (Elem.elemType n) ++" '"++
                                            N.showAsFun n ++
                                            "' - expected type"++ (if (S.size p > 1) then "s" else "") ++
                                            " '"++ show (S.toList p) ++
                                            "' but found '"++ show (S.toList (returns ent')) ++"'"))
                                        else Nothing
                                else
                                    -- If we got in here we have encountered an undefined
                                    -- node. Checking if all nodes are defined is NOT the
                                    -- the task of this function - this happens in checkCtx
                                    -- so typeCheck just returns Nothing in this case.
                                    Nothing)
                        (zip3 (params ent)
                                (N.getChildren n)
                                ((\x -> if (x == []) then [] else reverse [1 .. length x]) (params ent)))
                in
            -- Evaluate errors
            let errors
                    = map
                        (\x -> fromJust x)
                        -- filter out only the 'Just' results
                        (filter (\r -> isJust r) (results))
                in
            -- In case there are errors return them
            case errors of
                [] -> Nothing
                otherwise -> Just (concat errors)
        else Nothing