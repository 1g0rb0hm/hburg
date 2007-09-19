-----------------------------------------------------------------------------
-- |
-- Module      :  Tile
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Given operators and definitions extracted from the tree pattern matching
-- language specification, this module calculates all parameters which are
-- necessary in order to easily emit target code for the dynamic programming
-- stage of our code generator, namely the tiling phase.
-----------------------------------------------------------------------------

module Gen.Tile (
        -- * Types
        Tiling,Closure,Arity,
        LinkSet,OperatorsPerArity,ProductionsPerArity,
        -- * Construction
        new,
        -- * Functions
        getClosures,
        hasClosures,
        getProductionsPerArity,
        getOperatorsPerArity,
        getLinkSet,
        getProdsForArity,
        closureGetFromLabel,
        closureGetRuleLabel,
        closureGetToLabel,
    ) where

import Maybe (fromJust)

import Ast.Op (Operator)
import Ast.Term (TermClass(..))
import Ast.Node (getTerm, hasLink)
import qualified Ast.Def as D (Definition, getClosures, getProds, getDefForProd)
import Ast.Prod (Production, getArity, getNode, toOp, getRuleLabel, getResultLabel)

import Env.Elem (ElemClass(elemId))

import Gen.Emit.Label (Label, tTyToEnumLabel)

import qualified Data.Set as S
import qualified Data.Map as M
-----------------------------------------------------------------------------

type Arity = Int

-- | LinkSet. Set of operators having links
type LinkSet = (S.Set Operator)

-- | OperatorsPerArity. Map of operators keyed by their arity
type OperatorsPerArity
    = (M.Map 
        Arity               -- key: arity of operator
        (S.Set Operator))   -- operators

-- | ProductionsPerArity. Map of productions keyed by their arity
type ProductionsPerArity
    = (M.Map
        Arity               -- key: arity of the node
        [Production])             -- value: the production itself

-- | Tiling.
data Tiling
    = Tiling
        [Closure]           -- list of closures for this tiling
        OperatorsPerArity   -- holds all operators keyed by the arity of the production (used to calc. EnumSets)
        LinkSet             -- the link set
        ProductionsPerArity -- holds all productions with the label they produced, and are keyed by the arity of the production

-- Triple necessary for emission of closures
data Closure
    = Closure
        Label            -- from label
        Label            -- to label
        Label            -- rule label
    deriving (Eq, Show)

instance Show Tiling where
    show (Tiling closure opmap linkset funmap)
        = "\n\nTiling: \n\nClosure(s): \n" ++
        (show closure) ++ "\n\n" ++                     -- 1. Display Closures 
        "LinkSet: " ++                                  -- 2. Display LinkSets 
        (show linkset) ++ "\n\n" ++
        "OperatorSet(s): " ++                           -- 3. Display pre Arity operator sets
        (concatMap
            (\(arity, set) ->
                "\n\nArity: " ++ show arity ++ "\n" ++ show set)
            (M.toList opmap)) ++ "\n\n" ++
        "Production(s):" ++                             -- 4. Display per Arity productions
        (concatMap
            (\(arity, prods) ->
                "\n\nArity: " ++ show arity ++ "\n\n" ++
                (concatMap (\y -> show y ++ "\n") (prods)))
            (M.toList funmap))

getClosures :: Tiling -> [Closure]
getClosures (Tiling cl _ _ _) = cl

getProductionsPerArity :: Tiling -> ProductionsPerArity
getProductionsPerArity (Tiling _ _ _ funs) = funs

getOperatorsPerArity :: Tiling -> OperatorsPerArity
getOperatorsPerArity (Tiling _ opset _ _) = opset

getLinkSet :: Tiling -> LinkSet
getLinkSet (Tiling _ _ lset _) = lset

getProdsForArity :: Tiling -> Arity -> [Production]
getProdsForArity (Tiling _ _ _ funs) key
    = if (M.member key funs)
        then funs M.! key
        else []

closureGetFromLabel :: Closure -> Label
closureGetFromLabel (Closure l _ _) = l

closureGetToLabel :: Closure -> Label
closureGetToLabel (Closure _ l _ ) = l

closureGetRuleLabel :: Closure -> Label
closureGetRuleLabel (Closure _ _ l) = l

hasClosures :: Tiling -> Bool
hasClosures (Tiling [] _ _ _) = False
hasClosures _ = True

-- | Constructs a Tiling from Operators and Definitions. This function unwraps definitions and extracts
--      all the necessary information in order to produce the Dynamic Programming
--      part of our code generator, namely the tiling. It starts off with an
--      initially almost empty Tiling and as the definitions are processed
--      as much information as possible is gathered to avoid repeated traversals
--      of the definition list.
new :: [Operator] -> [D.Definition] -> Tiling
new ops defs
    = let opmap = M.fromList (map (\o -> (elemId o, o)) (ops)) in
    mapDefs defs opmap (Tiling (computeClosure defs) M.empty S.empty M.empty)
    where
        -- Map over all definitions
        mapDefs :: [D.Definition] -> (M.Map String Operator) -> Tiling -> Tiling
        mapDefs [] _ tiling = tiling
        mapDefs (d:ds) env otiling
            = mapDefs ds env (mapProds (D.getProds d) otiling)
            where
                -- Map over all productions
                mapProds :: [Production] -> Tiling -> Tiling
                mapProds [] tiling = tiling
                mapProds (p:ps) (Tiling cl opmap lset funmap)
                    = -- Retrieve Term
                    let tTy = case (getTerm (getNode p)) of
                                Just ty -> ty
                                Nothing -> error "\nERROR: Encountered Node without Term during Tiling!\n"
                        in
                    if (isTerminal tTy)
                        -- If node is a Terminal (e.g. ADD) we need to do some work!
                        then
                            -- 1. Get arity of node since this is the key for our maps
                            let arity = getArity p in
                            -- 2. Adjust the link set if necessary
                            let nlset 
                                    = if hasLink (getNode p)
                                        then S.insert (env M.! (elemId tTy)) lset
                                        else lset
                                in
                            -- 3. Adjust operator map keyed by arity if necessary
                            let nopmap
                                    = if (M.member arity opmap)
                                        then -- Get operator set for this arity
                                            let set = (opmap M.! arity) in
                                            -- Extend it with new operator
                                            let nset = S.insert (env M.! (elemId tTy)) set in
                                            M.insert arity nset opmap
                                        else M.insert arity (S.singleton (env M.! (elemId tTy))) opmap
                                in
                            -- 4. Adjust function map keyed by arity if necessary
                            if (M.member arity funmap)
                                then -- Get value out of map and udpate it
                                    let ntiling
                                            = (Tiling
                                                cl
                                                nopmap
                                                nlset
                                                (M.insert arity (p:(funmap M.! arity)) funmap))
                                        in
                                    mapProds ps ntiling
                                else -- Insert new key value pair into the map
                                    let ntiling
                                            = (Tiling
                                                cl
                                                nopmap
                                                nlset
                                                (M.insert arity [p] funmap))
                                        in
                                    mapProds ps ntiling
                        -- Node is non terminal and has a link, we need to adjust the link set
                        else 
                            let nlset
                                    = if hasLink (getNode p)
                                        then
                                            let nset = computeLinkSet (fromJust (D.getDefForProd defs p)) defs in
                                            S.union nset lset
                                        else lset
                                in
                            mapProds ps (Tiling cl opmap nlset funmap)

-- | Calculates link set for productions like:
--      * stmtseq = stmt [ stmtseq ]
--        stmt = ADD
--            | fun
--            | ...
--        Given a definition, this function calculates all possible
--        non terminals which may follow it as a link.
computeLinkSet :: D.Definition -> [D.Definition] -> S.Set Operator
computeLinkSet def defs
    = let (ops, workset) = divideUpProdTypes (D.getProds def) in
    S.fold
        (\ndef set' -> S.union (computeLinkSet ndef defs)  set')
        (ops)
        (workset)
    where
        divideUpProdTypes :: [Production] -> (S.Set Operator, S.Set D.Definition)
        divideUpProdTypes prods 
            = foldr 
                (\prod (ops, ds) ->
                    if (isTerminal prod)
                        then (S.insert (toOp prod) ops, ds)
                        else (ops, S.insert (fromJust (D.getDefForProd defs prod)) ds))
                (S.empty, S.empty) 
                prods

-- | Calculate necessary values for target code closure function.
computeClosure :: [D.Definition] -> [Closure]
computeClosure [] = []
computeClosure (d:defs)
    = let prods = D.getClosures d in
    (map
        (\p ->
            case getTerm (getNode p) of
                Just ty -> 
                    -- Construct necessary closure with correct labels
                    Closure 
                        (tTyToEnumLabel ty)
                        (getResultLabel p)
                        (getRuleLabel p)
                Nothing -> error "\nERROR: Can not compute closure of Nil node!\n") 
        (prods))
    ++
    (computeClosure defs)