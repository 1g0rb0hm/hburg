-----------------------------------------------------------------------------
-- |
-- Module      :  Csa
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- This module contains various semantic analysis functions used
-- during the process of parsing the tree pattern matching language
-- specification. Tree pattern typechecking is also implemented in
-- this module.
--
-----------------------------------------------------------------------------

module Csa.Csa (
		-- * Operations
		updateEnv,
		checkEnv,
		checkDef,
	) where

import Maybe (fromJust, isJust, isNothing)

import qualified List as L (find)
import qualified Data.Set as S
import qualified Data.Map as M

import Ast.TermTy (TermTy, TermTyClass(..))

import qualified Ast.Ident as Id (Ident)
import qualified Ast.Node as N (Node, NodeClass(..), getLink, showAsFunction, mapPreOrder)
import qualified Ast.Bind as B (getBinding)
import qualified Ast.Prod as P (getNode)
import qualified Ast.Def as D (Definition, getProds, isNodeDefined)

import Env.Env (ElemClass(..), Elem, Env,
			mergeEnvs, newEnv, envElem, inEnv)

import Parser.ParseErr (parseErrElem, typeError)

------------------------------------------------------------------------------------

type IdSet = S.Set Id.Ident

-- | TypeEntry. A type entry encodes the following information:
data TypeEntry
	= Entry {node	:: N.Node,		-- AST node where first definition was encountered
			returns	:: IdSet,		-- set of Id's a TermTy produces, possibly through chain rules
			params	:: [IdSet]}		-- parameter list where the elements are Sets of Id's a T expects

-- | TypeMap. Mapping of Identifiers to TypeEntries.
type TypeMap
	= (M.Map
		Id.Ident		-- Key: Id.Ident
		TypeEntry)		-- Value: TypeEntry

-- | tentry. Smart constructor for TypeEntries.
tentry :: N.Node -> IdSet -> [IdSet] -> TypeEntry
tentry n rs ps = Entry {node = n,
						returns = rs,
						params = ps}

-- | computeTypeMap. Compute map from TermTy's (or rather their Id's) to TypeEntries 
--		where a TypeEntry represents the return and parameter types of TermTy's.
computeTypeMap :: [D.Definition] -> TypeMap
computeTypeMap ds
	= let tmap = -- 1. Calculate 'return' types (a.k.a. set Nt's which a T or Nt produces)
			foldr
				(\d m' ->
					-- 1.0 Calculate return type set based on current definition
					let typeSet
							= S.insert
								(getId d)
								(computeTypeSet d (filter (\x -> x /= d) ds) S.empty)
						in
					-- 1.1 Insert non terminal being defined into TypeMap
					let m'' = if (M.notMember (getId d) m')
								then M.insert (getId d) (tentry N.emptyNode typeSet []) m'
								else let ent = m' M.! (getId d) in
									M.insert
										(getId d)
										(tentry (node ent) (S.union typeSet (returns ent)) (params ent)) m'
						in
					-- 1.2 Insert productions (a.k.a. right hand sides of definitions) into TypeMap
					foldr
						(\p m''' ->
							if (M.notMember (getId p) m''')
								then M.insert (getId p) (tentry (P.getNode p) typeSet []) m'''
								else let ent = m''' M.! (getId p) in
									M.insert
										(getId p)
										(tentry (node ent) (S.union typeSet (returns ent)) (params ent)) m''')
						(m'')
						(D.getProds d))
				(M.empty)
				(ds)
		in
	M.map -- 2. Calculate parameter types of T's (parameter types are calculated based on return types)
		(\ent ->
			if (isTerm (node ent))
				then -- Only T's can have parameters (a.k.a. patterns)
					let paramList
							= map
								(\child -> returns (tmap M.! (getId child)))
								(N.getChildren (node ent))
						in
					tentry (node ent) (returns ent) paramList
				else ent)
		(tmap)
	where
		-- | computeTypeSet. For a given Definition, calculate the set of Id's it produces.
		computeTypeSet :: D.Definition -> [D.Definition] -> IdSet -> IdSet
		computeTypeSet _ [] set = set
		computeTypeSet d ds set
			= let result 
					= L.find
						(\d ->
							isJust (L.find
									(\p -> getId d == getId p)
									(D.getProds d)))
						(ds)
				in
			case result of
				Just d' -> let set' = S.insert (getId d) set in
						computeTypeSet d' (filter (\x -> x /= d') ds) set'
				Nothing -> S.insert (getId d) set


-- | If Binding for TermTy is not in the Env return 'Right' with
--	updated Env, otherwise return 'Left' indicating the two culprits
updateEnv :: TermTy -> Env -> Either (Elem, Elem) Env
updateEnv ty env
	= if (hasBinding ty)
		then mergeEnvs env (newEnv (envElem (B.getBinding (getBinding ty))))
		else Right env


-- | Checks if a definition contains the following:
--		nt = nt1
--			| nt2
--			| nt <-- This is the error case since a definition is being defined in terms of itself
--			| nt3.
checkDef :: D.Definition -> Maybe String
checkDef d
	= let clash 
			= L.find
				(\p -> ((getId p) == (getId d))) 
				(filter (\p -> isNonTerm p) (D.getProds d)) 
		in
	case clash of
		Nothing -> Nothing
		Just p -> Just (parseErrElem (envElem (P.getNode p))
				("'" ++ elemShow (envElem (P.getNode p)) ++ "' can not be defined in terms of itself."))


-- | checkEnv. Checks if all nodes have been defined. Returns 'Nothing'
--		if checks succeed, or a 'Just [String]' where '[String]' contains the
--		the error messages.
checkEnv :: [D.Definition] -> Env -> Maybe [String]
checkEnv [] env = Nothing
checkEnv defs env
	= 
	let tymap = computeTypeMap defs in
	-- Retrieve all production nodes
	let pnodes = concatMap (\x -> (map (\y -> P.getNode y) (D.getProds x))) (defs) in
	-- Go over each production node in preorder and collect CSA results
	let results =
		concatMap
			(\p ->
				N.mapPreOrder
					-- Apply the following to each node
					(\n -> -- 1. Check if node is in the Environment
						if (inEnv env (envElem n))
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
														if (inEnv env (envElem (N.getLink n)))
															then Nothing
															else -- Whoops! Check of link failed
																Just (parseErrElem (envElem (N.getLink n)) (show (elemType (N.getLink n)) ++
																	" '" ++ elemShow (N.getLink n) ++ "' is undefined."))
											else -- Whoops! Type Check failed
												typeError
									else -- Whoops! Check failed
										Just (parseErrElem (envElem n) (show (elemType n) ++ " '" ++
											N.showAsFunction n ++ "' is undefined."))
							else -- Whoops! Check failed
								Just (parseErrElem (envElem n) (show (elemType n) ++ " '" ++
									elemShow n ++ "' is undefined.")))
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

-- | typeCheck. If node is a T then check its parameters
typeCheck :: N.Node -> TypeMap -> Maybe String
typeCheck n _ | (N.isNil n) = Nothing
typeCheck n tymap
	= if (isTerm n) -- Only T's are of interest for type checking.
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
										then Just (typeError (envElem n) i (show (elemType n) ++ " '" ++
											N.showAsFunction n ++
											"' - expected type" ++ (if (S.size p > 1) then "s" else "") ++
											" '" ++ show (S.toList p) ++
											" but found '" ++ show (S.toList (returns ent')) ++ "'"))
										else Nothing
								else
									-- If we got in here we have encountered an undefined
									-- node. Checking if all nodes are defined is NOT the
									-- the task of this function - this happens in checkEnv
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