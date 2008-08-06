-----------------------------------------------------------------------------
-- |
-- Module      :  Tile
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module generates tiling target source code. The tiling code is made
-- up of methods that roughly do the following:
--    * Tile() method:
--    * Label() closure method:
--    * Label_NN() method:
-----------------------------------------------------------------------------

module Hburg.Gen.Java.Tile (
  -- * Functions
  tile,
) where

{- unqualified imports  -}
import Control.Monad.State

import Text.PrettyPrint

import Hburg.Ast.Term (TermClass(..), nonTerminal)
import Hburg.Ast.Op (Operator, opSem)
import Hburg.Ast.Def (getNodeReturnType)
import Hburg.Ast.Cost as Cost (isZero)
import Hburg.Ast.Prod (Production, getCost, getNode, getRuleLabel, getResultLabel, getArity)

import Hburg.Gen.Java.Modifier (Modifier(..))

{- qualified imports  -}

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Hburg.Ast.Ident as Id (Ident)
import qualified Hburg.Ast.Node as N (Node, mapPreOrder2)
import qualified Hburg.Ast.Ir as Ir (Ir(..), baseRuleMap, linkSet)
import qualified Hburg.Ast.Closure as Cl (Closure, Label(..), closure, fromLabels, toLabels, empty)

import qualified Hburg.Gen.Ident as I (Ident, ntId, rId, nId, cId, cTy, nTy, ntTy, rTy, eTy)
import qualified Hburg.Gen.Label as Lab (termToEnum, childCall)
import qualified Hburg.Gen.Java.Class as Class (Class(..), new)
import qualified Hburg.Gen.Java.Method as M (Method, new)
import qualified Hburg.Gen.Java.Var as V (Var, new)
import qualified Hburg.Gen.Java.Param as P (new, fromList)

-----------------------------------------------------------------------------

type NodeKind = String
type Arity = Int

{- | Top level function for generating tiling target source code. -}
tile :: I.Ident -> NodeKind -> Ir.Ir -> Class.Class
tile ids nkind ir =
  let prodmap = Ir.baseRuleMap ir
      linkset = Ir.linkSet ir
      closure = Cl.closure $ Ir.definitions ir
  in
  execState
    (do
      modify $ \c ->
        c { Class.modifier = Private
          , Class.isStatic = True
          , Class.variables = enumSetVars ir linkset    -- generate enumSetName variables
          , Class.methods =
            [ labClosureMethod ids closure              -- generate label() mehtod
            , tileMethod ids linkset (M.keys prodmap)]  -- generate tile() method [Cooper p.566]
            ++ labMethods ids ir prodmap})      -- generate label_N() methods for nodes with arity N
    $ Class.new "" "Tiling"

{- | Short cut -}
t :: String -> Doc
t = text

{- ----------------------------------------------------------------------------- -}

{- | Generate a name for node sets based on node arity. -}
enumSetName :: Int -> Doc
enumSetName arity = t "arity" <> int arity <> t "Set"

{- | Produce enumSetName variables.
     Example: private static enumSetName unarySet = EnumSet.of(FUN,FUNAP,SIDEFFECT) -}
enumSetVars :: Ir.Ir -> S.Set Operator -> [V.Var]
enumSetVars ir linkset =
  let ops = Ir.operatorMap ir
      vars =
        map
          (\key -> genVar (enumSetName key) (ops M.! key))
          (M.keys ops)
  in
  if (S.null linkset) -- add linkset if there are linked subtrees
    then vars
    else (genVar (t "linkSet") linkset):vars
  where
    -- | genVar.
    genVar :: Doc -> S.Set Operator -> V.Var
    genVar name set =
      let ops =
            if (S.null set)
              then empty
              else foldr1
                    (\o1 o2 -> o1 <> comma <> o2)
                    (map (t . show . opSem) $ S.toList set) in
      V.new Private True "EnumSet" (render name)
        $ render (t "EnumSet.of" <> parens ops)

{- ----------------------------------------------------------------------------- -}

-- | Generates tile method as in [Cooper p.566]
tileMethod :: I.Ident -> S.Set Operator -> [Int] -> M.Method
tileMethod ids linkset sets =
  M.new Public True "void" "tile" [P.new (I.nTy ids) (I.nId ids)] body
  where
    -- | Method body of tile method
    body :: Doc
    body = t "assert" <+> parens (t (I.nId ids) <+> t "!= null") <+> colon
      <+> doubleQuotes (t "ERROR: tile() - node is null.") <> semi
      $+$ ifCascade sets
      $+$ if (S.null linkset) -- generate code for linkset if there is one
        then empty
        else t "if" <+> parens (t "linkSet.contains" <>
          parens (t (I.nId ids) <> t ".kind()"))
          <+> lbrace
            $+$ nest 2 (t (I.nTy ids) <+> t "link" <+> equals
              <+> t (I.nId ids) <> t ".link()" <> semi
              $+$ t "if" <+> parens (t "link != null")
              <+> t "tile" <> parens (t "link") <> semi)
          $+$ rbrace

    {- Generate the 'if ... else if ... else' cascade in the tiling() method -}
    ifCascade :: [Int] -> Doc
    ifCascade [] = error "\nERROR: Can not generate tiling() method\
                          \ because no nodes are defined!\n"
    {- if (..) {...} -}
    ifCascade (x:[]) = t "if" <+> parens ((enumSetName x) <> t ".contains"
      <> parens(t (I.nId ids) <> t ".kind()"))
      <+> lbrace
        $+$ nest 2 (
              vcat [ t "tile" <> parens ( t (I.nId ids) <>
                     t ('.':(Lab.childCall pos)) <> parens empty <> semi)
                   | pos <- [1..x]]
              $+$ labMethodName x <> parens(t "n") <> semi)
      $+$ rbrace
    {- if (..) {...} else if (..) ... else {...} -}
    ifCascade (x:xs) = t "if" <+> parens ((enumSetName x) <> t ".contains"
      <> parens(t (I.nId ids) <> t ".kind()"))
      <+> lbrace
        $+$ nest 2 (
              vcat [ t "tile" <> parens ( t (I.nId ids) <>
                     t ('.':(Lab.childCall pos)) <> parens empty) <> semi
                   | pos <- [1..x]]
              $+$ labMethodName x <> parens(t $ I.nId ids) <> semi)
      $+$ rbrace
      $+$ (vcat -- else if branches
        (map
          (\arity -> t "else if" <+> parens ((enumSetName arity)
            <> t ".contains" <> parens (t (I.nId ids) <> t ".kind()"))
            <+> lbrace
              $+$ nest 2 (
                    vcat [ t "tile" <> parens ( t (I.nId ids) <>
                           t ('.':(Lab.childCall pos)) <> parens empty) <> semi
                         | pos <- [1..arity]]
                    $+$ labMethodName arity <> parens(t $ I.nId ids) <> semi)
            $+$ rbrace)
          (xs)))
      -- if nothing else matches throw an error
      $+$ t "else"
      <+> lbrace
        $+$ nest 2 (t "throw new AssertionError"
          <> parens(doubleQuotes (t "ERROR: tile() - Encountered\
                                    \ undefined node '")
            <>  t "+" <+> t (I.nId ids) <> t ".kind()"
            <+> t "+" <> doubleQuotes (t "'.")) <> semi)
      $+$ rbrace

{- ----------------------------------------------------------------------------- -}

{- | Generate a name for the label method depending on the arity of the nodes. -}
labMethodName :: Int -> Doc
labMethodName arity = t "label_" <> int arity

{- | Generates label method. -}
labClosureMethod :: I.Ident -> Cl.Closure -> M.Method
labClosureMethod ids cls =
  let params =
        P.fromList
          [(I.nTy ids, I.nId ids), (I.ntTy ids,I.ntId ids),
           (I.cTy ids, I.cId ids), (I.rTy ids, I.rId ids)] in
  M.new Private True "void" "label" params body
  where
    -- | Method body of label() method
    body :: Doc
    body = t "if" <+> parens (t (I.cId ids) <+> t "<" <+> t (I.nId ids)
      <>  t ".cost" <> parens (t $ I.ntId ids))
      <+> lbrace
      $+$ nest 2 (t (I.nId ids) <> t ".put" <>
            parens (t (I.ntId ids) <> comma <+> t "new"
              <+> t (I.eTy ids) <> parens (t (I.cId ids)
              <>  comma <+> t (I.rId ids))) <> semi
          $+$ (if (Cl.empty cls)
                then empty
                else closure))
      $+$ rbrace
      where
        {- | Code for transitive closure method label(). Example transitive
             closure: stmt = reg, reg = lab, ... -}
        closure :: Doc
        closure = t "switch" <+> parens (t $ I.ntId ids)
          <+> lbrace
            $+$ nest 2 (vcat
              (map -- case statements for closures
                (\fromL ->
                  t "case" <+> t fromL <> colon
                  <+> lbrace
                    $+$ nest 2 (vcat
                      (map -- label() method calls in case statement
                        (\lab -> t "label" <>
                          parens( t (I.nId ids)  <> comma <+>
                                  t (Cl.toL lab) <> comma <+>
                                  t (I.cId ids) <>
                                  -- if defined, add cost
                                  (if (Cost.isZero (Cl.cost lab))
                                    then comma
                                    else space <> t "+" <+>
                                  t (show $ Cl.cost lab) <> comma) <+>
                                  t (Cl.ruleL lab)) <> semi)
                        (S.toList $ Cl.toLabels fromL cls))
                      $+$ t "break" <> semi)
                  $+$ rbrace)
                (Cl.fromLabels cls)))
          $+$ rbrace

{- | Generate methods which do the actual labelling of AST nodes. -}
labMethods :: I.Ident -> Ir.Ir -> M.Map Int [Production] -> [M.Method]
labMethods ids ir prodmap =
  map -- map over all arities and generate methods
    (\(arity, prod) -> labSetMethod arity prod)
    (M.toList prodmap)
  where
    {- | Generate method which labels Nodes with a certain arity -}
    labSetMethod :: Arity -> [Production] -> M.Method
    labSetMethod arity prods =
      M.new Private True "void" (render $ labMethodName arity) [P.new (I.nTy ids) (I.nId ids)] body
      where
        {- | Method body -}
        body :: Doc
        body = t (I.cTy ids) <+> t (I.cId ids) <> semi
          $+$ t "switch" <+> parens (t (I.nId ids) <> t ".kind()")
          <+> lbrace
            $+$ nest 2 ((vcat cases)
              $+$ t "default" <> colon
              <+> lbrace
                -- Error handling in case we are in a label method
                -- and encounter an unknown kind of node
                $+$ nest 2 (t "throw new AssertionError"
                  <> parens(doubleQuotes (t "ERROR:" <+> labMethodName arity
                        <> t "(): Unhandled Node kind:")
                    <>  t "+" <+> t (I.nId ids) <> t ".kind()") <> semi)
              $+$ rbrace)
          $+$ rbrace <+> t "// END SWITCH"
        
        {- | Generate case stmts. for label methods -}
        cases :: [Doc]
        cases = map
            (\ps -> if (length ps > 1)
                then complexCase ps
                else simpleCase $ head ps)
            (M.elems . genProdMap $ prods)
          where
          {- | Generate map holding productions keyed by production identifiers -}
          genProdMap :: [Production] -> M.Map Id.Ident [Production]
          genProdMap prods =
              foldr
                (\p m ->
                  let key = getId p in
                  if (M.member key m)
                    then M.insert key (p:(m M.! key)) m
                    else M.insert key [p] m)
                M.empty
                prods
        
        {- | Simple case statement -}
        simpleCase :: Production -> Doc
        simpleCase p = t "case" <+> t (Lab.termToEnum p) <> colon
          <+> lbrace
            $+$ nest 2 (costAndLabel p $+$ t "break" <> semi)
          $+$ rbrace
        
        {- | Complex case statement -}
        complexCase :: [Production] -> Doc
        complexCase prods = t "case" <+> t (Lab.termToEnum . head $ prods) <> colon
          <+> lbrace
            $+$ nest 2 (vcat (
              map
                (\p ->
                  if (getArity p > 0)
                    then t "if" <+> parens(iff p)
                      <+> lbrace
                        $+$ nest 2 (costAndLabel p)
                      $+$ rbrace
                    else costAndLabel p)
                (prods))
              $+$ t "break" <> semi)
          $+$ rbrace

        {- | Path to child node (i.e. child1().child2().child1()...) -}
        childPath :: N.Node -> [([Doc], N.Node)]
        childPath node =
          N.mapPreOrder2
            (\pos _ -> [t "." <> t (Lab.childCall pos) <> parens (empty)])
            (\n' -> n')
            node

        {- | Create "cost = ...;" and "label(...)" calls. -}
        costAndLabel :: Production -> Doc
        costAndLabel p =
          let path = childPath $ getNode p in
          -- Assign Cost
          t (I.cId ids) <+> equals <+>
          (if (null path)
            then t . show . getCost $ p
            else
              foldr1
                (\x y -> x <+> t "+" <+> y)
                (map
                  (\(call, n) ->
                    t (I.nId ids) <> hcat call <> t ".cost"
                    <> parens(
                      case (getNodeReturnType (Ir.definitions ir) n) of
                        Just term  -> t (Lab.termToEnum $ nonTerminal term)
                        Nothing -> error ("\nERROR: costAndLabel() Node '"++ show n ++
                                      "' has no return type!\n")))
                  (path))
              <+> (if (Cost.isZero $ getCost p)
                then empty
                else t "+" <+> t (show $ getCost p)))
          <> semi
          -- Call label method
          $+$ t "label" <> parens (t (I.nId ids) <> comma <+> t (getResultLabel p) <> comma
          <+> t (I.cId ids) <> comma <+> t (getRuleLabel p)) <> semi

        
        {- | Produce code that is used within an 'if' statement to evaluate
             if a certain node should be labeled.
               Example:
                * if (n.child1().kind() = ADD && n.child1().child2().is(NT_REG) ...) -}
        iff :: Production -> Doc
        iff p =
          foldr1
            (\x y -> x <+> t "&&" <+> y)
            (map
              (\(call, n) ->
                if (isTerminal n)
                  then t (I.nId ids) <> hcat call <> t ".kind() ==" <+> t (Lab.termToEnum n)
                  else
                    t (I.nId ids) <> hcat call <> t ".is" <> parens(
                      case (getNodeReturnType (Ir.definitions ir) n) of
                        Just term  -> t (Lab.termToEnum $ nonTerminal term)
                        Nothing -> error "\nERROR: iff() Node has no return type!\n"))
              (childPath $ getNode p))

-----------------------------------------------------------------------------