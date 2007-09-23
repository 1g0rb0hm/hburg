{
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser
-- Copyright   :  (c) ByteLABS.org
-- License     :  BSD-style (see the file LICENSE)
-- 
-- The LR parser for our tree pattern matching grammar. It does some basic
-- semantic checking, e.g. duplicate bindings, type checking, etc.
-----------------------------------------------------------------------------
module Parser.Parser ( 
        -- * Types
        ParseResult(..),
        -- * Functions
        parse
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Maybe (isJust, fromJust)

import Util (stringToInt)

import Ast.Op (Operator, op, opMap)
import qualified Ast.Incl as Incl (Include, new)
import qualified Ast.Ident as Id (toIdent)
import qualified Ast.Bind as B (new, empty, getIdent)
import qualified Ast.Attr as A (Attr, AttrTy(..), new, ty, emptyTy)
import qualified Ast.Code as C (Code, new, empty, isEmpty)
import qualified Ast.Decl as Decl (new)
import qualified Ast.Def as Def (Definition, new, mergeDefs)
import qualified Ast.Nt as Nt (new)
import qualified Ast.T as T (new)
import Ast.Term (Term, TermClass(..), terminal, nonTerminal)
import qualified Ast.Node as N (Node, TreeClass(..), new, setLink, addLinkCode)
import Ast.Prod (Production, prod)
import Ast.Cost as Cost (Cost, static, dynamic)
import Ast.Ir (Ir(..), OperatorMap)

import qualified Csa.Csa as Csa (updateCtx, checkCtx, checkDef, checkProd)

import qualified Csa.Ctx as Ctx (Ctx, new, empty, merge)
import qualified Csa.Elem as Elem (new)

import Parser.Lexer (Token(..), TokenClass(..))
import Parser.ParseErr (parseErrDupBind, parseErrTok, parseErrRedefinition)
-----------------------------------------------------------------------------
}

%name parse
%monad { P } { thenP } { returnP }
%tokentype { Token }
%token 
    cost        { ConToken _ TCost _ }
    sem         { ConToken _ TSemAction _ }
    generator   { ConToken _ TKeyword "generator" }
    declarations{ ConToken _ TKeyword "declarations" }
    operators   { ConToken _ TKeyword "operators" }
    rules       { ConToken _ TKeyword "rules" }
    end         { ConToken _ TKeyword "end" }
    ident       { ConToken _ TIdent _ }
    term        { ConToken _ TTerm _ }
    attrident   { ConToken _ TAttrIdent _ }
    out         { ConToken _ TAttrKeyword "out" }
    '<'         { ConToken _ TAttrStart "<:" }
    '>'         { ConToken _ TAttrEnd ":>" }
    ','         { ConToken _ TComma _ }
    '|'         { ConToken _ TOr _ }
    '['         { ConToken _ TBoxOpen _ }
    ']'         { ConToken _ TBoxClose _ }
    '('         { ConToken _ TParenOpen _ }
    ')'         { ConToken _ TParenClose _ }
    '='         { ConToken _ TAssign _ }
    ':'         { ConToken _ TColon _ }
    '.'         { ConToken _ TPeriod _ }
%%

-------------------------------------------------------------------
--
-- The Generator itself
--
G :: { Ir }
    : generator
        Incl
      declarations
        Sem
      operators
        Ops
      rules
        Ds
      end
        {%
            let (ops, opctx) = $6 in                    -- Operators and their Context
            let (defs, defctx, opmap) = $8 in           -- Definitions and their Context
            let debugMsg = foldr (++) "" (map (\d -> show d) defs) ++
                        "\n\nDefinition " ++ show defctx ++
                        "\n\nOperator " ++ show opctx
                in                                      -- if debug cli option is defined
            case (Ctx.merge defctx opctx) of
                Right ctx ->
                    case Csa.checkCtx defs ctx of
                        Nothing -> returnP Ir { include = $2
                                              , declaration = Decl.new $4
                                              , operators = ops
                                              , definitions = (reverse defs)
                                              , debug = debugMsg
                                              , operatorMap = opmap }
                        Just errors -> failP (foldr1 (\e old -> e ++ "\n" ++ old) errors)
                Left (el1, el2) -> error "\nERROR: Merging of Definition and Operator Context failed!\n"
        }

--
-- Include statements
--
Incl :: { Incl.Include }
    : Sem
        { Incl.new $1 }

--
-- Operators are stored in Contexts
--
Ops :: { ([Operator], Ctx.Ctx) }
    : Op
        { (\(op, ctx) -> ([op], ctx)) ($1) }
    | Ops ',' Op
        {%
            let (ops, ctx) = $1 in
            let (nops, nctx) = $3 in
            case (Ctx.merge ctx nctx) of
                Right e -> returnP ( nops : ops, e)
                Left (el1, el2) -> errP (parseErrDupBind "Operator" el2 el1) (ops, ctx)
        }

Op :: { (Operator, Ctx.Ctx) }
    : term Sem
        {
            if (C.isEmpty $2)
                then
                    let o = op (Id.toIdent $1) in
                    (o, Ctx.new (Elem.new o))
                else
                    let o = opMap (Id.toIdent $1) $2 in
                    (o, Ctx.new (Elem.new o))
        }

-------------------------------------------------------------------

--
-- Definitions of non terminals
--
Ds :: { ([ Def.Definition ], Ctx.Ctx, OperatorMap) }
    : D
        { (\(d, ctx, opmap) -> ([d], ctx, opmap)) $1 }
    | Ds D
        {%
            let (ndef, nctx, opmap1) = $2 in
            let (odefs, octx, opmap2) = $1 in
            let opmap = M.unionWith (\a1 a2 -> S.union a1 a2) opmap1 opmap2 in
            -- CSA: Check for possible erroneous redefinitions, if this check
            --      fails we call failP instead of errP because otherwise we
            --      would get loads of subsequent errors due to missing definitions
            --      in our Context. This would only confuse the user.
            case (Ctx.merge nctx octx) of
                Right e -> 
                    case Def.mergeDefs odefs ndef of
                        Right defs -> returnP (defs, e, opmap)
                        Left (n1, n2) -> failP (parseErrRedefinition "redefined at" (n1) (n2))
                Left (el1, el2) -> failP (parseErrDupBind "Non Terminal" el1 el2)
        }

D :: { (Def.Definition, Ctx.Ctx, OperatorMap) }
    : ident Sem '=' Prods '.'
        {%
            let def = Def.new (Id.toIdent $1) [] $2 (fst $4) in
            let ctx = Ctx.new (Elem.new def) in
            case Csa.checkDef def of
                Nothing -> returnP (def, ctx, snd $4)
                Just err -> errP (err) (def, ctx, snd $4)
        }
    | ident '<' Ads '>' Sem '=' Prods  '.'
        {%
            let def = Def.new (Id.toIdent $1) $3 $5 (fst $7) in
            let ctx = Ctx.new (Elem.new def) in
            case Csa.checkDef def of
                Nothing -> returnP (def, ctx, snd $7)
                Just err -> errP (err) (def, ctx, snd $7)
        }

-------------------------------------------------------------------

--
-- Productions
--
Prods :: { ([ Production ], OperatorMap)  }
    : Prod
        { ([ fst $1 ], snd $1) }
    | Prods '|' Prod
        {%
            -- CSA: Check if all productions with the same ident have the same
            --        amount of parameters.
            case Csa.checkProd (fst $1) (fst $3) of
                Right prods ->
                    returnP (prods, M.unionWith (\a1 a2 -> S.union a1 a2) (snd $1) (snd $3))
                Left (n1, n2) ->
                    errP (parseErrRedefinition
                        "redefined with different amount of parameters at"
                        (n1) (n2))
                        (((fst $3):(fst $1)), M.unionWith (\a1 a2 -> S.union a1 a2) (snd $1) (snd $3))
        }

Prod :: { (Production, OperatorMap) }
    : Sem T Sem ':' Cost
        {   (prod (N.new $2 $1 $3 N.empty C.empty N.empty C.empty) $5,
             M.singleton 0 $ S.singleton $ op (getId $2)) }
    | Sem T Sem '[' Sem Nt Sem ']' Sem ':' Cost
        {%
            let link = (N.new $6 C.empty C.empty N.empty C.empty N.empty C.empty) in
            let n = (N.new $2 $1 $3 N.empty C.empty N.empty $9) in
            let p = prod (N.setLink (N.addLinkCode n $5 $7) link) $11 in
            let opmap = M.singleton 0 $ S.singleton $ op (getId $2) in
            -- CSA: check duplicate bindings for T and Nt
            if (equalBindings $2 $6)
                then errP (parseErrDupBind "Binding"
                            (Elem.new (B.getIdent (getBinding $6)))
                            (Elem.new (B.getIdent (getBinding $2))))
                            (p, opmap)
                else returnP (p, opmap)
        }
    | Sem T Sem Pat Sem ':' Cost
        {%
            let (ns, ctx, opmap) = $4 in
            let n = N.new $2 $1 $3 ns $5 N.empty C.empty in
            let p = prod n $7 in
            let opmap' = updateOpMap n opmap in
            -- CSA: check duplicate bindings
            case (Csa.updateCtx $2 ctx) of
                Right _ -> returnP (p, opmap')
                Left (el1 , el2) -> errP (parseErrDupBind "Binding" el1 el2) (p, opmap')
        }
    | Sem T Sem Pat Sem '[' Sem Nt Sem ']' Sem ':' Cost
        {%
            let link = N.new $8 C.empty C.empty N.empty C.empty N.empty C.empty in
            let (child, ctx, opmap) = $4 in
            let n = N.setLink (N.addLinkCode (N.new $2 $1 $3 child $5 N.empty $11) $7 $9) link in
            let p = prod n $13 in
            let opmap' = updateOpMap n opmap in
            -- CSA: check duplicate bindings
            -- 1: Check binding clashes for T in Context
            case (Csa.updateCtx $2 ctx) of
                Left (el1 , el2) -> errP (parseErrDupBind "Binding" el1 el2) (p, opmap')
                Right ctx1 ->
                        -- 2: Check binding clashes for Nt in Context extended with T's binding
                        case (Csa.updateCtx $8 ctx1) of
                            Left (el1 , el2) -> errP (parseErrDupBind "Binding" el2 el1) (p, opmap')
                            Right _ -> returnP (p, opmap')
        }
    | Sem Nt Sem ':' Cost
        { (prod (N.new $2 $1 $3 N.empty C.empty N.empty C.empty) $5, M.empty) }
    | Sem Nt Sem '[' Sem Nt Sem ']' Sem ':' Cost
        {%
            let link = (N.new $6 C.empty C.empty N.empty C.empty N.empty C.empty) in
            let n = N.new $2 $1 $3 N.empty C.empty N.empty $9 in
            let p = prod (N.setLink (N.addLinkCode n $5 $7) link) $11 in
            -- CSA: check duplicate bindings for Nt and Nt
            if (equalBindings $2 $6)
                then errP (parseErrDupBind "Binding"
                            (Elem.new (B.getIdent (getBinding $6)))
                            (Elem.new (B.getIdent (getBinding $2))))
                            (p, M.empty)
                else returnP (p, M.empty)
        }

-------------------------------------------------------------------

--
-- Patterns
--
Pat :: { (N.Node, Ctx.Ctx, OperatorMap) }
    : '(' Sem Nt Sem PatSeq ')'
        {%
            let (ns, ctx, opmap) = $5 in
            let n =  N.new $3 $2 $4 N.empty C.empty ns C.empty in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e, opmap)
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx, opmap)
        }
    | '(' Sem T Sem PatSeq ')'
        {% 
            let (ns, ctx, opmap) = $5 in
            let n = N.new $3 $2 $4 N.empty C.empty ns C.empty in
            let opmap' = updateOpMap n opmap in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e, opmap')
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx, opmap')
        }
    | '(' Sem T Sem Pat Sem PatSeq ')'
        {%
            let (ns1, ctx1, opmap1) = $5 in
            let (ns2, ctx2, opmap2) = $7 in
            let n = N.new $3 $2 $4 ns1 $6 ns2 C.empty in
            let opmap' = updateOpMap n (M.unionWith (\a1 a2 -> S.union a1 a2) opmap1 opmap2) in
            -- CSA: Check for duplicate bindings
            case Ctx.merge ctx2 ctx1 of
                Left (e1, e2) -> errP (parseErrDupBind "Binding" e1 e2) (n, ctx1, opmap')
                Right ctx ->
                    case (Csa.updateCtx $3 ctx) of
                        Right e -> returnP (n, e, opmap')
                        Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx, opmap')
        }

--
-- Pattern Sequences
--
PatSeq :: { (N.Node, Ctx.Ctx, OperatorMap) }
    : {- empty -}
        { (N.empty, Ctx.empty , M.empty) }
    | ',' Sem Nt Sem PatSeq
        {% 
            let (ns, ctx, opmap) = $5 in
            let n = N.new $3 $2 $4 N.empty C.empty ns C.empty in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e, opmap)
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx, opmap)
        }
    | ',' Sem T Sem PatSeq
        {%
            let (ns, ctx, opmap) = $5 in
            let n = N.new $3 $2 $4 N.empty C.empty ns C.empty in
            let opmap' = updateOpMap n opmap in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e, opmap')
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx, opmap')
        }
    | ',' Sem T Sem Pat Sem PatSeq
        {%
            let (ns1, ctx1, opmap1) = $5 in
            let (ns2, ctx2, opmap2) = $7 in
            let n = N.new $3 $2 $4 ns1 $6 ns2 C.empty in
            let opmap' = updateOpMap n (M.unionWith (\a1 a2 -> S.union a1 a2) opmap1 opmap2) in
            -- CSA: Check for duplicate bindings
            case Ctx.merge ctx2 ctx1 of
                Left (e1, e2) -> failP (parseErrDupBind "Binding" e1 e2)
                Right ctx ->
                    case (Csa.updateCtx $3 ctx) of
                        Right e -> returnP (n, e, opmap')
                        Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx, opmap')
        }

-------------------------------------------------------------------

--
-- Non Terminals
--
Nt :: { Term }
    : ident                     { nonTerminal (Nt.new (Id.toIdent $1) B.empty []) }
    | ident ident               { nonTerminal (Nt.new (Id.toIdent $1) (B.new (Id.toIdent $2)) []) }
    | ident '<' As '>'          { nonTerminal (Nt.new (Id.toIdent $1) B.empty $3) }
    | ident '<' As '>' ident    { nonTerminal (Nt.new (Id.toIdent $1) (B.new (Id.toIdent $5)) $3) }
--
-- Terminals
--
T :: { Term }
    : term          { terminal (T.new (Id.toIdent $1) B.empty)  }
    | term ident    { terminal (T.new (Id.toIdent $1) (B.new (Id.toIdent $2))) }

-------------------------------------------------------------------

--
-- Attributes
--
As :: { [A.Attr] }
    : A         { [ $1 ] }
    | As ',' A  { $1 ++ [ $3 ] }
A :: { A.Attr }
    : attrident     { A.new (Id.toIdent $1) A.InAttr A.emptyTy }
    | out attrident { A.new (Id.toIdent $2) A.OutAttr A.emptyTy }
    | Ad            { $1 }

--
-- Attribute Definitions
--
Ads :: { [A.Attr] }
    : Ad            { [ $1 ] }
    | Ads ',' Ad    { $1 ++ [ $3 ] }

Ad :: { A.Attr }
    : attrident attrident        { A.new (Id.toIdent $2) A.InAttr (A.ty (Id.toIdent $1)) }
    | out attrident attrident    { A.new (Id.toIdent $3) A.OutAttr (A.ty (Id.toIdent $2))}

-------------------------------------------------------------------
--
-- Cost definition
--
Cost :: { Cost.Cost }
    : cost      { Cost.static (stringToInt (show (Id.toIdent $1))) }
    | Sem       { Cost.dynamic $1 }

--
-- Semantic action
--
Sem :: { C.Code }
    : {- empty -}   { C.empty }
    | sem           { C.new (show (Id.toIdent $1)) }
{
-----------------------------------------------------------------------------

-- | Monad P deals with Parse Results
type P a = ParseResult a

-- | ParseResult type
data ParseResult a
    = ParseOk a             -- ^ Successful parse
    | ParseErr [String] a   -- ^ Parse contained errors
    | ParseFail String      -- ^ Fatal error happened
    deriving (Show, Eq, Ord)

thenP :: P a -> (a -> P b) -> P b
m `thenP` k
    = case m of
        ParseOk a -> k a                -- Indicates sucessful parse
        ParseErr err a ->               -- Indicates CSA errors
            case k a of
                ParseOk a -> ParseErr err a
                ParseErr nerr a -> ParseErr (err ++ nerr) a
                ParseFail errmsg -> ParseFail (concat (err ++ [errmsg]))
        ParseFail err -> ParseFail err  -- Indicates a serious CSA error

returnP :: a -> P a
returnP ok
    = ParseOk ok

failP :: String -> P a
failP err
    = ParseFail err

errP :: String -> a -> P a
errP errmsg rest
    = ParseErr [ errmsg ] rest

updateOpMap :: N.Node -> OperatorMap -> OperatorMap
updateOpMap n opmap
    = M.alter
        (\a ->
            if (isJust a)
                then Just $ S.insert (op $ getId n) (fromJust a)
                else Just $ S.singleton (op $ getId n))
        (length (N.getChildren n))
        opmap

-- Called by Happy if a parse error occurs
happyError :: [Token] -> P a
happyError [] = failP ("\nParse Error at unknown token? Sorry!\n")
happyError (tok:toks) 
    = failP (parseErrTok tok (show (Id.toIdent tok)))
}