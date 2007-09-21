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

import Util (stringToInt)

import Ast.Op (Operator, op, opMap)
import qualified Ast.Incl as Incl (Include, new)
import qualified Ast.Ident as Id (toIdent)
import qualified Ast.Bind as B (new, empty, getIdent)
import qualified Ast.Attr as A (Attr, AttrTy(..), new, ty, emptyTy)
import qualified Ast.Code as C (Code, new, empty, isEmpty)
import qualified Ast.Decl as Decl (Declaration, new)
import qualified Ast.Def as Def (Definition, new, mergeDefs)
import qualified Ast.Nt as Nt (new)
import qualified Ast.T as T (new)
import Ast.Term (Term, TermClass(..), terminal, nonTerminal)
import qualified Ast.Node as N (Node, TreeClass(..), new, setLink, addLinkBlockCode)
import Ast.Prod (Production, prod)
import Ast.Cost as Cost (Cost, static, dynamic)

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
G :: { (Incl.Include, Decl.Declaration, [Operator], [Def.Definition], String) }
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
            let incl = $2 in            -- Includes
            let decl = Decl.new $4 in   -- Declarations
            let (ops, opctx) = $6 in    -- Operators and their Context
            let (defs, defctx) = $8 in  -- Definitions and their Context
            let debug = foldr (++) "" (map (\d -> show d) defs) ++
                        "\n\nDefinition " ++ show defctx ++
                        "\n\nOperator " ++ show opctx
                in                      -- if debug cli option is defined
            case (Ctx.merge defctx opctx) of
                Right ctx ->
                    case Csa.checkCtx defs ctx of
                        Nothing -> returnP (incl, decl, ops, (reverse defs), debug)
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
Ds :: { ([ Def.Definition ], Ctx.Ctx) }
    : D
        { (\(def, ctx) -> ([def], ctx)) ($1) }
    | Ds D
        {%
            let (ndef, nctx) = $2 in
            let (odefs, octx) = $1 in
            -- CSA: Check for possible erroneous redefinitions, if this check
            --      fails we call failP instead of errP because otherwise we
            --      would get loads of subsequent errors due to missing definitions
            --      in our Context. This would only confuse the user.
            case (Ctx.merge nctx octx) of
                Right e -> 
                    case Def.mergeDefs odefs ndef of
                        Right defs -> returnP (defs, e)
                        Left (n1, n2) -> failP (parseErrRedefinition "redefined at" (n1) (n2))
                Left (el1, el2) -> failP (parseErrDupBind "Non Terminal" el1 el2)
        }

D :: { (Def.Definition, Ctx.Ctx) }
    : ident Sem '=' Prods '.'
        {%
            let def = Def.new (Id.toIdent $1) [] $2 $4 in
            let ctx = Ctx.new (Elem.new def) in
            case Csa.checkDef def of
                Nothing -> returnP (def, ctx)
                Just err -> errP (err) (def, ctx)
        }
    | ident '<' Ads '>' Sem '=' Prods  '.'
        {%
            let def = Def.new (Id.toIdent $1) $3 $5 $7 in
            let ctx = Ctx.new (Elem.new def) in
            case Csa.checkDef def of
                Nothing -> returnP (def, ctx)
                Just err -> errP (err) (def, ctx)
        }

-------------------------------------------------------------------

--
-- Productions
--
Prods :: { [ Production ]  }
    : Prod
        { [ $1 ] }
    | Prods '|' Prod
        {%
            -- CSA: Check if all productions with the same ident have the same
            --        amount of parameters.
            case Csa.checkProd $1 $3 of
                Right prods -> returnP prods
                Left (n1, n2) ->
                    errP (parseErrRedefinition
                        "redefined with different amount of parameters at"
                        (n1) (n2))
                        ($3:$1)
        }

Prod :: { Production }
    : Sem T Sem ':' Cost
        {   prod (N.new $2 $1 $3 N.empty C.empty N.empty C.empty) $5 }
    | Sem T Sem '[' Sem Nt Sem ']' Sem ':' Cost
        {%
            let link = (N.new $6 C.empty C.empty N.empty C.empty N.empty C.empty) in
            let n = (N.new $2 $1 $3 N.empty C.empty N.empty $9) in
            let p = prod (N.setLink (N.addLinkBlockCode n $5 $7) link) $11 in
            -- CSA: check duplicate bindings for T and Nt
            if (equalBindings $2 $6)
                then errP (parseErrDupBind "Binding"
                            (Elem.new (B.getIdent (getBinding $6)))
                            (Elem.new (B.getIdent (getBinding $2))))
                            (p)
                else returnP p
        }
    | Sem T Sem Pat Sem ':' Cost
        {%
            let (ns, ctx) = $4 in
            let n = N.new $2 $1 $3 ns $5 N.empty C.empty in
            let p = prod n $7 in
            -- CSA: check duplicate bindings
            case (Csa.updateCtx $2 ctx) of
                Right _ -> returnP p
                Left (el1 , el2) -> errP (parseErrDupBind "Binding" el1 el2) (p)
        }
    | Sem T Sem Pat Sem '[' Sem Nt Sem ']' Sem ':' Cost
        {%
            let link = N.new $8 C.empty C.empty N.empty C.empty N.empty C.empty in
            let (child, ctx) = $4 in
            let n = N.setLink (N.addLinkBlockCode (N.new $2 $1 $3 child $5 N.empty $11) $7 $9) link in
            let p = prod n $13 in
            -- CSA: check duplicate bindings
            -- 1: Check binding clashes for T in Context
            case (Csa.updateCtx $2 ctx) of
                Left (el1 , el2) -> errP (parseErrDupBind "Binding" el1 el2) (p)
                Right ctx1 ->
                        -- 2: Check binding clashes for Nt in Context extended with T's binding
                        case (Csa.updateCtx $8 ctx1) of
                            Left (el1 , el2) -> errP (parseErrDupBind "Binding" el2 el1) (p)
                            Right _ -> returnP p
        }
    | Sem Nt Sem ':' Cost
        { prod (N.new $2 $1 $3 N.empty C.empty N.empty C.empty) $5 }
    | Sem Nt Sem '[' Sem Nt Sem ']' Sem ':' Cost
        {%
            let link = (N.new $6 C.empty C.empty N.empty C.empty N.empty C.empty) in
            let n = N.new $2 $1 $3 N.empty C.empty N.empty $9 in
            let p = prod (N.setLink (N.addLinkBlockCode n $5 $7) link) $11 in
            -- CSA: check duplicate bindings for Nt and Nt
            if (equalBindings $2 $6)
                then errP (parseErrDupBind "Binding"
                            (Elem.new (B.getIdent (getBinding $6)))
                            (Elem.new (B.getIdent (getBinding $2))))
                            (p)
                else returnP p
        }

-------------------------------------------------------------------

--
-- Patterns
--
Pat :: { (N.Node, Ctx.Ctx) }
    : '(' Sem Nt Sem PatSeq ')'
        {%
            let (ns, ctx) = $5 in
            let n =  N.new $3 $2 $4 N.empty C.empty ns C.empty in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e)
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx)
        }
    | '(' Sem T Sem PatSeq ')'
        {% 
            let (ns, ctx) = $5 in
            let n = N.new $3 $2 $4 N.empty C.empty ns C.empty in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e)
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx)
        }
    | '(' Sem T Sem Pat Sem PatSeq ')'
        {%
            let (ns1, ctx1) = $5 in
            let (ns2, ctx2) = $7 in
            let n = N.new $3 $2 $4 ns1 $6 ns2 C.empty in
            -- CSA: Check for duplicate bindings
            case Ctx.merge ctx2 ctx1 of
                Left (e1, e2) -> errP (parseErrDupBind "Binding" e1 e2) (n,ctx1)
                Right ctx ->
                    case (Csa.updateCtx $3 ctx) of
                        Right e -> returnP (n, e)
                        Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx)
        }

--
-- Pattern Sequences
--
PatSeq :: { (N.Node, Ctx.Ctx) }
    : {- empty -}
        { (N.empty, Ctx.empty ) }
    | ',' Sem Nt Sem PatSeq
        {% 
            let (ns, ctx) = $5 in
            let n = N.new $3 $2 $4 N.empty C.empty ns C.empty in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e)
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx)
        }
    | ',' Sem T Sem PatSeq
        {%
            let (ns, ctx) = $5 in
            let n = N.new $3 $2 $4 N.empty C.empty ns C.empty in
            -- CSA: Check for duplicate bindings
            case (Csa.updateCtx $3 ctx) of
                Right e -> returnP (n, e)
                Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx)
        }
    | ',' Sem T Sem Pat Sem PatSeq
        {%
            let (ns1, ctx1) = $5 in
            let (ns2, ctx2) = $7 in
            let n = N.new $3 $2 $4 ns1 $6 ns2 C.empty in
            -- CSA: Check for duplicate bindings
            case Ctx.merge ctx2 ctx1 of
                Left (e1, e2) -> failP (parseErrDupBind "Binding" e1 e2)
                Right ctx ->
                    case (Csa.updateCtx $3 ctx) of
                        Right e -> returnP (n, e)
                        Left (el1, el2) -> errP (parseErrDupBind "Binding" el1 el2) (n, ctx)
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

-- Called by Happy if a parse error occurs
happyError :: [Token] -> P a
happyError [] = failP ("\nParse Error at unknown token? Sorry!\n")
happyError (tok:toks) 
    = failP (parseErrTok tok (show (Id.toIdent tok)))
}