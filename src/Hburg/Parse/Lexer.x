{
-----------------------------------------------------------------------------
-- |
-- Module      :  Lexer
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Tree pattern matching language ALEX lexer specification.
-----------------------------------------------------------------------------

module Hburg.Parse.Lexer (
  -- * Types
  TokenTy(..),
  Token(..),
  -- * Functions
  scanner
) where

{- unqualified imports  -}
import Hburg.Csa.Elem (ElemClass(..))

{- qualified imports  -}

-----------------------------------------------------------------------------

}

%wrapper "monad"

$digit = 0-9
$comma      = \,
$or         = \|
$openparen  = \(
$closeparen = \)
$openbox    = \[
$closebox   = \]
$assign     = \=
$colon      = \:
$period     = \.
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]

$idchar = [$alpha $digit \_ \-]
$upperidchar = [$upper $digit]
-- parentheses allow for method calls (useful for IN parameters)
$attridchar = [$alpha $digit \_ \- \( \)] 

@term = $upper [$upper \_ \- $digit]*
@ident = $lower $idchar*
@keywords =
  generator
  | declarations
  | operators
  | rules
  | end

-- Attributes
@idsep = \. $attridchar+
@idsepseq = $attridchar+ @idsep*
@generics = \< @idsepseq \> -- Java generics
@attrident = @idsepseq @generics?
@attrkeyword = out

-----------------------------------------------------------------------------
CODEGENERATOR :-
  <0>  $white+        ;
  <0>  "--".*         ;

  -- Return a token containing complete semantic action
  "(:"                { semAction ':' }
  "(."                { semAction '.' } -- like Coco/R

  -- Go to state <attr> when encountering an attribute
  <0>"<:"             { attrStart }
  <0>"<."             { attrStart } -- like Coco/R
  <attr> $white+      ;
  <attr> @attrkeyword { \t@(_,_,s) n -> mkT (TAttrKeyword $ take n s) t n }
  <attr> @attrident   { \t@(_,_,s) n -> mkT (TAttrIdent $ take n s) t n }
  <attr> $comma       { mkT TComma }
  <attr> ":>"         { attrEnd }
  <attr> ".>"         { attrEnd } -- like Coco/R

  <0> @keywords       { \t@(_,_,s) n -> mkT (TKeyword $ take n s) t n }

  <0> $comma          { mkT TComma }
  <0> $or             { mkT TOr }
  <0> $openparen      { mkT TParenOpen }
  <0> $closeparen     { mkT TParenClose }
  <0> $openbox        { mkT TBoxOpen }
  <0> $closebox       { mkT TBoxClose }
  <0> $assign         { mkT TAssign }
  <0> $colon          { mkT TColon }
  <0> $period         { mkT TPeriod }

  <0> @term           { \t@(_,_,s) n -> mkT (TTerm  $ take n s) t n }
  <0> @ident          { \t@(_,_,s) n -> mkT (TIdent $ take n s) t n }

  <0> $digit+         { \t@(_,_,s) n -> mkT (TCost  $ take n s) t n }

{
-----------------------------------------------------------------------------

{- | Easily create 'Alex Token's -}
mkT :: TokenTy -> AlexInput -> Int -> Alex Token
mkT tok (pos,_,_) _ = return $ MkToken pos tok

-----------------------------------------------------------------------------

{- data AlexPosn = AlexPn address::!Int line::!Int col::!Int -}

-- | Token Type
data TokenTy =
  TCost String
  | TSemAction String
  | TKeyword String
  | TIdent String
  | TAttrStart
  | TAttrEnd
  | TAttrKeyword String
  | TAttrIdent String
  | TTerm String
  | TComma
  | TOr
  | TParenOpen
  | TParenClose
  | TBoxOpen
  | TBoxClose
  | TAssign
  | TColon
  | TPeriod
  | TEOF
  deriving (Eq,Ord)

{- | Token Datatype -}
data Token =
  MkToken
    AlexPosn -- ^ token position information
    TokenTy  -- ^ token

-----------------------------------------------------------------------------

instance Show TokenTy where
  show (TCost s) = s
  show (TSemAction s) = s
  show (TKeyword s) = s
  show (TIdent s) = s
  show (TAttrKeyword s) = s
  show (TAttrIdent s) = s
  show (TTerm s) = s
  show TAttrStart = "<."
  show TAttrEnd = ".>"
  show TComma = ","
  show TOr = "|"
  show TParenOpen = "("
  show TParenClose = ")"
  show TBoxOpen = "["
  show TBoxClose = "]"
  show TAssign = "="
  show TColon = ":"
  show TPeriod = "."
  show TEOF = "EOF"

instance Show Token where
  show (MkToken pos tok) = showPosn pos ++" - "++ show tok

instance Eq Token where
  (==) (MkToken p1 t1) (MkToken p2 t2) = p1 == p2 && t1 == t2

instance Ord Token where
  compare t1@(MkToken (AlexPn _ l1 c1) _)
          t2@(MkToken (AlexPn _ l2 c2) _) =
    if (t1 == t2)
      then EQ
      else if (l1 < l2)
        then LT
        else if (l1 == l2)
          then if (c1 < c2) then LT else GT
          else GT

instance ElemClass Token where
  elemShow t = show t
  elemL (MkToken (AlexPn _ l _) _) = l
  elemC (MkToken (AlexPn _ _ c) _) = c

-----------------------------------------------------------------------------

attrStart :: AlexInput -> Int -> Alex Token
attrStart (p,_,_) _ = do
  alexSetStartCode attr
  return $ MkToken p TAttrStart

attrEnd :: AlexInput -> Int -> Alex Token
attrEnd (p,_,_) _ = do
  alexSetStartCode 0
  return $ MkToken p TAttrEnd

-- @TODO: revert escaped \:\) which may be in semantic action
semAction :: Char -> AlexInput -> Int -> Alex Token
semAction dot _ _ = do
  inp <- alexGetInput
  scan [] inp
  where
    scan :: [Char] -> AlexInput -> Alex Token
    scan cs inp = do
      case alexGetChar inp of
        Just (c1,inp)
          | c1 == dot ->
          case alexGetChar inp of
            Just (c2,inp)
              | c2 == ')' -> done cs inp
            Just (c2,inp) -> scan (c2:c1:cs) inp
            Nothing -> semError inp
        Just (c1,inp) -> scan (c1:cs) inp
        Nothing -> semError inp

    done :: [Char] -> AlexInput -> Alex Token
    done cs inp@(p,_,_) = do
      alexSetInput inp
      return $ MkToken p $ TSemAction $ reverse cs

    semError :: AlexInput -> Alex Token
    semError inp = do
      alexSetInput inp
      alexError' "Lexical Error in semantic action"

showPosn :: AlexPosn -> String
showPosn (AlexPn _ l c) = "[line:"++ show l ++" col:"++ show c ++"]"

alexError' :: String -> Alex Token
alexError' s = do
  (p,_,inp) <- alexGetInput
  alexError $ s ++" "++ showPosn p ++": "++
    (if (null inp)
      then " at EOF"
      else " at charcter "++ take 1 inp)

alexEOF :: Alex Token
alexEOF = return $ MkToken undefined TEOF

{- | Tokenize String into [Token]s -}
scanner :: String -> Either String [Token]
scanner str =
  let accum ts =
        do
          t@(MkToken _ tok) <- monadScan
          if tok == TEOF
            then return . reverse $ ts
            else accum (t:ts)
  in
  runAlex str $ accum []

{- | Custom monadScan version  -}
monadScan :: Alex Token
monadScan = do
  inp <- alexGetInput
  sc  <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError inp' -> alexError' "Lexical Error at "
    AlexSkip  inp' _ -> do {alexSetInput inp'; monadScan}
    AlexToken inp' len action -> do {alexSetInput inp'; action inp len}
}