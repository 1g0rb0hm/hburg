{
-----------------------------------------------------------------------------
-- |
-- Module      :  Lexer
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Tree pattern matching language ALEX lexer specification.
-----------------------------------------------------------------------------

module Parser.Lexer (
        -- * Classes
        TokenClass(..),
        -- * Types
        Token(..),
        -- * Functions
        scanner
    ) where

-----------------------------------------------------------------------------

import Csa.Elem (ElemClass(..))

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

-- terminals are all upper case plus digits and '-' '_'
@term = $upper [$upper \_ \- $digit]*

-- identifiers must start with a lower case character
@ident = $lower $idchar*

@keywords 
    = generator
    | declarations
    | operators
    | rules
    | end

-- Attribute Specific
$attridchar = [$idchar \< \>]

@attrident = $alpha $attridchar*

@attrkeyword = out


CODEGENERATOR :-
<0>  $white+        { skipit }
<0>  "--".*         { skipit }

-- Return a token containing complete semantic action
"(:"                { semanticAction }
"(."                { semanticAction } -- like Coco/R

-- Go to state <attr> when encountering an attribute
<0>"<:"             { attrStart }
<0>"<."             { attrStart } -- like Coco/R
<attr> $white+      { skipit }
<attr> @attrkeyword { mkL TAttrKeyword }
<attr> @attrident   { mkL TAttrIdent }
<attr> $comma       { mkL TComma }
<attr> ":>"         { attrEnd }
<attr> ".>"         { attrEnd } -- like Coco/R

<0> @keywords       { mkL TKeyword }

<0> $comma          { mkL TComma }
<0> $or             { mkL TOr }
<0> $openparen      { mkL TParenOpen }
<0> $closeparen     { mkL TParenClose }
<0> $openbox        { mkL TBoxOpen }
<0> $closebox       { mkL TBoxClose }
<0> $assign         { mkL TAssign }
<0> $colon          { mkL TColon }
<0> $period         { mkL TPeriod }

<0> @term           { mkL TTerm }
<0> @ident          { mkL TIdent }

<0> $digit+         { mkL TCost}

{
-- Each right-hand side has type :: AlexInput -> Int -> Alex result


-- | Token Datatype
data Token 
    = ConToken AlexPosn TokenClass String

instance Eq Token where
    (==) (ConToken pos1 c1 str1)
         (ConToken pos2 c2 str2)
        = if (and [pos1 == pos2, c1 == c2, str1 == str2])
            then True
            else False

instance Ord Token where
    compare t1@(ConToken (AlexPn _ l1 c1) _ _)
            t2@(ConToken (AlexPn _ l2 c2) _ _)
        = if (t1 == t2)
            then EQ
            else if (l1 < l2)
                    then LT
                    else if (l1 == l2)
                            then 
                                if (c1 < c2)
                                    then LT
                                    else GT
                            else GT

instance Show Token where
    show (ConToken pos cl s) 
        = (showPosn pos) ++ " - " ++ (show cl) ++ " '" ++ s

instance ElemClass Token where
    elemShow t = show t
    elemL (ConToken (AlexPn _ line _) _ _) = line
    elemC (ConToken (AlexPn _ _ col) _ _) = col

-- | Token Classes
data TokenClass
  = TCost
  | TSemAction
  | TKeyword
  | TIdent
  | TAttrStart
  | TAttrEnd
  | TAttrKeyword
  | TAttrIdent
  | TTerm
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
  deriving (Eq, Show)

---------------------------------------------------------
-- Lexer Utility functions
---------------------------------------------------------

-- | Easily create 'Alex Token'
mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p,_,str) len 
    = return (ConToken p c (take len str))

attrStart :: AlexInput -> Int -> Alex Token
attrStart inp@(p,_,str) pos 
    = do
        alexSetStartCode attr;
        return (ConToken p TAttrStart "<:")

attrEnd :: AlexInput -> Int -> Alex Token
attrEnd inp@(p,_,str) pos 
    = do
        alexSetStartCode 0;
        return (ConToken p TAttrEnd ":>")

-- @TODO: revert escaped \:\) which may be in semantic action
semanticAction :: AlexInput -> Int -> Alex Token
semanticAction _ _
    = do {
        input <- alexGetInput;
        go 1 [] input
    }
    where
        go :: Int -> [Char] -> AlexInput -> Alex Token
        go 0 xs input 
            = let (p,_,str) = input in
            do
                alexSetInput input;
                return (ConToken p TSemAction xs)
        go n xs input
            = do
                case alexGetChar input of
                    Nothing -> err input
                    Just (c,input) ->
                        do
                            case c of
                                ':' -> isCloseParen ':' input
                                '.' -> isCloseParen '.' input
                                c -> go n (xs ++ [c]) input
            where
                isCloseParen :: Char -> AlexInput -> Alex Token
                isCloseParen ch input
                    = do
                        case alexGetChar input of
                            Nothing -> err input
                            Just (')',inp) -> go 0 xs inp
                            Just (c,inp) -> go n (xs ++ [ch] ++ [c]) inp
            
        err :: AlexInput -> Alex Token
        err input
            = do
                alexSetInput input;
                lexError "Lexical Error in semantic action"

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col)
    = "[line:" ++ show line ++ " col:" ++ show col ++ "]"

lexError :: String -> Alex Token
lexError s
    = do
        (p,c,input) <- alexGetInput;
        alexError (s ++ " " ++ showPosn p ++ ": " ++
               (if (not (null input))
                 then " at charcter " ++ show (head input)
                 else " at end of file"))

alexEOF :: Alex Token
alexEOF = return (ConToken undefined TEOF "")

{-
    newtype Alex a = Alex { runAlex :: AlexState -> Either String (AlexState, a) }
    runAlex :: String -> Alex a -> Either String a
-}

-- | scanner. Tokenizes a String into an array of tokens
scanner :: String -> Either String [Token]
scanner str
    = runAlex str $
        do
            let accum xs = do {
                tok@(ConToken p cl s) <- monadScan;
                if cl == TEOF
                    then return xs
                    else do { accum (xs ++ [tok]) }
                }
            accum []

skipit :: AlexInput -> Int -> Alex Token
skipit input len = monadScan

monadScan :: Alex Token
monadScan
    = do
        inp@(p,c,input) <- alexGetInput
        sc <- alexGetStartCode
        case alexScan inp sc of
            AlexEOF -> alexEOF
            AlexError inp' -> lexError "Lexical Error at "
            AlexSkip  inp' len -> do
                            alexSetInput inp'
                            monadScan
            AlexToken inp' len action -> do
                            alexSetInput inp'
                            action inp len
}