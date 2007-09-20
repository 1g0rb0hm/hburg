-----------------------------------------------------------------------------
-- |
-- Module      :  ParseErr
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module contains various error text generation functions.
-----------------------------------------------------------------------------

module Parser.ParseErr (
        -- * Functions
        parseErrDupBind,
        parseErrRedefinition,
        parseErrTok,
        parseErrElem,
        typeError,
    ) where

-----------------------------------------------------------------------------

import Parser.Lexer(Token)
import Ast.Node(Node, showAsFun)
import Env.Env(ElemClass(..), Elem, envElem)

-- | typeError. Produces error message upon type error.
typeError :: Elem -> Int -> String -> String
typeError e i msg
    = "\nType error at [line:" ++ show (elemL e) ++
        " col:" ++ show (elemC e) ++ "] parameter " ++ 
        show i ++ " of " ++ msg ++ "\n"

-- | parseErrRedefinition. Produces error message upon the
--      redefinition of a Node.
parseErrRedefinition :: String -> Node -> Node -> String
parseErrRedefinition str n1 n2
    = parseErrElem
            (envElem n1)
            ("'" ++ showAsFun n1 ++ "' " ++ str ++ " " ++
            "[line:" ++ (show (elemL (envElem n2))) ++
            " col:" ++ (show (elemC (envElem n2))) ++ 
            "], namely as '" ++
            showAsFun n2 ++ "'.")

-- | parseErrDupBind. Produces error message when encountering
--      a duplicate binding.
parseErrDupBind :: String -> Elem -> Elem -> String
parseErrDupBind str e1 e2
    = parseErrElem
            (e1)
            (str ++ " '" ++ (elemShow e1) ++ "' already defined at " ++
            "[line:" ++ (show (elemL e2)) ++
            " col:" ++ (show (elemC e2)) ++ "].")

-- | parseErrTok.
parseErrTok :: Token -> String -> String
parseErrTok tok msg = parseErrElem (envElem tok) msg

-- | parseErrElem. Generic error message.
parseErrElem :: Elem -> String -> String
parseErrElem e msg
    = "\nParse error at [line:" ++ show (elemL e) ++
        " col:" ++ show (elemC e) ++ "]: " ++ msg ++ "\n"