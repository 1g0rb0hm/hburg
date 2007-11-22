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

import Parser.Lexer(Token)
import Ast.Node(Node, showAsFun)

import qualified Csa.Elem as E (ElemClass(..), Elem, new)

-----------------------------------------------------------------------------

-- | typeError. Produces error message upon type error.
typeError :: E.Elem -> Int -> String -> String
typeError e i msg
    = "\nType error at [line:"++ show (E.elemL e) ++
        " col:"++ show (E.elemC e) ++"] parameter "++ 
        show i ++" of "++ msg ++"\n"

-- | parseErrRedefinition. Produces error message upon the
--      redefinition of a Node.
parseErrRedefinition :: String -> Node -> Node -> String
parseErrRedefinition str n1 n2
    = parseErrElem
            (E.new n1)
            ("'"++ showAsFun n1 ++"' "++ str ++" "++
            "[line:"++ (show (E.elemL (E.new n2))) ++
            " col:"++ (show (E.elemC (E.new n2))) ++ 
            "], namely as '"++
            showAsFun n2 ++"'.")

-- | parseErrDupBind. Produces error message when encountering
--      a duplicate binding.
parseErrDupBind :: String -> E.Elem -> E.Elem -> String
parseErrDupBind str e1 e2
    = parseErrElem
            (e1)
            (str ++" '"++ (E.elemShow e1) ++"' already defined at "++
            "[line:"++ (show (E.elemL e2)) ++
            " col:"++ (show (E.elemC e2)) ++"].")

-- | parseErrTok.
parseErrTok :: Token -> String -> String
parseErrTok tok msg = parseErrElem (E.new tok) msg

-- | parseErrElem. Generic error message.
parseErrElem :: E.Elem -> String -> String
parseErrElem e msg
    = "\nParse error at [line:"++ show (E.elemL e) ++
        " col:"++ show (E.elemC e) ++"]: "++ msg ++"\n"