-----------------------------------------------------------------------------
-- |
-- Module      :  Messages
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- This module contains various error text generation functions.
-----------------------------------------------------------------------------

module Hburg.Parse.Msg (
  -- * Functions
  parseErrDupBind,
  parseErrRedef,
  parseErrTok,
  parseErrElem,
  typeErr,
) where

{- unqualified imports  -}
import Hburg.Parse.Lexer(Token)
import Hburg.Ast.Node(Node, showAsFun)

{- qualified imports  -}
import qualified Hburg.Csa.Elem as E (ElemClass(..), Elem, new)

-----------------------------------------------------------------------------

{- | typeErr. Produces error message upon type error. -}
typeErr :: E.Elem -> Int -> String -> String
typeErr e i msg = "[line:"++ show (E.elemL e) ++
  " col:"++ show (E.elemC e) ++"] Type Error: parameter "++
  show i ++" of "++ msg

{- | parseErrRedef. Produces error message upon the
     redefinition of a Node. -}
parseErrRedef :: String -> Node -> Node -> String
parseErrRedef str n1 n2 =
  parseErrElem
    (E.new n1)
    ("'"++ showAsFun n1 ++"' "++ str ++" "++
     "[line:"++ (show (E.elemL (E.new n2))) ++
     " col:"++ (show (E.elemC (E.new n2))) ++
     "], namely as '"++
     showAsFun n2 ++"'.")

{- | parseErrDupBind. Produces error message when encountering
     a duplicate binding. -}
parseErrDupBind :: String -> E.Elem -> E.Elem -> String
parseErrDupBind str e1 e2 =
  parseErrElem
    (e1)
    (str ++" '"++ (E.elemShow e1) ++"' already defined at "++
     "[line:"++ (show (E.elemL e2)) ++
     " col:"++ (show (E.elemC e2)) ++"].")

{- | parseErrTok. -}
parseErrTok :: Token -> String -> String
parseErrTok tok msg = parseErrElem (E.new tok) msg

{- | parseErrElem. Generic error message. -}
parseErrElem :: E.Elem -> String -> String
parseErrElem e msg = "[line:"++ show (E.elemL e) ++
  " col:"++ show (E.elemC e) ++"] Error: "++ msg

-----------------------------------------------------------------------------