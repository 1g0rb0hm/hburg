-----------------------------------------------------------------------------
-- |
-- Module      :  ParseErr
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
-- This module contains various error text generation functions.
--
--
-----------------------------------------------------------------------------

module Parser.ParseErr (
	parseErrDupBind,
	parseErrRedefinition,
	parseErrTok,
	parseErrElem,
	) where

import Parser.Lexer(Token)
import Ast.Node(Node, showAsFunction)
import Env.Env(ElemClass(..), Elem, envElem)

--
-- Parse & CSA: Error functions placeholder
--

parseErrRedefinition :: String -> Node -> Node -> String
parseErrRedefinition str n1 n2
	= parseErrElem 
			(envElem n1) 
			("'" ++ showAsFunction n1 ++ "' " ++ str ++ " " ++
			"[line:" ++ (show (elemL (envElem n2))) ++
			" col:" ++ (show (elemC (envElem n2))) ++ "], namely as '"++ 
			showAsFunction n2 ++ "'.")

parseErrDupBind :: String -> Elem -> Elem -> String
parseErrDupBind str e1 e2
	= parseErrElem 
			(e1) 
			(str ++ " '" ++ (elemShow e1) ++ "' already defined at " ++
			"[line:" ++ (show (elemL e2)) ++
			" col:" ++ (show (elemC e2)) ++ "].")

parseErrElem :: Elem -> String -> String
parseErrElem e msg
	= "\nParse error at [line:" ++ show (elemL e) ++ 
		" col:" ++ show (elemC e) ++ "]: " ++ msg ++ "\n"
		
parseErrTok :: Token -> String -> String
parseErrTok tok msg 
	= "\nParse error at [line:" ++ show (elemL (envElem tok)) ++ 
		" col:" ++ show (elemC (envElem tok)) ++ "]: " ++ msg ++ "\n"
