-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- The main entry point of our code generator. In here we do several things,
-- namely:
--		* handles CLI args
--		* feeds the lexer and reacts to possible errors
--		* feeds the parser and reacts to possible errors
--		* feeds the code generator and emits its result to a file
--
-----------------------------------------------------------------------------

module Main where

import IO
import System
import System.Console.GetOpt

import Parser.Lexer (Token, scanner)
import Parser.Parser (ParseResult(..), parse)

import Ast.Incl (Include)
import Ast.Decl (Declaration)
import Ast.Def (Definition)
import Ast.Op (Operator)

import Gen.Emit (emit)
import qualified Gen.Emit.EmitClass as E

------------------------------------------------------------------------------------

-- | main. Read arguments and start code generator
main :: IO ()
main = getArgs >>= \args -> codeGen args

--
-- Display information about ourselves
--

usageHeader :: String -> String
usageHeader prog 
	= "Usage: " ++ prog ++ " file\n"

--
-- Various ways how we may exit
--

byeStr :: String -> IO a
byeStr s = putStr s >> exitWith ExitSuccess

bye :: IO a
bye = exitWith ExitSuccess

showTokens :: [Token] -> IO a
showTokens t 
	= byeStr (outToken t) 
	where
		outToken :: [Token] -> String
		outToken [] = ""
		outToken (x:xs) = (show x) ++ outToken xs

die :: String -> IO a
die s 
	= hPutStr stderr (s ++ "\n") >> exitWith (ExitFailure 1)

dieCodeGen :: String -> IO a
dieCodeGen s
	= getProgName >>= \prog -> die (prog ++ ": " ++ s)

--
-- Command line arguments
--

data CLIFlags 
	= OptHelp
	| OptOutputClass String
	| OptOutputPackage String
	| OptNodeKindType String
	| OptDebug
	deriving (Eq)

constArgs :: [a]
constArgs = []

argInfo :: [OptDescr CLIFlags]
argInfo 
	= [Option [ '?' ] ["help"] (NoArg OptHelp)
			"display this help and exit"
		, Option [ 'd' ] [] (NoArg OptDebug)
			"display debugging output after parsing"
		, Option [ 'c' ] ["classname"] (ReqArg OptOutputClass "Class")
			"code generator Java class name (default: Codegen)"
		, Option [ 'p' ] ["package"] (ReqArg OptOutputPackage "Package")
			"Java package name"
		, Option [ 't' ] ["type"] (ReqArg OptNodeKindType "Type")
			"Java datatype which discriminates nodes (default: NodeKind)"
		]

--
-- Extract various command line options
--
getOutputClassName :: [CLIFlags] -> IO (String)
getOutputClassName cli
	= case [ s | (OptOutputClass s) <- cli ] of
		[]	-> return ("Codegen")
		files -> return (last files)

getOutputPackage :: [CLIFlags] -> IO (String)
getOutputPackage cli
	= case [ s | (OptOutputPackage s) <- cli ] of
		[]	-> return ("")
		packages -> return (last packages)

getNodeKindType :: [CLIFlags] -> IO (String)
getNodeKindType cli
	= case [ s | (OptNodeKindType s) <- cli ] of
		[]	-> return ("NodeKind")
		types -> return (last types)


--
-- Run our nice code generator and parser
--


-- | codeGen. Evaluate arguments and kick of scanner and parser
codeGen :: [String] -> IO ()
codeGen args 
	= case getOpt Permute argInfo (constArgs ++ args) of
		(cli,_,[]) | OptHelp `elem` cli ->
			getProgName >>= \prog -> byeStr (usageInfo (usageHeader prog) argInfo)
		(cli,[fname],[]) ->
			-- Read the input file
			readFile fname	>>= \content ->
			-- The output class name
			getOutputClassName cli >>= \outclass ->
			-- The output package name
			getOutputPackage cli >>= \outpkg ->
			-- The node kind type
			getNodeKindType cli >>= \ntype ->
			-- Run Our Parser
			case runParse content of
				-- If the result or the parser is Right we emit code for it
				Right (incl, decl, ops, defs, parseDebug) ->
					let clazz = emit outclass outpkg ntype incl decl ops defs in
					if (OptDebug `elem` cli)
						then 
							do
								outputClass clazz
								byeStr 
									("\n#### DEBUG: Parsing START ####\n" ++
									parseDebug ++ 
									"\n\n#### DEBUG: Parsing END ####\n")
						else 
							do
								outputClass clazz
								bye
				Left (err, debug) | OptDebug `elem` cli ->
					dieCodeGen (err ++ debug)
				Left (err, _) -> 
					dieCodeGen err
		(_,_,errors) -> 
			getProgName >>= \prog -> 
				die (concat errors ++
					 usageInfo (usageHeader prog) argInfo)

-- | runParse. Runs the Lexer and Parser
runParse :: String -> Either 
						(String, String) 
						(Include, Declaration, [Operator], [Definition], String)
runParse input = 
	-- Scan using our scanner
	case (scanner input) of
		Left e -> Left (e, "")
		-- Parse using our parser
		Right lexx -> case parse lexx of
			-- Parse was Ok, we can continue....
			ParseOk result ->
					Right result
			-- There were errors...
			ParseErr errs (incl, decl, ops, defs, debug) -> 
					Left (concat errs, debug)
			-- The parse failed due to some serious error...
			ParseFail s -> 
					Left (s, "")

-- | outputClass. Output generated class into a file
outputClass :: E.EmitClass a => [a] -> IO [()]
outputClass classes
	= mapM
		(\clazz -> writeFile (E.emitTo clazz) (E.emit clazz))
		classes