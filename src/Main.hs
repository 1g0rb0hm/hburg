-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- The main entry point of our code generator. In here we do several things,
-- namely:
--        * handles CLI args
--        * feeds the lexer and reacts to possible errors
--        * feeds the parser and reacts to possible errors
--        * feeds the code generator and emits its result to a file
-----------------------------------------------------------------------------

module Main (
  -- Functions
  main
) where

{- unqualified imports  -}
import IO
import System
import System.Console.GetOpt

import Parser.Lexer (scanner)
import Parser.Parser (ParseResult(..), parse)

{- qualified imports  -}
import qualified Debug as D (Level(..), Entry, new, filter, format)

import qualified Ast.Ir as Ir (Ir(..))

import qualified Gen.Backend as B (emit)
import qualified Gen.Emit as E (Emit(..))

------------------------------------------------------------------------------------

{- | main. Read arguments and start code generator -}
main :: IO ()
main = do
  args <- getArgs
  codeGen args

{- | Successful Exit -}
bye :: String -> IO a
bye s = do
  putStrLn s
  exitWith (ExitSuccess)

{- | Exit upon failure -}
die :: String -> IO a
die s = do
  hPutStrLn stderr s
  exitWith (ExitFailure 1)

{- | Command line arguments -}
data CLIFlags =
  OptHelp
  | OptOutputClass String
  | OptOutputPackage String
  | OptNodeKindType String
  | OptDebug
  deriving (Eq)

{- | Command line info for arguments -}
argInfo :: [OptDescr CLIFlags]
argInfo =
  [Option [ '?' ] ["help"] (NoArg OptHelp)
      "display this help and exit"
  , Option [ 'd' ] ["debug"] (NoArg OptDebug)
      "display debugging output after parsing"
  , Option [ 'c' ] ["classname"] (ReqArg OptOutputClass "Class")
      "code generator Java class name (default: Codegen)"
  , Option [ 'p' ] ["package"] (ReqArg OptOutputPackage "Package")
      "Java package name (e.g.: comp.gen)"
  , Option [ 't' ] ["type"] (ReqArg OptNodeKindType "Type")
      "Java datatype which discriminates IR nodes (default: NodeKind)"]

{- | Program usage -}
usage :: String -> String
usage prog = "Usage: "++ prog ++" [OPTION...] file"

--
-- Extract various command line options
--
getOutputClassName :: [CLIFlags] -> IO (String)
getOutputClassName cli =
  case [ s | (OptOutputClass s) <- cli ] of
    [] -> return ("Codegen")
    files -> return (last files)

getOutputPackage :: [CLIFlags] -> IO (String)
getOutputPackage cli =
  case [ s | (OptOutputPackage s) <- cli ] of
    [] -> return ("")
    packages -> return (last packages)

getNodeKindType :: [CLIFlags] -> IO (String)
getNodeKindType cli =
  case [ s | (OptNodeKindType s) <- cli ] of
    [] -> return ("NodeKind")
    types -> return (last types)

{- | Evaluate arguments and kick of scanner and parser -}
codeGen :: [String] -> IO ()
codeGen args =
  case getOpt Permute argInfo ([] ++ args) of
    (cli,_,[]) | OptHelp `elem` cli -> do
      prog <- getProgName
      bye (usageInfo (usage prog) argInfo)
    (cli,[fname],[]) -> do
      content <- readFile fname
      class'  <- getOutputClassName cli
      pkg     <- getOutputPackage cli
      type'   <- getNodeKindType cli
      -- Run Parser
      case runParse content of
        -- Successful Parse
        Right result -> do
          outputFiles $ B.emit class' pkg type' result
          if (OptDebug `elem` cli)
            then bye $ D.format $ D.filter D.All $ Ir.debug result
            else bye ""
        -- Parse Errors
        Left err | OptDebug `elem` cli ->
          die $ D.format $ D.filter D.All err
        Left err ->
          die $ D.format $ D.filter D.Error err
    -- Parameter Error
    (_,_,errors) -> do
      prog <- getProgName
      die (concat errors ++ usageInfo (usage prog) argInfo)

{- | Run Lexer and Parser -}
runParse :: String -> Either [D.Entry] Ir.Ir
runParse input =
  -- Lex
  case (scanner input) of
    Left e -> Left [D.new D.Error e]
    Right lexx ->
      -- Parse
      case parse lexx of
        ParseOk result -> Right result
        ParseErr errors result -> Left (Ir.debug result ++ errors)
        ParseFail failed -> Left failed

{- | Output generated code to file -}
outputFiles :: E.Emit a => [a] -> IO ()
outputFiles es = mapM_ (\e -> writeFile (E.emitTo e) (E.emit e)) es

------------------------------------------------------------------------------------