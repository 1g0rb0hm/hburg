-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Copyright (c) 2008 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE)
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
-- Run hburg on all test grammars that are available in order to spot regressions.
-- These tests are very basic and only check HBURGs exit status.
-----------------------------------------------------------------------------

module Main (main) where

{- unqualified imports  -}
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import Data.List
import Control.Monad

{- qualified imports  -}
import qualified Distribution.Simple as Cabal (defaultMainArgs)
import qualified System.Cmd as Cmd (system)

-----------------------------------------------------------------------------

type Suffix = String
type Grammar = String

seperator = take 80 $ repeat '#'

-- | main.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    xs | not $ null $ filter (\x -> "help" `isSuffixOf` x || "?" `isSuffixOf` x) xs
      -> usage
    _ -> do
      build args
      if (not $ null $ filter (isSuffixOf "test") args)
        then do
          -- run valid grammars
          good <- inputFiles "test/" ".tpg"
          resultGood <- runTests good ExitSuccess
          -- run grammars that should trigger errors
          bad <- inputFiles "test/errors/" ".tpg"
          resultBad <- runTests bad (ExitFailure 1)
          putStrLn $ seperator ++ "\n"
          -- print out summary
          let results = resultBad ++ resultGood
          if (null results)
            then
              -- success case
              putStrLn $ (show . length $ good ++ bad) ++ " Tests Successful!\n"
            else
              -- some tests failed
              do
                putStrLn $ (show . length $ results) ++ " of " ++
                           (show . length $ good ++ bad) ++ " Tests Failed!\n"
                mapM_ (\r -> putStrLn $ "Failed Test: " ++ r) results
                putStr "\n"
        else
          return ()

-- | Retrieve hburg input files
inputFiles :: FilePath -> Suffix -> IO [String]
inputFiles path sfx = do
  contents <- getDirectoryContents path
    `catch` (\e -> do {putStrLn . show $ e; return []})
  return $ map (path ++) $ filter (isSuffixOf sfx) contents

-- | Build HBURG by running specified Cabal targets
build :: [String] -> IO ()
build args =
  let tasks = filter
                  (`elem` ["configure", "build", "clean"])
                  args
      in
  mapM_ (\t -> Cabal.defaultMainArgs [t]) tasks

-- | Print usage
usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ "Usage: runghc "++ prog ++" (clean|configure|build|test|help|?)"

-- | Setup directory for tests
setupTest :: IO ()
setupTest = do
  createDirectoryIfMissing True "test/target" 
    `catch` (putStrLn . show)

-- | Cleanup after test
cleanUpTest :: IO ()
cleanUpTest = do
  removeDirectoryRecursive "test/target"
    `catch` (putStrLn . show)

-- | Run our test case
runTest :: ExitCode -> Grammar -> IO (ExitCode, String)
runTest code gram =
  let cmd = "dist/build/hburg/hburg -p test.target " ++ gram in
  do
    setupTest
    putStrLn seperator
    putStrLn $ "Test: " ++ cmd
    putStrLn $ "Output Start>>"
    exitCode <- Cmd.system cmd
      `catch` (\e -> do {
          putStrLn . show $ e;
          return (ExitFailure 2)})
    cleanUpTest
    putStrLn "<<Output End"
    putStrLn $ "Successful? " ++ show (exitCode == code)
    return (exitCode, cmd)

-- | Run a list of test cases
runTests :: [Grammar] -> ExitCode -> IO [String]
runTests grams code = do
  retVal <- mapM (runTest code) grams
  return (map (snd) $ filter (\r -> code /= fst r) retVal)

-----------------------------------------------------------------------------