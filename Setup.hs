import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import System.Exit
import System.Process
import Directory

main = defaultMainWithHooks $ defaultUserHooks { runTests = tests }

good = ["01-deep.tpg","02-grammar.tpg","03-grammar.tpg","04-grammar.tpg", "05-grammar.tpg"]
bad = ["err-dupbindings.tpg","err-many-many-errors.tpg","err-manyundefined.tpg",
        "err-selfredef.tpg","err-termredef.tpg","err-typerror1.tpg","err-typerror2.tpg",
        "err-typerror3.tpg","err-useundefined.tpg","err-varyingparams.tpg"]
clean = ["test/Codegen.java","test/MapEntry.java","test/Nt.java","test/Node.java",
        "test/RuleEnum.java"]

-- | Feed the code generator with valid and erroneous grammars and check its
--  return code. So on 'valid' grammars we check for 'ExitSuccess' and on 'invalid'
--  grammars we check for an exit code which is NOT equal to 'ExitSuccess'.
tests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ExitCode
tests _ _ _ lbi
    = let hburg = (buildDir lbi) ++ "/hburg/hburg" in
    let rungood 
            = runit
                (cmd hburg good "") 
                (\ex -> "TEST " ++ (if (ex == ExitSuccess) then "SUCCESFUL" else "FAILED!!"))
        in
    let runbad 
            = runit
                (cmd hburg bad "errors/")
                (\ex -> "TEST " ++ (if (ex /= ExitSuccess) then "SUCCESFUL" else "FAILED!!"))
        in
    do  exitOk <- rungood
        exitBad <- runbad
        cleanup
        let checkOk = map (\ex -> ex == ExitSuccess ) (exitOk)
        let checkBad = map (\ex -> ex /= ExitSuccess ) (exitBad)
        if (and (checkOk ++ checkBad))
            then do putStrLn "ALL TESTS SUCCESFUL!!"
                    exitWith ExitSuccess
            else do putStrLn "TESTS FAILED!!"
                    exitFailure
    where
        cmd :: String -> [String] -> String -> [String]
        cmd exec xs dir
            = map
                (\g -> exec ++ " -p test " ++ "test/" ++ dir ++ g)
                (xs)

        runit :: [String] -> (ExitCode -> String) -> IO [ExitCode]
        runit xs f
            = mapM
                (\t ->
                    do  putStrLn (replicate 70 '-')
                        putStrLn ("TEST: '"++ t ++"'\n")
                        x <- waitForProcess =<< runCommand t
                        putStrLn (f x)
                        putStrLn (replicate 70 '-')
                        return x)
                (xs)

        cleanup :: IO [()]
        cleanup
            = mapM
                (\f -> removeFile f)
                (clean)