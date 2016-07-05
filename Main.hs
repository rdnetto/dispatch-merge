import Control.Monad
import Control.Monad.Loops (untilJust)
import System.Console.ANSI (clearScreen)
import System.Environment (getArgs, getEnv)
import System.Exit (exitSuccess, ExitCode(..))
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcessWithExitCode)
import Text.Printf

import DiffParser
import Prompt


main :: IO ()
main = do
    -- Configure terminal
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    -- Load files
    argv <- getArgs
    args <- firstValidM [
                (return $ safeList argv),
                getGitConflicts,
                error "No files found."
            ]

    -- Test with file
    forM_ args $ \f -> do
        hunks <- liftM (parseText . lines) $ readFile f
        let conflict_count = length $ filter isConflict hunks
        let info i = DiffInfo f i conflict_count
        resolvedHunks <- sequence $ resolveHunks info 1 hunks
        putStrLn . unlines $ concat resolvedHunks

    where
        -- recursive helper function for incrementing conflict index
        resolveHunks :: (Int -> DiffInfo) -> Int -> [DiffSection] -> [IO [String]]
        resolveHunks info i ((HText s):ds) = (return s) : (resolveHunks info i ds)
        resolveHunks info i (d:ds) = (resolve (info i) d) : (resolveHunks info (i+1) ds)
        resolveHunks _ _ [] = []


resolve :: DiffInfo -> DiffSection -> IO [String]
resolve _ (HText s) = return s
resolve info hunk@(HConflict _ _) = do
        clearScreen
        displayHunk info hunk
        displayPrompt info
        cmd <- untilJust (getChar >>= return . parsePromptOption)
        resolveHunk cmd hunk

displayHunk :: DiffInfo -> DiffSection -> IO ()
displayHunk info (HConflict local remote) = let
        border = "--------------------------------------------------------------------------------"
    in do
        -- TODO: make this prettier
        -- TODO: display line number of hunk
        putStrLn border
        putStrLn $ filename info
        putStrLn $ printf "Hunk %i of %i" (index info) (diffCount info)
        putStrLn border
        putStr . unlines $ contents local

        putStrLn border
        putStr . unlines $ contents remote
        putStrLn border

        return ()

resolveHunk :: PromptOption -> DiffSection -> IO [String]
resolveHunk PLeft (HConflict hunk _) = return $ contents hunk
resolveHunk PRight (HConflict _ hunk) = return $ contents hunk
resolveHunk PUnion (HConflict h1 h2) = return $ contents h1 ++ contents h2
resolveHunk PZap (HConflict _ _) = return []
resolveHunk PQuit _ = exitSuccess
resolveHunk PHelp h = displayPromptHelp >> resolve undefined h
resolveHunk PNext (HConflict h1 h2) = return $ reconstructConflict h1 h2
resolveHunk PEdit hunk = withSystemTempFile "hunk" $ editHunk hunk

editHunk :: DiffSection -> FilePath -> Handle -> IO [String]
editHunk (HConflict h1 h2) tmpfile h = do
    hPutStr h . unlines $ reconstructConflict h1 h2
    hClose h

    --TODO: if subprocess fails, allow graceful recovery instead of crash
    editor:args <- liftM words . getEnv $ "EDITOR"
    callProcess editor (args ++ [tmpfile])

    (return . lines) =<< readFile tmpfile

--TODO: recover from subprocess failure, which will happen if we're not in a git repo
getGitConflicts :: IO (Maybe [FilePath])
getGitConflicts = do
    (code, sout, _) <- readProcessWithExitCode "git" ["diff", "--name-only", "--diff-filter=U"] ""
    return $ case code of
        ExitSuccess -> Just $ lines sout
        _           -> Nothing

--Replaces empty list with Nothing
safeList :: [a] -> Maybe [a]
safeList [] = Nothing
safeList xs = Just xs

--Returns the first result that is not Nothing. Only runs actions as required; short-circuiting.
firstValidM :: [IO (Maybe a)] -> IO a
firstValidM (x:xs) = do
    x' <- x
    case x' of
        Just a -> return a
        Nothing -> firstValidM xs
firstValidM [] = error "firstValidM: exhausted options"

