import Data.Char (toUpper)
import Data.List (partition)
import Control.Monad
import Control.Monad.Loops (untilJust)
import System.Console.ANSI
import System.Environment (getArgs)
import System.IO

import Git
import Prompt
import ResolveModified
import ResolveDeleted
import Util

-- TODO: fix build system
main :: IO ()
main = do
    -- Configure terminal - configure stdin for reading individual keys, and move cursor to the bottom of stdout.
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    flushTerminal

    -- Load files
    argv <- getArgs
    (useGit, args) <- firstValidM [
                -- TODO: this would probably be much clearer with a monad transformer
                return $ (False,) . fmap simpleStat <$> safeList argv,
                fmap (True,) <$> getGitConflicts,
                error "No files found."
            ]

    -- Resolve conflicts for each file
    results <- breakableForM args $ resolveFile useGit

    -- Display results
    clearScreen

    let (resolvedFiles, unresolvedFiles) = mapBoth fst fst $ partition snd results
    let nonnull = not . null
    let printFN f = putStrLn $ '\t' : f

    when (nonnull resolvedFiles) $ putStrLn "Resolved:"
    forM_ resolvedFiles printFN
    putStrLn ""

    when (nonnull unresolvedFiles) $ putStrLn "Unresolved:"
    forM_ unresolvedFiles printFN

resolveFile :: Bool -> Stat -> IO (Maybe (FilePath, Bool))
resolveFile useGit (Unknown, Unknown, f) = resolveModifiedFile useGit f
resolveFile useGit (Added, Added, f) = resolveModifiedFile useGit f
resolveFile _ (Deleted, Deleted, f) = return $ Just (f, True) -- just need to mark as resolved
resolveFile useGit s@(x, y, f) = do
    -- file was added/deleted on one branch
    clearScreen
    putStrLn border
    putStrLn . vivid_white $ "--" ++ f
    putStrLn $ "This file was " ++ describeConflict x y
    putStrLn "Which version would you like to use?"
    putStrLn ""
    displayFilePrompt

    cmd <- untilJust (return . parseFilePromptOption . toUpper =<< getChar)
    res <- handleFileCmd cmd s
    case res of
        Success _ -> return $ Just (f, True)
        TryAgain _ _ _ -> resolveFile useGit s
        Skipped -> return $ Just (f, False)
        Exit -> return Nothing

