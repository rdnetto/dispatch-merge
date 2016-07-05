import Data.Algorithm.Patience
import Control.Monad
import Control.Monad.Loops (untilJust)
import System.Console.ANSI
import System.Environment (getArgs, getEnv)
import System.Exit (exitSuccess, ExitCode(..))
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcessWithExitCode)
import Text.Printf

import DiffAndMerge
import DiffParser
import Prompt
import Util


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

        cmd <- untilJust (return . parsePromptOption =<< getChar)
        putStrLn ""

        res <- resolveHunk cmd hunk
        case res of
            Just s  -> return s
            Nothing -> resolve info hunk

displayHunk :: DiffInfo -> DiffSection -> IO ()
displayHunk info (HConflict local remote) = let
        border = dull_cyan "--------------------------------------------------------------------------------"
        c2s = (:"")
    in do
        -- TODO: display line number of hunk, git-style
        -- TODO: should really show context of hunk...

        -- Header
        putStrLn border
        putStrLn . vivid_white $ filename info
        putStrLn . vivid_white $ printf "Hunk %i of %i" (index info) (diffCount info)
        putStrLn border

        -- Show diff
        let line_diff = lineDiff local remote
        let word_diff = wordDiff local remote
        let char_diff = (fmap c2s) <$> charDiff local remote

        mapM_ (putStr . render_diff) char_diff

        putStrLn border
        return ()

-- Colorize a diff entry
render_diff :: Item String -> String
render_diff (Old x) = withColor Dull Red x
render_diff (New x) = withColor Dull Green x
render_diff (Both x _) = x

-- Resolve a single hunk. Call again if it returns Nothing.
-- TODO: should use a merge strategy consistent with the kind of diff used
resolveHunk :: PromptOption -> DiffSection -> IO (Maybe [String])
resolveHunk PLeft (HConflict hunk _) = return2 $ contents hunk
resolveHunk PRight (HConflict _ hunk) = return2 $ contents hunk
resolveHunk PUnion (HConflict h1 h2) = return2 $ contents h1 ++ contents h2
resolveHunk PZap (HConflict _ _) = return2 []
resolveHunk PQuit _ = exitSuccess
resolveHunk PHelp h = displayPromptHelp >> return Nothing
resolveHunk PNext (HConflict h1 h2) = return2 $ reconstructConflict h1 h2
resolveHunk PEdit hunk = withSystemTempFile "hunk" $ editHunk hunk

editHunk :: DiffSection -> FilePath -> Handle -> IO (Maybe [String])
editHunk (HConflict h1 h2) tmpfile h = do
    hPutStr h . unlines $ reconstructConflict h1 h2
    hClose h

    --TODO: if subprocess fails, allow graceful recovery instead of crash
    editor:args <- liftM words . getEnv $ "EDITOR"
    callProcess editor (args ++ [tmpfile])

    (return . Just . lines) =<< readFile tmpfile

-- Retrieves a list of files with conflicts if the working directory is in a git repo. Returns Nothing on failure.
getGitConflicts :: IO (Maybe [FilePath])
getGitConflicts = do
    (code, sout, _) <- readProcessWithExitCode "git" ["diff", "--name-only", "--diff-filter=U"] ""
    return $ case code of
        ExitSuccess -> Just $ lines sout
        _           -> Nothing

