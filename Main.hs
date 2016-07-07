import Data.Algorithm.Patience hiding (diff)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, fromJust)
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


-- Helper type for representing the outcome of a command. TryAgain takes mutators for each of the parameters of resolve.
data CmdOutcome = Success [String]
                | TryAgain (F (Maybe DiffMode)) (F DiffInfo) (F DiffSection)
type F a = a -> a


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
        resolvedHunks <- sequence $ handleCmds info 1 hunks
        putStrLn . unlines $ concat resolvedHunks

    where
        -- recursive helper function for incrementing conflict index
        handleCmds :: (Int -> DiffInfo) -> Int -> [DiffSection] -> [IO [String]]
        handleCmds info i ((HText s):ds) = (return s) : (handleCmds info i ds)
        handleCmds info i (d:ds) = (resolve Nothing (info i) d) : (handleCmds info (i+1) ds)
        handleCmds _ _ [] = []

resolve :: Maybe DiffMode -> DiffInfo -> DiffSection -> IO [String]
resolve _ _ (HText s) = return s
resolve mode info hunk@(HConflict l r) = do
        let mode' = fromMaybe (selectMode l r) mode
        clearScreen
        displayHunk mode' info hunk
        displayPrompt info

        cmd <- untilJust (return . parsePromptOption . toUpper =<< getChar)
        putStrLn ""

        res <- handleCmd cmd hunk
        case res of
            Success s  -> return s
            TryAgain a b c -> resolve (a $ Just mode') (b info) (c hunk)

displayHunk :: DiffMode -> DiffInfo -> DiffSection -> IO ()
displayHunk mode info (HConflict local remote) = let
        border = dull_cyan "--------------------------------------------------------------------------------"
    in do
        -- TODO: display line number of hunk, git-style
        -- TODO: should really show context of hunk...

        -- Header
        putStrLn border
        putStrLn . vivid_white $ filename info
        putStrLn . vivid_white $ printf "Hunk %i of %i" (index info) (diffCount info)

        -- TODO: IF DEBUG
        let scores = diffScores local remote
        let l x = show . fromJust $ lookup x scores
        putStrLn $ printf "Heuristic: Char %s, Word %s, Line %s" (l Char) (l Word) (l Line)

        putStrLn border

        -- Show diff
        mapM_ (putStr . render_diff) $ diff mode local remote

        putStrLn border
        return ()

-- Colorize a diff entry
render_diff :: Item String -> String
render_diff (Old x) = withColor Dull Red x
render_diff (New x) = withColor Dull Green x
render_diff (Both x _) = x

-- Handle a user input. Returns Just x if a hunk has been resolved, otherwise Nothing.
-- TODO: should use a merge strategy consistent with the kind of diff used
handleCmd :: PromptOption -> DiffSection -> IO CmdOutcome
handleCmd (PSimpleRes res) (HConflict h1 h2) = return . Success $ resolveHunk res h1 h2
handleCmd (PSetDiffMode d) _ = return $ TryAgain (const $ Just d) id id
handleCmd PNext (HConflict h1 h2) = return . Success $ reconstructConflict h1 h2
handleCmd PEdit hunk = withSystemTempFile "hunk" $ editHunk hunk
handleCmd PHelp _ = displayPromptHelp >> return (TryAgain id id id)
handleCmd PQuit _ = exitSuccess

editHunk :: DiffSection -> FilePath -> Handle -> IO CmdOutcome
editHunk (HConflict h1 h2) tmpfile h = do
    hPutStr h . unlines $ reconstructConflict h1 h2
    hClose h

    --TODO: if subprocess fails, allow graceful recovery instead of crash
    editor:args <- liftM words . getEnv $ "EDITOR"
    callProcess editor (args ++ [tmpfile])

    (return . Success . lines) =<< readFile tmpfile

-- Retrieves a list of files with conflicts if the working directory is in a git repo. Returns Nothing on failure.
getGitConflicts :: IO (Maybe [FilePath])
getGitConflicts = do
    (code, sout, _) <- readProcessWithExitCode "git" ["diff", "--name-only", "--diff-filter=U"] ""
    return $ case code of
        ExitSuccess -> Just $ lines sout
        _           -> Nothing

