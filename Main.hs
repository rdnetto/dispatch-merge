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
import Text.Printf

import DiffAndMerge
import DiffParser
import Git
import Prompt
import Util
import Debug.Trace


-- Helper type for representing the outcome of a command. TryAgain takes mutators for each of the parameters of resolve.
data CmdOutcome = Success [String]
                | TryAgain (F (Maybe DiffMode)) (F DiffInfo) (F DiffSection)
                | Skipped
type F a = a -> a


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
                return $ (False,) <$> safeList argv,
                fmap (True,) <$> getGitConflicts,
                error "No files found."
            ]

    -- Resolve conflicts for each file
    forM_ args $ \f -> do
        hunks <- liftM (parseText . lines) $ readFile f
        let conflict_count = length $ filter isConflict hunks
        let info i = DiffInfo f i conflict_count

        resolutions <- resolveHunks info 1 [] hunks
        let complete = trace (show $ fst <$> resolutions) (and $ fst <$> resolutions)
        let resolvedHunks = snd <$> resolutions

        writeFile f . unlines $ concat resolvedHunks

        -- only git-add file if we didn't skip any sections
        when (useGit && complete) $ gitAdd f

    -- TODO: display a list of modified files on quit

-- Recursive helper function for maintaining state while iterating over hunks.
-- info - constructs a DiffInfo with the specified index
-- i - the index of the current conflict
-- prev - the (resolved) previous hunk, if any
-- d:ds - the list of hunks
-- Returns - list of resolved sections. Bool is True if the section no longer contains conflicts.
resolveHunks :: (Int -> DiffInfo) -> Int -> [String] -> [DiffSection] -> IO [(Bool, [String])]
resolveHunks info i _ ((HText s):ds) = return . ((True, s):) =<< (resolveHunks info i s ds)
resolveHunks info i prev (d0:ds) = do
    let d1 = case ds of
               dx:_ -> diffStr dx
               []   -> []

    (complete, res) <- resolve Nothing (info i) prev d0 d1
    rest <- resolveHunks info (i+1) res ds
    return $ (complete, res) : rest
resolveHunks _ _ _ [] = return $ return (True, [])

resolve :: Maybe DiffMode -> DiffInfo -> [String] -> DiffSection -> [String] -> IO (Bool, [String])
resolve _ _ _ (HText s) _ = return (True, s)
resolve mode info prev hunk@(HConflict l r) after = do
        -- TODO: this should be user configurable
        let contextSize = 3
        let mode' = fromMaybe (selectMode l r) mode
        clearScreen
        displayHunk mode' info (lastN contextSize prev) hunk (take contextSize after)
        displayPrompt info

        cmd <- untilJust (return . parsePromptOption . toUpper =<< getChar)
        putStrLn ""

        res <- handleCmd cmd hunk
        case res of
            Success s -> return (True, s)
            TryAgain a b c -> resolve (a $ Just mode') (b info) prev (c hunk) after
            Skipped -> return (False, diffStr hunk)

displayHunk :: DiffMode -> DiffInfo -> [String] -> DiffSection -> [String] -> IO ()
displayHunk mode info prev (HConflict local remote) after = let
        border = dull_cyan "--------------------------------------------------------------------------------"
    in do
        -- Header
        putStrLn border
        putStrLn . vivid_white $ filename info
        putStrLn . vivid_white $ printf "Hunk %i of %i" (index info) (diffCount info)

        let scores = diffScores local remote
        let l x = show . fromJust $ lookup x scores
        putStrLn $ printf "Change blocks: char %s, word %s, line %s" (l Char) (l Word) (l Line)

        -- Show line no.s, unified diff style
        let lStart = lineNo local
        let rStart = lineNo remote
        let lCount = length $ contents local
        let rCount = length $ contents remote
        putStrLn border
        putStrLn . dull_cyan $ printf "@@ -%.2i,%i +%.2i,%i @@" lStart lCount rStart rCount

        -- Show diff
        mapM_ putStrLn prev
        mapM_ (putStr . renderDiff) $ diff mode local remote
        mapM_ putStrLn after

        putStrLn border
        return ()

-- Colorize a diff entry
renderDiff :: Item String -> String
renderDiff (Old x) = withColor Dull Red x
renderDiff (New x) = withColor Dull Green x
renderDiff (Both x _) = x

-- Handle a user input. Returns Just x if a hunk has been resolved, otherwise Nothing.
handleCmd :: PromptOption -> DiffSection -> IO CmdOutcome
handleCmd (PSimpleRes res) (HConflict h1 h2) = return . Success $ resolveHunk res h1 h2
handleCmd (PSetDiffMode d) _ = return $ TryAgain (const $ Just d) id id
handleCmd PSkip _ = return Skipped
handleCmd PEdit hunk = withSystemTempFile "hunk" $ editHunk hunk
handleCmd PHelp _ = displayPromptHelp >> return (TryAgain id id id)
handleCmd PQuit _ = exitSuccess

editHunk :: DiffSection -> FilePath -> Handle -> IO CmdOutcome
editHunk d tmpfile h = do
    hPutStr h . unlines $ diffStr d
    hClose h

    editor:args <- liftM words . getEnv $ "EDITOR"
    exitCode <- callProcessWithExitCode editor (args ++ [tmpfile])

    case exitCode of
        ExitSuccess -> (return . Success . lines) =<< readFile tmpfile
        _           -> return $ TryAgain id id id

