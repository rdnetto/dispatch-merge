import Data.Algorithm.Patience hiding (diff)
import Data.Char (toUpper)
import Data.List (partition)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad
import Control.Monad.Loops (untilJust)
import System.Console.ANSI
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..))
import System.IO
import System.IO.Temp (withSystemTempFile)
import Text.Printf

import Diffs
import DiffParser
import Git
import Prompt
import Util


-- Helper type for representing the outcome of a command. TryAgain takes mutators for each of the parameters of resolve.
data CmdOutcome = Success [String]
                | TryAgain (F (Maybe DiffMode)) (F DiffInfo) (F DiffSection)
                | Skipped
                | Exit
type F a = a -> a

data Resolution = Resolved [String]
                | Unresolved [String]
                | Terminated
                deriving (Eq, Show)

border :: String
border = dull_cyan "--------------------------------------------------------------------------------"

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

handleFileCmd :: PromptOption -> Stat -> IO CmdOutcome
handleFileCmd (PSimpleRes side) (l, r, f) = resolveFileConflict side (l, r) f >> return (Success [])
handleFileCmd PHelp _ = displayFilePromptHelp >> return (TryAgain id id id)
handleFileCmd PQuit _ = return Exit
handleFileCmd PSkip _ = return Skipped

-- git-add restores the file, git-rm deletes the file
resolveFileConflict :: SimpleRes -> (ChangeType, ChangeType) -> FilePath -> IO ()
resolveFileConflict RLeft (Added, Unknown) = gitAdd
resolveFileConflict RLeft (Deleted, Unknown) = gitRemove
resolveFileConflict RRight (Unknown, Added) = gitAdd
resolveFileConflict RRight (Unknown, Deleted) = gitRemove
resolveFileConflict RRight (Added, Unknown) = gitRemove
resolveFileConflict RRight (Deleted, Unknown) = gitAdd
resolveFileConflict RLeft (Unknown, Added) = gitRemove
resolveFileConflict RLeft (Unknown, Deleted) = gitAdd

-- Resolves a merge conflict by showing the user the diff and prompting them.
-- Returns Just (f, complete) if the file was processed, or Nothing if the user terminated.
-- useGit - if True, run git-add for a fully resolved file
-- f - path to file
resolveModifiedFile :: Bool -> FilePath -> IO (Maybe (FilePath, Bool))
resolveModifiedFile useGit f = do
    hunks <- liftM (parseText . lines) $ readFile f
    let conflict_count = length $ filter isConflict hunks
    let info i = DiffInfo f i conflict_count

    (term, resolutions) <- resolveHunks info 1 [] hunks
    let complete = and $ isResolved <$> resolutions
    let resolvedHunks = getResolution <$> resolutions

    writeFile f . unlines $ concat resolvedHunks

    -- only git-add file if we didn't skip any sections
    when (useGit && complete) $ gitAdd f

    -- If term is true, this will cause loop to terminate prematurely.
    breakIf term (f, complete)

-- Recursive helper function for maintaining state while iterating over hunks.
-- info - constructs a DiffInfo with the specified index
-- i - the index of the current conflict
-- prev - the (resolved) previous hunk, if any
-- d:ds - the list of hunks
-- Returns - a boolean that is true if we have terminated prematurely, and a list of resolutions.
resolveHunks :: (Int -> DiffInfo) -> Int -> [String] -> [DiffSection] -> IO (Bool, [Resolution])
resolveHunks info i _ ((HText s):ds) = do
    (term, rs) <- resolveHunks info i s ds
    return (term, (Resolved s) : rs)

resolveHunks info i prev (d0:ds) = do
    let d1 = case ds of
               dx:_ -> diffStr dx
               []   -> []

    res <- resolve Nothing (info i) prev d0 d1
    let leaveUnresolved = Unresolved . diffStr
    let term = isTerminated res
    let res' = if term
               then leaveUnresolved d0
               else res

    (term', rest) <- if term
                     then return . (True,) $ leaveUnresolved <$> ds
                     else resolveHunks info (i+1) (getResolution res) ds

    return (term', res' : rest)

resolveHunks _ _ _ [] = return (False, [])

resolve :: Maybe DiffMode -> DiffInfo -> [String] -> DiffSection -> [String] -> IO Resolution
resolve _ _ _ (HText s) _ = return (Resolved s)
resolve mode info prev hunk@(HConflict l r) after = do
        -- TODO: this should be user configurable
        let contextSize = 3
        let mode' = fromMaybe (selectMode l r) mode
        clearScreen
        displayHunk mode' info (lastN contextSize prev) hunk (take contextSize after)
        displayModPrompt info

        cmd <- untilJust (return . parsePromptOption . toUpper =<< getChar)
        putStrLn ""

        res <- handleCmd cmd hunk
        case res of
            Success s -> return $ Resolved s
            TryAgain a b c -> resolve (a $ Just mode') (b info) prev (c hunk) after
            Skipped -> return $ Unresolved (diffStr hunk)
            Exit -> return Terminated

displayHunk :: DiffMode -> DiffInfo -> [String] -> DiffSection -> [String] -> IO ()
displayHunk mode info prev (HConflict local remote) after = do
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
handleCmd PHelp _ = displayModPromptHelp >> return (TryAgain id id id)
handleCmd PQuit _ = return Exit

editHunk :: DiffSection -> FilePath -> Handle -> IO CmdOutcome
editHunk d tmpfile h = do
    hPutStr h . unlines $ diffStr d
    hClose h

    editor:args <- liftM words . getEnv $ "EDITOR"
    exitCode <- callProcessWithExitCode editor (args ++ [tmpfile])

    case exitCode of
        ExitSuccess -> (return . Success . lines) =<< readFile tmpfile
        _           -> return $ TryAgain id id id

isResolved :: Resolution -> Bool
isResolved (Resolved _) = True
isResolved _ = False

isTerminated :: Resolution -> Bool
isTerminated Terminated = True
isTerminated _ = False

getResolution :: Resolution -> [String]
getResolution (Resolved s) = s
getResolution (Unresolved s) = s
getResolution _ = error "Unresolvable resolution"

