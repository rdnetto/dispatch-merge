module ResolveModified where

import Data.Algorithm.Patience hiding (diff)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad
import Control.Monad.Loops (untilJust)
import System.Console.ANSI
import System.Environment (getEnv)
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

    -- Show diff, with left-padding for raw mode
    let lpad = case mode of
                    Raw -> map (' ':)
                    _   -> id
    mapM_ putStrLn $ lpad prev
    mapM_ (putStr . renderDiff) $ diff mode local remote
    mapM_ putStrLn $ lpad after

    putStrLn border
    return ()

-- Colorize a diff entry
renderDiff :: Item String -> String
renderDiff (Old x) = dull_red x
renderDiff (New x) = dull_green x
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

