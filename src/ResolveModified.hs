module ResolveModified where

import Data.Algorithm.Patience hiding (diff)
import Data.Char (isPrint, toUpper)
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad
import Control.Monad.Loops (untilJust)
import Safe (maximumDef)
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
    hunks <- parseText . lines <$> readFile f
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

    -- Convert context lines to Items, so we don't need to treat them as a special case
    let lines = (toBoth . appendNL <$> prev)
                ++ diff mode local remote
                ++ (toBoth . appendNL <$> after)

    -- Determine what (if anything) to display in the left margin of the diff.
    -- Each element is a list of lines to display for a given line of diff.
    -- TODO: add support for rendering blames
    let margin = case mode of
                    Raw -> return . colourItemChar <$> lines
                    _   -> replicate (length lines) []

    mapM_ putStr $ pairMargin margin (renderDiff <$> lines)

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

toBoth :: a -> Item a
toBoth x = Both x x

-- This function pairs an arbitrary no. of margin notes with a line of diff.
-- It pads the margin notes as needed to ensure the start of each line the diff lines up.
pairMargin :: [[String]] -> [String] -> [String]
pairMargin margins diffLines = res where
    -- Compute max length for margin, capped at a reasonable value
    -- TODO: cap should be user configurable
    marginMaxLength = maximumDef 0 . concat $ map2 (length . stripAnsi) margins
    marginWidth = min marginMaxLength 20

    -- Match margin notes to diff lines
    res = zipWith pairMargin' margins diffLines

    -- Helper function for mapping over nested lists
    map2 :: (a -> b) -> [[a]] -> [[b]]
    map2 f = map (map f)

    -- Combines margin notes and diff line into a single (multi-line) string.
    -- Note that we expect diff to end in a new line, but not the margin notes.
    pairMargin' :: [String] -> String -> String
    pairMargin' [] line = pairMargin1 "" line
    pairMargin' [marginNote] line = pairMargin1 marginNote line
    pairMargin' (mn0:mns) line = concat $ line1 : rest where
        line1 = pairMargin1 mn0 line
        rest  = zipWith pairMargin1 mns (repeat "\n")

    -- Like pairMargin', but deals with the base case of a single, one-line margin note.
    pairMargin1 :: String -> String -> String
    pairMargin1 marginNote line = padMargin marginNote ++ line' where
        line' = if   marginWidth == 0
                then line
                else ' ' : line

    -- Truncates/pads the string to marginWidth length, ignoring ANSI escape codes.
    padMargin :: String -> String
    padMargin s = res where
        stripLength x = length $ stripAnsi x
        f x = stripLength x <= marginWidth

        res = if   stripLength s < marginWidth
              then s ++ replicate (marginWidth - stripLength s) ' '
              else fromJust . find f $ prefixes s

-- Like DAP.itemChar, but with ANSI colouring
colourItemChar :: Item a -> String
colourItemChar (Old _) = dull_red "-"
colourItemChar (New _) = dull_green "+"
colourItemChar (Both _ _) = " "

