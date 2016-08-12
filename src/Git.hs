module Git where

import Data.Char (isSpace)
import Data.List (find)
import Data.List.Split (chop, dropBlanks, split, whenElt)
import Data.Maybe (catMaybes, fromJust)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode)

import Util


-- Corresponds to DAU markers in git-status porcelain format
data ChangeType = Deleted | Added | Unknown
    deriving (Eq, Show)

-- Represents info parsed from git-blame porcelain format
data BlameInfo = BlameInfo {
                    commit :: CommitHash,
                    summary :: String,
                    content :: String
                } deriving (Eq, Show)

type Stat = (ChangeType, ChangeType, FilePath)
type CommitHash = String

-- There are other change types, but we don't care about them.
readChangeType :: Char -> Maybe ChangeType
readChangeType 'D' = Just Deleted
readChangeType 'A' = Just Added
readChangeType 'U' = Just Unknown
readChangeType  _  = Nothing

-- Human friendly description of conflicts.
describeConflict :: ChangeType -> ChangeType -> String
describeConflict Deleted Deleted = "deleted on both branches"
describeConflict Added Added     = "added on both branches"
describeConflict Unknown Unknown = "modified on both branches"
describeConflict Added Unknown   = "added on the local branch"
describeConflict Unknown Deleted = "deleted on the remote branch"
describeConflict Unknown Added   = "added on the remote branch"
describeConflict Deleted Unknown = "deleted on the local branch"

-- Helper method for creating a Stat for the 'both modified' case
simpleStat :: FilePath -> Stat
simpleStat = (Unknown, Unknown, )

-- Retrieves a list of files with conflicts if the working directory is in a git repo. Returns Nothing on failure.
getGitConflicts :: IO (Maybe [Stat])
getGitConflicts = do
    (code, sout, _) <- readProcessWithExitCode "git" ["status", "--porcelain"] ""
    let stats = catMaybes $ parseStat <$> lines sout

    return $ case code of
        ExitSuccess -> Just stats
        _           -> Nothing

-- Returns the path to the .git directory, based on the current working directory.
-- (Need init to remove trailing newline)
gitDir :: IO FilePath
gitDir = rstrip <$> readProcess "git" ["rev-parse", "--git-dir"] ""

-- Returns the commit from the local branch. Throws exception on failure.
-- TODO: fix error handling
getLocalCommit :: IO CommitHash
getLocalCommit = do
    d <- gitDir
    rstrip <$> readFile (d </> "ORIG_HEAD")

-- Returns the commit from the remote branch. Throws exception on failure.
-- TODO: fix error handling
getRemoteCommit :: IO CommitHash
getRemoteCommit = do
    d <- gitDir
    rstrip <$> readFile (d </> "MERGE_HEAD")

parseStat :: String -> Maybe Stat
parseStat s = res where
    (stat, f) = break isSpace s
    f' = dropWhile isSpace f

    res = case readChangeType <$> stat of
        [Just a, Just b] -> Just (a, b, f')
        _                -> Nothing

-- Runs git add on the specified file. Throws an exception on failure.
gitAdd :: FilePath -> IO ()
gitAdd f = callProcessSilent "git" ["add", f]

-- Runs git rm on the specified file. Throws an exception on failure.
gitRemove :: FilePath -> IO ()
gitRemove f = callProcessSilent "git" ["rm", f]

-- Runs git-blame on part of a revision of a file, and parses the output.
-- TODO: better error handling - currently throws on failure
gitBlame :: FilePath -> CommitHash -> Int -> Int -> IO [BlameInfo]
gitBlame file rev lineNo lineCount = do
    let lineRange = (show lineNo) ++ ",+" ++ (show lineCount)
    output <- readProcess "git" ["blame", "--line-porcelain", "-L", lineRange, rev, "--", file] ""

    -- Each section consists of multiple lines of metadata, followed by a line of content prefixed by a tab.
    -- This splitter breaks the list of lines into elements consisting of either metadata or content.
    let startsWithTab x = (head x) == '\t'
    let tokenizer = dropBlanks (whenElt startsWithTab)
    let sections = split tokenizer $ lines output

    -- Extract the metadata-content pairs, and map them into BlameInfo
    let takeBlame (md:[c]:xs) = ((md, c), xs)
    return $ uncurry parseBlameInfo <$> chop takeBlame sections

-- Parses `git blame --porcelain` output for a single line
parseBlameInfo :: [String] -> String -> BlameInfo
parseBlameInfo metadata (_:diff_content) = BlameInfo hash title diff_content where
    metadata' = words <$> metadata
    (hash:_):records = metadata'
    title = unwords . tail . fromJust $ find (\(x:_) -> x == "summary") records

