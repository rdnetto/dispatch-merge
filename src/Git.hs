module Git where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Util


-- Corresponds to DAU markers in git-status porcelain format
data ChangeType = Deleted | Added | Unknown
    deriving (Eq, Show)

type Stat = (ChangeType, ChangeType, FilePath)

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

