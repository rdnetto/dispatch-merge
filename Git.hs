module Git where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import System.Process (callProcess, readProcessWithExitCode)


-- Corresponds to DAU markers in git-status porcelain format
data ChangeType = Deleted | Added | Unknown
    deriving (Eq, Show)

type Stat = (ChangeType, ChangeType, FilePath)

-- There are other change types, but we don't care about them.
{- From the man page:
 - DD  unmerged, both deleted
 - AU  unmerged, added by us
 - UD  unmerged, deleted by them
 - UA  unmerged, added by them
 - DU  unmerged, deleted by us
 - AA  unmerged, both added
 - UU  unmerged, both modified
 -}
readChangeType :: Char -> Maybe ChangeType
readChangeType 'D' = Just Deleted
readChangeType 'A' = Just Added
readChangeType 'U' = Just Unknown
readChangeType  _  = Nothing

-- Helper method for creating a Stat for the 'both modified' case
simpleStat :: FilePath -> Stat
simpleStat = (Unknown, Unknown, )

-- Retrieves a list of files with conflicts if the working directory is in a git repo. Returns Nothing on failure.
getGitConflicts :: IO (Maybe [Stat])
getGitConflicts = do
    (code, sout, _) <- readProcessWithExitCode "git" ["status", "--porcelain-only"] ""
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
gitAdd f = callProcess "git" ["add", f]

