module Git where

import System.Exit (ExitCode(..))
import System.Process (callProcess, readProcessWithExitCode)


-- Retrieves a list of files with conflicts if the working directory is in a git repo. Returns Nothing on failure.
getGitConflicts :: IO (Maybe [FilePath])
getGitConflicts = do
    (code, sout, _) <- readProcessWithExitCode "git" ["diff", "--name-only", "--diff-filter=U"] ""
    return $ case code of
        ExitSuccess -> Just $ lines sout
        _           -> Nothing

-- Runs git add on the specified file. Throws an exception on failure.
gitAdd :: FilePath -> IO ()
gitAdd f = callProcess "git" ["add", f]

