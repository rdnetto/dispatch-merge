module ResolveDeleted where

import Diffs
import Git
import Prompt
import ResolveModified


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

