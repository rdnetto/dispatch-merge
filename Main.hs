import Control.Monad
import Control.Monad.Loops (untilJust)
import System.Console.ANSI (clearScreen)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)

import DiffParser
import Prompt


main :: IO ()
main = do
    -- Configure terminal
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    -- Test with file
    hunks <- liftM (parseText . lines) $ readFile "test.txt"
    resolvedHunks <- mapM resolve hunks
    putStrLn . unlines $ concat resolvedHunks

resolve :: DiffSection -> IO [String]
resolve (HText s) = return s
resolve hunk@(HConflict _ _) = do
        clearScreen
        displayHunk hunk
        displayPrompt
        cmd <- untilJust (getChar >>= return . parsePromptOption)
        resolveHunk cmd hunk

displayHunk :: DiffSection -> IO ()
displayHunk (HConflict local remote) = let
        border = "--------------------------------------------------------------------------------"
    in do
        -- TODO: make this prettier
        putStrLn border
        putStrLn "Hunk # of #"
        putStrLn border
        putStr . unlines $ contents local

        putStrLn border
        putStr . unlines $ contents remote
        putStrLn border

        return ()

resolveHunk :: PromptOption -> DiffSection -> IO [String]
resolveHunk PLeft (HConflict hunk _) = return $ contents hunk
resolveHunk PRight (HConflict _ hunk) = return $ contents hunk
resolveHunk PUnion (HConflict h1 h2) = return $ contents h1 ++ contents h2
resolveHunk PZap (HConflict h1 h2) = return []
resolveHunk PQuit _ = exitSuccess
resolveHunk PHelp h = displayPromptHelp >> resolve h
resolveHunk PNext (HConflict h1 h2) = return $ reconstructConflict h1 h2
resolveHunk PEdit hunk = withSystemTempFile "hunk" $ editHunk hunk

editHunk :: DiffSection -> FilePath -> Handle -> IO [String]
editHunk (HConflict h1 h2) tmpfile h = do
    hPutStr h . unlines $ reconstructConflict h1 h2
    hClose h

    --TODO: if subprocess fails, allow graceful recovery instead of crash
    editor:args <- liftM words . getEnv $ "EDITOR"
    callProcess editor (args ++ [tmpfile])

    (return . lines) =<< readFile tmpfile

