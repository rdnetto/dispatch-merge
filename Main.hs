import Control.Monad
import Control.Monad.Loops (untilJust)
import System.IO

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
    displayHunk hunk
    displayPrompt
    cmd <- untilJust (getChar >>= return . parsePromptOption)
    return $ resolveHunk hunk cmd

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

resolveHunk :: DiffSection -> PromptOption -> [String]
resolveHunk hunk cmd = error ""
-- TODO

