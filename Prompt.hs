module Prompt where

import System.IO (hFlush, stdout)
import Text.Printf

import DiffAndMerge
import DiffParser
import Util


data PromptOption = PSimpleRes SimpleRes
                    | PQuit
                    | PHelp
                    | PNext
                    | PEdit
                    | PSetDiffMode DiffMode
                    deriving Show


displayPrompt :: DiffInfo -> IO ()
displayPrompt info =
        let msg = ">> (%i of %i) -- %s\n\
                  \   L left, R right, U use both, Z zap\n\
                  \   N line, W word, C char\n\
                  \   Q quit, H help, S skip, E edit: "
        in do
            -- need to flush because of line buffering
            putStr . vivid_white $ printf msg (index info) (diffCount info) (filename info)
            hFlush stdout

displayPromptHelp :: IO ()
displayPromptHelp = do
    mapM_ putStrLn [
            "",
            "  L -- use local version (left/red)",
            "  R -- use remote version (right/green)",
            "  U -- use both (local first)",
            "  Z -- zap (discard) both",
            "  S -- skip to the next hunk",
            "  E -- edit the hunk",
            "  H -- show this screen",
            "  N -- show line-diff",
            "  W -- show word-diff",
            "  C -- show char-diff",
            "  Q -- quit",
            "",
            "",
            "Press any key to continue..."
        ]
    _ <- getChar
    return ()

parsePromptOption :: Char -> Maybe PromptOption
parsePromptOption 'L' = Just $ PSimpleRes RLeft
parsePromptOption 'R' = Just $ PSimpleRes RRight
parsePromptOption 'U' = Just $ PSimpleRes RUnion
parsePromptOption 'Z' = Just $ PSimpleRes RZap
parsePromptOption 'Q' = Just PQuit
parsePromptOption '\EOT' = Just PQuit   -- Ctrl+D
parsePromptOption 'H' = Just PHelp
parsePromptOption 'S' = Just PNext
parsePromptOption 'E' = Just PEdit
parsePromptOption 'W' = Just $ PSetDiffMode Word
parsePromptOption 'N' = Just $ PSetDiffMode Line
parsePromptOption 'C' = Just $ PSetDiffMode Char
parsePromptOption  _  = Nothing

