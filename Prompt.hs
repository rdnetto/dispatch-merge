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
                  \   l left, r right, u use both, z zap\n\
                  \   L line, W word, C char\n\
                  \   q quit, h help, n next, e edit: "
        in do
            -- need to flush because of line buffering
            putStr . vivid_white $ printf msg (index info) (diffCount info) (filename info)
            hFlush stdout

displayPromptHelp :: IO ()
displayPromptHelp = do
    mapM_ putStrLn [
            "",
            "  l -- use local version (left/red)",
            "  r -- use remote version (right/green)",
            "  u -- use both (local first)",
            "  z -- zap (discard) both",
            "  n -- skip to the next hunk",
            "  e -- edit the hunk",
            "  h -- show this screen",
            "  L -- show line-diff",
            "  W -- show word-diff",
            "  C -- show char-diff",
            "  q -- quit",
            "",
            "",
            "Press any key to continue..."
        ]
    _ <- getChar
    return ()

parsePromptOption :: Char -> Maybe PromptOption
parsePromptOption 'l' = Just $ PSimpleRes RLeft
parsePromptOption 'r' = Just $ PSimpleRes RRight
parsePromptOption 'u' = Just $ PSimpleRes RUnion
parsePromptOption 'z' = Just $ PSimpleRes RZap
parsePromptOption 'q' = Just PQuit
parsePromptOption 'h' = Just PHelp
parsePromptOption 'n' = Just PNext
parsePromptOption 'e' = Just PEdit
parsePromptOption 'W' = Just $ PSetDiffMode Word
parsePromptOption 'L' = Just $ PSetDiffMode Line
parsePromptOption 'C' = Just $ PSetDiffMode Char
parsePromptOption  _  = Nothing

