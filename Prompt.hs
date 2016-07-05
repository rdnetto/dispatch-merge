module Prompt where

import System.IO (hFlush, stdout)
import Text.Printf

import DiffParser
import Util

displayPrompt :: DiffInfo -> IO ()
displayPrompt info =
        let msg = ">> (%i of %i) -- %s\n\
                  \>> l left, r right, u use both, z zap\n\
                  \   q quit, h help, n next, e edit: "
        in do
            putStr . vivid_white $ printf msg (index info) (diffCount info) (filename info)
            hFlush stdout

displayPromptHelp :: IO ()
displayPromptHelp = do
    mapM_ putStrLn [
            "",
            "  l -- use local version (left)",
            "  r -- use remote version (right)",
            "  u -- use both (local first)",
            "  z -- zap (discard) both",
            "  n -- skip to the next hunk",
            "  e -- edit the hunk",
            "  h -- show this screen",
            "  q -- quit",
            "",
            "",
            "Press any key to continue..."
        ]
    _ <- getChar
    return ()

data PromptOption = PLeft
                    | PRight
                    | PUnion
                    | PZap
                    | PQuit
                    | PHelp
                    | PNext
                    | PEdit
                    deriving Show

parsePromptOption :: Char -> Maybe PromptOption
parsePromptOption 'l' = Just PLeft
parsePromptOption 'r' = Just PRight
parsePromptOption 'u' = Just PUnion
parsePromptOption 'z' = Just PZap
parsePromptOption 'q' = Just PQuit
parsePromptOption 'h' = Just PHelp
parsePromptOption 'n' = Just PNext
parsePromptOption 'e' = Just PEdit
parsePromptOption  _  = Nothing

