module Prompt where

import System.Console.ANSI
import System.IO (hFlush, stdout)
import Text.Printf

import DiffParser
import Diffs
import Util


data PromptOption = PSimpleRes SimpleRes
                    | PQuit
                    | PHelp
                    | PSkip
                    | PEdit
                    | PSetDiffMode DiffMode
                    deriving (Eq, Show)


displayModPrompt :: DiffInfo -> IO ()
displayModPrompt info =
    let msg = ">> (%i of %i) -- %s\n\
              \   %s, %s, %s\n\
              \   A raw, I line, W word, C char\n\
              \   Q quit, H help, N next, E edit: "
        lmsg = withColor Dull Red "L local"
        rmsg = withColor Dull Green "R remote"
        umsg = withColor Dull Yellow "U use both"
    in do
        -- need to flush because of line buffering
        putStr . vivid_white $ printf msg (index info) (diffCount info) (filename info) lmsg rmsg umsg
        hFlush stdout

displayModPromptHelp :: IO ()
displayModPromptHelp = do
    mapM_ putStrLn [
            "",
            "  L -- use local version (red)",
            "  R -- use remote version (green)",
            "  U -- use both (local first)",
            "  N -- skip to the next hunk",
            "  E -- edit the hunk",
            "  H -- show this screen",
            "  I -- show line-diff",
            "  W -- show word-diff",
            "  C -- show char-diff",
            "  Q -- quit",
            "",
            "",
            "Press any key to continue..."
        ]
    _ <- getChar
    return ()

displayFilePrompt :: IO ()
displayFilePrompt =
    let lmsg = withColor Dull Red "L local"
        rmsg = withColor Dull Green "R remote"
        msg  = ">> %s, %s\n\
              \   Q quit, H help, N next: "
    in do
        -- need to flush because of line buffering
        putStr . vivid_white $ printf msg lmsg rmsg
        hFlush stdout

displayFilePromptHelp :: IO ()
displayFilePromptHelp = do
    mapM_ putStrLn [
            "",
            "  L -- use local version (red)",
            "  R -- use remote version (green)",
            "  N -- skip to the next hunk",
            "  H -- show this screen",
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
parsePromptOption 'Q' = Just PQuit
parsePromptOption '\EOT' = Just PQuit   -- Ctrl+D
parsePromptOption 'H' = Just PHelp
parsePromptOption 'N' = Just PSkip
parsePromptOption 'E' = Just PEdit
parsePromptOption 'W' = Just $ PSetDiffMode Word
parsePromptOption 'I' = Just $ PSetDiffMode Line
parsePromptOption 'C' = Just $ PSetDiffMode Char
parsePromptOption 'A' = Just $ PSetDiffMode Raw
parsePromptOption  _  = Nothing

-- Restricted version of parsePromptOption
parseFilePromptOption :: Char -> Maybe PromptOption
parseFilePromptOption c = do
    res <- parsePromptOption c
    if res `elem` [PSimpleRes RLeft, PSimpleRes RRight, PQuit, PHelp, PSkip]
        then return res
        else Nothing

