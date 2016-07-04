module Prompt where

displayPrompt :: IO ()
displayPrompt = putStrLn ">> (# of #) -- $FILENAME\n\
                         \>> l left, r right, u use both, z zap\n\
                         \   q quit, h help, n next, e edit: "

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
parsePromptOption 'n' = Just PNext
parsePromptOption 'e' = Just PEdit
parsePromptOption  _  = Nothing

