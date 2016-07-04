import Data.List


main :: IO ()
main = do
    txt <- readFile "test.txt"
    print $ parseText txt


data DiffSection = HText [String]
                    | HConflict Hunk Hunk
                    deriving Show
data Hunk = Hunk {
    name :: String,
    contents :: [String]
} deriving Show

parseText :: String -> [DiffSection]
parseText txt = HText startText : parseHunk hunk
    where
        (startText, hunk) = break (isPrefixOf conflict_start_marker) $ lines txt

parseHunk hunk = []

conflict_start_marker = "<<<<<<< "
conflict_sep_marker = "======="
conflict_end_marker = ">>>>>>> "

