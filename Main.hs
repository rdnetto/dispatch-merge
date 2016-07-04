import Data.List


data DiffSection = HText [String]
                    | HConflict Hunk Hunk
                    deriving Show
data Hunk = Hunk {
    name :: String,
    contents :: [String]
} deriving Show

main :: IO ()
main = do
    txt <- readFile "test.txt"
    print . parseText $ lines txt

parseText :: [String] -> [DiffSection]
parseText txt = HText startText : parseHunk hunk where
    (startText, hunk) = break (isPrefixOf conflict_start_marker) txt

parseHunk :: [String] -> [DiffSection]
parseHunk [] = []
parseHunk lns =
    let
        header:xs = lns
        (hunk1, _:ys) = break (== conflict_sep_marker) xs
        (hunk2, footer:rest) = break (isPrefixOf conflict_end_marker) ys

        name1 = dropPrefix conflict_start_marker header
        name2 = dropPrefix conflict_end_marker footer
    in HConflict (Hunk name1 hunk1) (Hunk name2 hunk2) : parseText rest

conflict_start_marker, conflict_sep_marker, conflict_end_marker :: String
conflict_start_marker = "<<<<<<< "
conflict_sep_marker = "======="
conflict_end_marker = ">>>>>>> "

dropPrefix :: String -> String -> String
dropPrefix p = drop $ length p
