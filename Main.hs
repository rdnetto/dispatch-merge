import Data.List


main :: IO ()
main = do
    txt <- readFile "test.txt"
    let contents = parseText txt
    return ()


data DiffSection = HText [String]
                    | HConflict Hunk Hunk
data Hunk = Hunk {
    name :: String,
    contents :: [String]
}

parseText :: String -> [DiffSection]
parseText txt = HText startText : parseHunk hunk
    where
        (startText, hunk) = break (isPrefixOf conflict_start_marker) $ lines txt

parseHunk hunk = undefined

conflict_start_marker = "<<<<<<< "
conflict_sep_marker = "======="
conflict_end_marker = ">>>>>>> "

