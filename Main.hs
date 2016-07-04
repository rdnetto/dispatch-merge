import DiffParser


main :: IO ()
main = do
    txt <- readFile "test.txt"
    print . parseText $ lines txt

