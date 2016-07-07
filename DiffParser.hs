module DiffParser (DiffSection(..), DiffInfo(..), Hunk(..), parseText, reconstructConflict, isConflict) where

import Data.List


data DiffSection = HText [String]
                    | HConflict Hunk Hunk       --local, remote
                    deriving (Show, Eq)

data DiffInfo = DiffInfo {
    filename :: FilePath,
    index :: Int,
    diffCount :: Int
} deriving (Show, Eq)

data Hunk = Hunk {
    name :: String,
    contents :: [String],
    lineNo :: Int
} deriving (Show, Eq)


conflict_start_marker, conflict_sep_marker, conflict_end_marker :: String
conflict_start_marker = "<<<<<<< "
conflict_sep_marker = "======="
conflict_end_marker = ">>>>>>> "

-- Parses a regular block of text.
parseText :: [String] -> [DiffSection]
parseText = parseTextInternal (1, 1)

-- (lLn, rLn) - line nos. of the hunk in local and remote files
parseTextInternal :: (Int, Int) -> [String] -> [DiffSection]
parseTextInternal (lLn, rLn) txt = HText startText : parseHunk (lLn', rLn') hunk where
    (startText, hunk) = break (isPrefixOf conflict_start_marker) txt
    lLn' = lLn + length startText
    rLn' = rLn + length startText

-- Parses a merge conflict.
-- (lLn, rLn) - line nos. of the hunk in local and remote files
-- lns - content of the hunk
parseHunk :: (Int, Int) -> [String] -> [DiffSection]
parseHunk _ [] = []
parseHunk (lLn, rLn) lns =
    let
        header:xs = lns
        (hunk1, _:ys) = break (== conflict_sep_marker) xs
        (hunk2, footer:rest) = break (isPrefixOf conflict_end_marker) ys

        name1 = dropPrefix conflict_start_marker header
        name2 = dropPrefix conflict_end_marker footer

        lLn' = lLn + length hunk1
        rLn' = rLn + length hunk1
    in HConflict (Hunk name1 hunk1 lLn) (Hunk name2 hunk2 rLn) : parseTextInternal (lLn', rLn') rest

dropPrefix :: String -> String -> String
dropPrefix p = drop $ length p

reconstructConflict :: Hunk -> Hunk -> [String]
reconstructConflict local remote = concat [
        [conflict_start_marker ++ name local],
        contents local,
        [conflict_sep_marker],
        contents remote,
        [conflict_end_marker ++ name remote]
    ]

isConflict :: DiffSection -> Bool
isConflict (HConflict _ _) = True
isConflict _ = False

