module DiffParser (DiffSection(..), DiffInfo(..), Hunk(..), parseText, diffStr, isConflict) where

import Data.List


data DiffSection = HText [String]
                    | HConflict Hunk Hunk       -- local, remote
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


conflict_start_marker, conflict_com_marker, conflict_sep_marker, conflict_end_marker :: String
conflict_start_marker = "<<<<<<< "
conflict_com_marker = "||||||| "
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

-- Converts a diff back to its string representation
diffStr :: DiffSection -> [String]
diffStr (HText s) = s
diffStr (HConflict local remote) = concat [
        [conflict_start_marker ++ name local],
        contents local,
        [conflict_sep_marker],
        contents remote,
        [conflict_end_marker ++ name remote]
    ]

-- Parses a merge conflict.
-- (lLn, rLn) - line nos. of the hunk in local and remote files
-- lns - content of the hunk
parseHunk :: (Int, Int) -> [String] -> [DiffSection]
parseHunk _ [] = []
parseHunk (lLn, rLn) lns =
    let
        header:xs = lns
        (body, footer:rest) = break (isPrefixOf conflict_end_marker) xs
        (hunk1, hunk2) = parseHunkInternal body

        name1 = dropPrefix conflict_start_marker header
        name2 = dropPrefix conflict_end_marker footer

        lLn' = lLn + length hunk1
        rLn' = rLn + length hunk2
    in HConflict (Hunk name1 hunk1 lLn) (Hunk name2 hunk2 rLn) : parseTextInternal (lLn', rLn') rest

-- Parses the body of a merge conflict into two hunks.
-- Merge conflicts can be in 'merge' style (default) or 'diff3' style
--
{- Section under consideration:
 - <hunk1>
 - |||||||
 - <common base>
 - =======
 - <hunk2>
 -}
parseHunkInternal :: [String] -> ([String], [String])
parseHunkInternal lns | isDiff3   = (d3_hunk1, d3_hunk2)
                      | otherwise = (m_hunk1,  m_hunk2)
    where
        (a, b) = break (isPrefixOf conflict_com_marker) lns
        isDiff3 = (b /= [])

        -- merge style
        (m_hunk1, _:m_hunk2) = break (== conflict_sep_marker) a

        -- diff3 style
        d3_hunk1 = a
        (common, _:d3_hunk2) = break (== conflict_sep_marker) b

dropPrefix :: String -> String -> String
dropPrefix p = drop $ length p

isConflict :: DiffSection -> Bool
isConflict (HConflict _ _) = True
isConflict _ = False

