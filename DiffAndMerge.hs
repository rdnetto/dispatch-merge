-- This module contains logic for diffing and merging hunks
module DiffAndMerge(DiffMode(..), SimpleRes(..), diff, resolveHunk, diffScores, selectMode) where

import qualified Data.Algorithm.Patience as DAP
import Data.Algorithm.Patience (Item(..))
import Data.Char (isSpace, isSymbol)
import Data.Function (on)
import Data.List (group, maximumBy)
import Data.Ratio

import DiffParser


data DiffMode = Line | Word | Char
    deriving (Show, Eq)

data SimpleRes = RLeft | RRight | RUnion | RZap
    deriving (Show, Eq)

type Score = Ratio Int


diff :: DiffMode -> Hunk -> Hunk -> [Item String]
diff Line l r = lineDiff l r
diff Word l r = wordDiff l r
diff Char l r = (fmap c2s) <$> charDiff l r where
    c2s = (:"")

lineDiff :: Hunk -> Hunk -> [Item String]
lineDiff l r = DAP.diff (f l) (f r) where
    f = (map appendNL) . contents

wordDiff :: Hunk -> Hunk -> [Item String]
wordDiff l r = DAP.diff (f l) (f r) where
    f = (breakWords =<<) . (map appendNL) . contents
    breakWords = safeBreak sep
    sep x = isSpace x || isSymbol x

charDiff :: Hunk -> Hunk -> [Item Char]
charDiff l r = DAP.diff (f l) (f r) where
    f = (id =<<) . (map appendNL) . contents

-- Like words/lines/etc., but includes the boundary text as a separate element.
-- Designed to be used with isSpace, etc.
safeBreak :: (Char -> Bool) -> String -> [String]
safeBreak _ [] = []
safeBreak f t = p : ws : safeBreak f s where
    (p, x) = break f t
    (ws, s) = span f x

appendNL :: String -> String
appendNL = (++ " \n")

-- Performs simple, pure resolution
resolveHunk :: SimpleRes -> Hunk -> Hunk -> [String]
resolveHunk RLeft hunk _  = contents hunk
resolveHunk RRight _ hunk = contents hunk
resolveHunk RUnion h1 h2  = contents h1 ++ contents h2
resolveHunk RZap _ _      = []

--Scores the diffs for a pair of hunks.
diffScores :: Hunk -> Hunk -> [(DiffMode, Score)]
diffScores l r = scores where
    score d = diffScore $ d l r
    scores = [
            (Char, score charDiff),
            (Word, score wordDiff),
            (Line, score lineDiff)
        ]

-- Heuristically selects the optimal diff mode
selectMode :: Hunk -> Hunk -> DiffMode
selectMode l r = mode where
    (mode, _) = maximumBy compareModes $ diffScores l r

    compareModes :: (DiffMode, Score) -> (DiffMode, Score) -> Ordering
    compareModes (m1, s1) (m2, s2)
        | s1 == s2  = (compare `on` modePref) m1 m2
        | otherwise = compare s1 s2
        where
            -- Map DiffMode to something which implements Ord
            modePref Line = 0
            modePref Word = 1
            modePref Char = 2

-- Measures the complexity of a diff, as a value between 0 and 1. Higher = simpler.
diffScore :: [Item a] -> Score
diffScore xs = (sum groupScores) % normScore where
    cs = DAP.itemChar <$> xs
    groupScores = map norm . filter notBoth . group $ cs
    normScore = norm . filter (/= ' ') $ cs

    norm = (^2) . length
    notBoth (' ':_) = False
    notBoth _ = True

