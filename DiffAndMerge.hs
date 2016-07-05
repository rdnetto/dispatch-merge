-- This module contains logic for diffing and merging hunks
module DiffAndMerge(DiffMode(..), SimpleRes(..), diff, resolveHunk) where

import qualified Data.Algorithm.Patience as DAP
import Data.Algorithm.Patience (Item)
import Data.Char (isSpace)

import DiffParser


data DiffMode = Line | Word | Char
    deriving Show

data SimpleRes = RLeft | RRight | RUnion | RZap
    deriving Show


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
    breakWords = safeBreak isSpace

charDiff :: Hunk -> Hunk -> [Item Char]
charDiff l r = DAP.diff (f l) (f r) where
    f = (id =<<) . (map appendNL) . contents

-- Like words/lines/etc., but doesn't discard the boundary text.
-- Designed to be used with isSpace, etc.
safeBreak :: (Char -> Bool) -> String -> [String]
safeBreak _ [] = []
safeBreak f t = (p ++ ws) : safeBreak f s where
    (p, x) = break f t
    (ws, s) = span f x

appendNL :: String -> String
appendNL = (++ " \n")

resolveHunk :: SimpleRes -> Hunk -> Hunk -> [String]
resolveHunk RLeft hunk _  = contents hunk
resolveHunk RRight _ hunk = contents hunk
resolveHunk RUnion h1 h2  = contents h1 ++ contents h2
resolveHunk RZap _ _      = []

