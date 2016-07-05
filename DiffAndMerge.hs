-- This module contains logic for diffing and merging hunks
module DiffAndMerge(lineDiff, wordDiff, charDiff) where

import Data.Algorithm.Patience
import Data.Char (isSpace)

import DiffParser


lineDiff :: Hunk -> Hunk -> [Item String]
lineDiff l r = diff (f l) (f r) where
    f = (map appendNL) . contents

wordDiff :: Hunk -> Hunk -> [Item String]
wordDiff l r = diff (f l) (f r) where
    f = (breakWords =<<) . (map appendNL) . contents
    breakWords = safeBreak isSpace

charDiff :: Hunk -> Hunk -> [Item Char]
charDiff l r = diff (f l) (f r) where
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
