-- This module contains logic for diffing and merging hunks
module Diffs(DiffMode(..), SimpleRes(..), diff, resolveHunk, diffScores, selectMode) where

import Data.Char (isSpace, isSymbol)
import Patience hiding (diff)
import qualified Patience as P

import DiffParser
import Util (appendNL)


data DiffMode = Line | Word | Char | Raw
    deriving (Show, Eq)

data SimpleRes = RLeft | RRight | RUnion
    deriving (Show, Eq)

type Score = Int


diff :: DiffMode -> Hunk -> Hunk -> [Item String]
diff Raw  l r = lineDiff l r
diff Line l r = lineDiff l r
diff Word l r = wordDiff l r
diff Char l r = sCharDiff l r

lineDiff :: Hunk -> Hunk -> [Item String]
lineDiff l r = P.diff (f l) (f r) where
    f = map appendNL . contents

wordDiff :: Hunk -> Hunk -> [Item String]
wordDiff l r = P.diff (f l) (f r) where
    f = (breakWords =<<) . map appendNL . contents
    breakWords = safeBreak sep
    sep x = isSpace x || isSymbol x

charDiff :: Hunk -> Hunk -> [Item Char]
charDiff l r = P.diff (f l) (f r) where
    f = (id =<<) . map appendNL . contents

sCharDiff :: Hunk -> Hunk -> [Item String]
sCharDiff l r = fmap c2s <$> charDiff l r where
    c2s = (:"")

-- Like words/lines/etc., but includes the boundary text as a separate element.
-- Designed to be used with isSpace, etc.
safeBreak :: (Char -> Bool) -> String -> [String]
safeBreak _ [] = []
safeBreak f t = p : ws : safeBreak f s where
    (p, x) = break f t
    (ws, s) = span f x

-- Performs simple, pure resolution
resolveHunk :: SimpleRes -> Hunk -> Hunk -> [String]
resolveHunk RLeft hunk _  = contents hunk
resolveHunk RRight _ hunk = contents hunk
resolveHunk RUnion h1 h2  = contents h1 ++ contents h2

-- Scores the diffs for a pair of hunks.
diffScores :: Hunk -> Hunk -> [(DiffMode, Score)]
diffScores l r = scores where
    score d = diffScore $ d l r
    scores = [
            (Char, score sCharDiff),
            (Word, score wordDiff),
            (Line, score lineDiff)
        ]

-- Heuristically selects the optimal diff mode
selectMode :: Hunk -> Hunk -> DiffMode
selectMode l r
    -- if a non-empty hunk consists entirely of whitespace, use raw mode
    | isWS l || isWS r = Raw
    | otherwise        = mode
    where
        isWS = (all isSpace) . concat . contents
        -- if a mode has a score higher than threshold, select the next one
        threshold = 4
        thresholdCrit = (<= threshold) . snd
        candidates = filter thresholdCrit $ diffScores l r
        mode = case candidates of
                m:_ -> fst m
                []  -> Line

-- Measures the complexity of a diff, in terms of the no. of changes. Higher = more complex.
diffScore :: [Item String] -> Score
diffScore xs = score where
    xs' = coalesce xs
    score = filtLength isOld + filtLength isNew
    filtLength f = length $ filter f xs'

    isOld (Old _) = True
    isOld _ = False

    isNew (New _) = True
    isNew _ = False

-- Combines multiple adjacent Old/New/Both entries into single ones. Ensures that there are no adjacent entries of the same case.
coalesce :: Monoid a => [Item a] -> [Item a]
coalesce (a1:a2:as)
    | itemChar a1 == itemChar a2 = coalesce $ fuse a1 a2 : as
    | otherwise                  = a1 : coalesce (a2:as)
    where
        fuse (Old x) (Old y) = Old (x <> y)
        fuse (New x) (New y) = New (x <> y)
        fuse (Both x1 x2) (Both y1 y2) = Both (x1 <> y1) (x2 <> y2)

        itemChar (Old  _  ) = '-'
        itemChar (New  _  ) = '+'
        itemChar (Both _ _) = ' '

coalesce x = x

