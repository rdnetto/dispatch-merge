module Util where

import System.Console.ANSI


-- Helper methods for colours
vivid_white, dull_cyan :: String -> String
vivid_white = withColor Vivid White
dull_cyan = withColor Dull Cyan

-- Wrap a string in ANSI escape codes to colour it.
withColor :: ColorIntensity -> Color -> String -> String
withColor b c s = setSGRCode [SetColor Foreground b c] ++ s ++ setSGRCode []

-- Replaces empty list with Nothing
safeList :: [a] -> Maybe [a]
safeList [] = Nothing
safeList xs = Just xs

-- Returns the first result that is not Nothing. Only runs actions as required; short-circuiting.
firstValidM :: Monad m => [m (Maybe a)] -> m a
firstValidM (x:xs) = do
    x' <- x
    case x' of
        Just a -> return a
        Nothing -> firstValidM xs
firstValidM [] = error "firstValidM: exhausted options"

