module Util where

import System.Console.ANSI


--Render a string in vivid white
vivid_white :: String -> String
vivid_white s = setSGRCode [SetColor Foreground Vivid White] ++ s ++ setSGRCode []

--Replaces empty list with Nothing
safeList :: [a] -> Maybe [a]
safeList [] = Nothing
safeList xs = Just xs

--Returns the first result that is not Nothing. Only runs actions as required; short-circuiting.
firstValidM :: [IO (Maybe a)] -> IO a
firstValidM (x:xs) = do
    x' <- x
    case x' of
        Just a -> return a
        Nothing -> firstValidM xs
firstValidM [] = error "firstValidM: exhausted options"

