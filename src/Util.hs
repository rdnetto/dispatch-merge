module Util where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, stripPrefix)
import Control.Monad (replicateM_)
import System.Console.ANSI
import System.Console.Terminal.Size (size, height)
import System.Exit (ExitCode)
import System.Process (readProcess, readProcessWithExitCode)


-- Helper methods for colours
dull_red, dull_green, dull_yellow, vivid_white, dull_cyan :: String -> String
vivid_white = withColor Vivid White
dull_cyan = withColor Dull Cyan
dull_red = withColor Dull Red
dull_green = withColor Dull Green
dull_yellow = withColor Dull Yellow

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

-- Returns the last N items from the list, or the list itself if it is less than N items long.
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- Print enough blank lines to clear the terminal.
-- Unlike System.Console.ANSI.clearScreen, it ensures that the cursor is at the bottom of the screen, for consistent output.
flushTerminal :: IO ()
flushTerminal = do
    s <- size
    case s of
        Just s' -> replicateM_ (height s') (putStrLn "")
        Nothing -> return ()

-- Like callProcess, but does not print to stdout.
callProcessSilent :: FilePath -> [String] -> IO ()
callProcessSilent f args = readProcess f args "" >> return ()

-- Like callProcess, but doesn't throw an exception on failure.
callProcessWithExitCode :: FilePath -> [String] -> IO ExitCode
callProcessWithExitCode path args = do
    (code, _, _) <- readProcessWithExitCode path args ""
    return code

-- Emulate for-each loop with early termination (break).
breakableForM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
breakableForM (x:xs) f = do
    y <- f x
    case y of
        Just y' -> return . (y':) =<< breakableForM xs f
        Nothing -> return []
breakableForM [] _ = return []

-- Syntactic sugar for breakableForM. If True, breaks.
breakIf :: Monad m => Bool -> b -> m (Maybe b)
breakIf False x = return $ Just x
breakIf True _ = return Nothing

-- Convenience method for mapping over tuples of lists.
mapBoth :: (a -> c) -> (b -> d) -> ([a], [b]) -> ([c], [d])
mapBoth f g (xs, ys) = (f <$> xs, g <$> ys)

appendNL :: String -> String
appendNL = (++ " \n")

-- Remove trailing whitespace
rstrip :: String -> String
rstrip = dropWhileEnd isSpace

-- Removes the ANSI escape codes from a String.
stripAnsi :: String -> String
stripAnsi ('\ESC':'[':s) = stripAnsi . tail $ dropWhile (/= 'm') s
stripAnsi (s0:ss) = s0 : stripAnsi ss
stripAnsi "" = ""

-- Returns the prefixes of xs, in descending order of length. Includes [].
prefixes :: [a] -> [[a]]
prefixes xs = (flip take $ xs) <$> reverse [0 .. length xs]

-- String replace function
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace needle subst haystack@(h0:hs) = res where
    res = case stripPrefix needle haystack of
            Just x  -> subst ++ replace needle subst x
            Nothing -> h0 : replace needle subst hs

-- Helper function for mapping over nested lists
map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

