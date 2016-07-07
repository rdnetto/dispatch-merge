module Util where

import Control.Monad (replicateM_)
import System.Console.ANSI
import System.Console.Terminal.Size (size, height)
import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)


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

-- Like callProcess, but doesn't throw an exception on failure.
callProcessWithExitCode :: FilePath -> [String] -> IO ExitCode
callProcessWithExitCode path args = do
    (code, _, _) <- readProcessWithExitCode path args ""
    return code

