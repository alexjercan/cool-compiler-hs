module Main where

import Lexer.Parsec qualified

import Token

offsetToLineColumn :: String -> Int -> (Int, Int)
offsetToLineColumn str offset =
    let linesList = lines str
        (line, column) = go offset linesList 1
     in (line, column)
  where
    go _ [] _ = (0, 0)
    go n (l : ls) line
        | n < length l = (line, n + 1)
        | otherwise = go (n - length l - 1) ls (line + 1)

formatError :: String -> TokenInfo -> String
formatError input (TokenInfo (Illegal err) pos) = "Lexer Error: " ++ show err ++ " at " ++ show (offsetToLineColumn input pos)
formatError _ _ = "Lexer Error: Unknown error"

isIllegal :: TokenInfo -> Bool
isIllegal (TokenInfo (Illegal _) _) = True
isIllegal _ = False

compile :: String -> String
compile input = do
    let tokens = Lexer.Parsec.tokenize input
    let illegalTokens = filter isIllegal tokens
    if null illegalTokens
        then "Lexer OK"
        else unlines $ map (formatError input) illegalTokens

main :: IO ()
main = interact compile
