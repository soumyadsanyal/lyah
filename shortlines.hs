import Data.Char
import Control.Monad

main = do
    contents <- getContents
    putStr (shortLinesOnly contents)


shortLinesOnly :: String -> String
shortLinesOnly = \inp ->
    let allLines = lines inp
        splitWords = map words allLines
        shortLinesOnly = filter (\l -> length l < 10) splitWords
        result = unlines $ map unwords shortLinesOnly
    in result


