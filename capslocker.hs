import Control.Monad
import Data.Char

main = forever $ do
    contents <- getContents
    putStr $ map toUpper contents
