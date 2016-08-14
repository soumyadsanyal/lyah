module Safe where

data Safe a = Dead | Safe a
    deriving (Show, Eq, Read)
