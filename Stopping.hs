module Stopping where

data Stopping a b = Stop a | Continue b
    deriving (Show, Read, Eq, Ord)
