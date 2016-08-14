module MyFunctor where
import Safe
import Stack
import Tree
import Stopping


class MyFunctor phi where
    lift :: (a -> b) -> phi a -> phi b

instance MyFunctor Stack where
    lift f EStack = EStack
    lift f (Push x xs) = Push (f x) (lift f xs)

instance MyFunctor Safe where 
    lift f Dead = Dead 
    lift f (Safe x) = Safe (f x)

instance MyFunctor Tree where
    lift f E = E
    lift f (Node root left right) = Node (f root) (lift f left) (lift f right)

instance MyFunctor (Stopping a) where
    lift f (Continue x) = Continue (f x)
    lift f (Stop x) = Stop x


    
