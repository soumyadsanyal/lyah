module Falsey where
import Tree

class Falsey a where
    truthy :: a -> Bool

instance Falsey Int where
    truthy 0 = False
    truthy _ = True

instance Falsey Float where
    truthy 0 = False
    truthy _ = True

instance Falsey Double where
    truthy 0 = False
    truthy _ = True

instance Falsey Bool where
    truthy False = False
    truthy _ = True

instance Falsey Char where
    truthy ' ' = False
    truthy _ = True

instance Falsey [a] where
    truthy [] = False
    truthy _ = True

instance Falsey (Tree a) where
    truthy E = False
    truthy _ = True



