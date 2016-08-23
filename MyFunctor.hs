module MyFunctor where
import Safe
import Stack
import Tree
import Stopping

class MyFunctor typeConstructor where
    lift :: (a -> b) -> typeConstructor a -> typeConstructor b

-- this class needs to be implemented for as many value constructors are contained in the type constructor in question.

instance MyFunctor Stack where
    lift f EStack = EStack
    lift f (Push x xs) = Push (f x) (lift f xs)

-- here the type constructor is Stack, which has value constructors EStack and Push a Stack a.
-- EStack is nullary
-- Push a Stack a takes a single parameter of type a (no constraints given on a).

instance MyFunctor Safe where 
    lift f Dead = Dead 
    lift f (Safe x) = Safe (f x)

-- the type constructor Safe a contains two value constructors: Dead and Safe a

instance MyFunctor Tree where
    lift f E = E
    lift f (Node root left right) = Node (f root) (lift f left) (lift f right)

-- the type constructor Tree a contains two value constructors: E and Node a Tree a Tree a

instance MyFunctor (Stopping a) where
    lift f (Continue x) = Continue (f x)
    lift f (Stop x) = Stop x

instance MyFunctor ((->) a) where
    lift f g x = f (g x)


--I think the important takeaway here is that I don't really understand the implementation of the constructor (->). What are its value constructors?
--
--



