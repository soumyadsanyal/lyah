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

-- a type is a set of values
-- a type constructor is a set of value constructors
-- a value constructor is a function returning a value. This may be onstant, or may be dependent on parameters that are other values.
-- E.g. Maybe Int is a type consisting of values Nothing (constant value) or Just x, where x is any value of type Int.
-- We can also have type constructors that take type parameters. These are functions that take types and return types.
-- E.g. Maybe a is a *function* \a -> Nothing | Just a, taking a type bound to the type parameter a and returning a type Maybe a, which is a set consisting of values Nothing or \{Just x | x in a\}. 
-- what is the type of an expression such as Maybe? We refer to functions from types to types as having kinds. In this case, the kind of Maybe is (* -> *).
-- (->) is a function that takes two types and returns a type. It takes the form \a -> \b -> (a -> b). 
-- in this case, the expression ((->) a) is a function \b -> (a -> b). 

instance MyFunctor ((->) a) where
    lift f g = f.g





