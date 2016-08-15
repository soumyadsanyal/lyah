module Kinds where


--class Tofu t where
--    tofuLift :: a b -> t b a

--so tofuLift is a function of the signature * -> *.
--this means that j a has signature * and t a j has signature *
--One possibility is that a is *, and j is * -> *. Then t a j has signature kind(t) * (* -> *), suggesting that kind t is 
--
-- * -> (* -> *) -> *
--
-- Are there any other possibilities?
--
-- What if kind a is (* *) ? Or (* -> *) ? Let's say it's (* -> *). Then in that case we could have that kind j is (* -> *) -> *. Then kind t would be (* -> *) -> ( (* -> *) -> *) -> *. In this situation kind a is perfectly compatible with something like Int -> Bool, and kind j is compatible with something like (Int -> Bool) -> Bool, by some sort of `eval` definition.
--
-- It's not clear to me that this argument in LYAH makes sense. I guess it makes sense in that he states that `we assume * for a`.
--
-- Let's roll with that assumption, and then redo the exercise for the new assumption I've made. There are clearly infinitely many distinct possibilities.
--
-- Let's assume that kind a is *, kind j is * -> *, kind t is * -> (* -> *) -> *
-- An instance of this class needs to be of signature kind t

-- exploring some constructions to make sure I understand kinds
data First a = First a 
    deriving (Show)

--kind First is * -> *
--
data Second a b c = Second (a b c)
    deriving (Show)

--kind Second is (* -> * -> *) -> * -> * -> *
--
data Third a b = Third (a b b)

--kind Third is (* -> * -> *) -> * -> *
--

data Solution a b = Solution (b a)
    deriving (Show)

-- kind Solution is * -> (* -> *) -> *
--

data OtherSolution a b c = OtherSolution a (c b) 
    deriving (Show)

class Tofu t where
    tofuLift :: a b -> t b a

instance Tofu Solution where
    tofuLift = Solution 




