myfoldl f c l
 | null l = c
 | True = myfoldl f (f c x) xs
 where (x:xs) = l

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f c l 
 | null l = c
 | True = f x (myfoldr f c xs)
 where (x:xs) = l

mapper :: (a -> b) -> [a] -> [b]
mapper g = myfoldr (\x list -> (g x):list) [] 

data Household = Soumya | Sarah | Bronzie | Scarlet | Sam

type Location = String
type Names = [String]
type Ages = [Int]

--data Home = Family Names Ages Location | DispersedFamily Names Ages [Location] deriving (Show, Eq, Read)

type Name = String
type Age = Int

data FamilyMember = FamilyMember {
		firstname:: Name,
		lastname:: Name,
		age:: Age,
		location::Location}
		deriving (Show, Eq, Read)

data Family = Family [FamilyMember]
		deriving (Show, Eq, Read)


data Safe a b = Safe a | Failed b
		deriving (Show, Eq, Read)

data SafeInt = SafeInt Int | FailedInt String
		deriving (Show, Eq, Read)

guaranteedPositive :: Int -> SafeInt
guaranteedPositive x = if (x<0) then (FailedInt "negative") else SafeInt x




