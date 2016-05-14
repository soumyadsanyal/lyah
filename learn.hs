import qualified Data.Map as Map

double x = x + x

-- righttriangle a b c = (square a) + (square b) - (square c) == 0

-- square :: (Num a) => a -> a
-- square x = x^2

doublesmall x = if x>100 then x else x*2

soumya = "Soumya Deepta Sanyal"

revhelper x y = case x of
 []-> y
 _ -> revhelper (tail x) ((head x): y)

rev x = if x==[] then x else revhelper x []


listaddhelper x y flipped = case flipped of
 False -> listaddhelper (rev x) y True
 True -> case x of 
  [] -> y
  _ -> listaddhelper (tail x) ((head x): y) True

listadd x y = if x == [] then y else listaddhelper x y False

listadd' x y = if x == [] then y else listadd' (init x) ((last x): y) 

retrieve :: (Eq a) => [a] -> Int -> a
retrieve list n = if ((n<0) || (list==[])) then error "Index out of range" else 
 (case n of
 0 -> head list
 _ -> retrieve (tail list) (n-1)
 )

lessthan:: (Ord a) => [a] -> [a] -> Bool
lessthan first second = case first of 
    [] -> True
    _ -> (case second of
        [] -> False
        _ -> (let 
            x = head first 
            y = head second 
            in case ((x<y), (x==y), (x>y)) of
                (True, False, False) -> True
                (False, True, False) -> lessthan (tail first) (tail second)
                (False, False, True) -> False
             )
        )
    
len :: [a] -> Int
len list = case list of
    [] -> 0
    _ -> 1+(len (tail list))

len' :: [a] -> Int
len' xs = sum [1 | _<- xs]



empty :: [a] -> Bool
empty list = case list of
 [] -> True
 _ -> False


taker :: [a] -> Int -> [a]
taker list quantity 
 | quantity < 0 = error "can't take less than zero things"
 | quantity ==0 || (empty list) = []
 | quantity >0 = (head list): (taker (tail list) (quantity - 1))

dropper :: [a] -> Int -> [a] 
dropper list quantity
 | quantity <= 0 || (empty list) = list
 | quantity > 0 = dropper (tail list) (quantity - 1)

biggesthelper :: (Ord a) => [a] -> [a] -> a
biggesthelper list container
 | empty list = if (empty container) then error "empty list!" else head container
 | (empty container) || (head container) < (head list) = biggesthelper (tail list) ((head list):[])
 | otherwise = biggesthelper (tail list) container

biggest :: (Ord a) => [a] -> a
biggest list = biggesthelper list []

smallesthelper :: (Ord a) => [a] -> [a] -> a
smallesthelper list container
 | empty list = if (empty container) then error "empty list!" else head container
 | (empty container) || (head container) > (head list) = smallesthelper (tail list) ((head list):[])
 | otherwise = smallesthelper (tail list) container

smallest :: (Ord a) => [a] -> a
smallest list = smallesthelper list []

summing :: (Num a) => [a] -> a
summing list
 | empty list = 0
 | otherwise = (head list) + summing (tail list)

timesing :: (Num a) => [a] -> a
timesing list
 | empty list = 1
 | otherwise = (head list) * timesing (tail list)

isIn :: (Eq a) => [a] -> a -> Bool
isIn list thing 
 | empty list = False
 | thing == head list = True
 | otherwise = isIn (tail list) thing

cycler :: (Eq a) => [a] -> [a]
cycler list = let result = (listadd' list result) in result

repeater :: (Eq a) => a -> [a]
repeater thing = cycler (thing:[])

replicater :: (Eq a) => a -> Int -> [a]
replicater thing times = taker (repeater thing) times

odd' candidate = if mod candidate 2 == 1 then True else False

removenumbers :: [Char] -> [Char]
removenumbers list = [num | num<- list, not (elem num ['0'..'9'])]

righttriangles = [(a,b,c) | c<-[1..10], b<-[1..c], a<-[1..b], a^2 + b^2 == c^2, a+b+c==24]

initials :: [String] -> String
initials list
 | list==[] = ""
 | True = [x] ++ "." ++ (initials rest)
   where first:rest = list
         x:_ = first


-- Soumya Deepta Sanyal [] []
-- oumya Deepta Sanyal "S":[] []
-- ...
-- a Deepta Sanyal "ymuoS":[] []
--  Deepta Sanyal "aymuoS":[] []
-- Deepta Sanyal [] "Soumya":[]

splithelper :: String -> String -> [String] -> [String]
splithelper thestring holder result
 | thestring == "" = (reverse holder):result
 | head thestring == ' ' = splithelper (tail thestring) [] ((reverse holder):result)
 | True = splithelper (tail thestring) ((head thestring):holder) result 

split :: String -> [String]
split thestring = reverse (splithelper thestring [] [])

biggest' :: (Ord a) => [a] -> a
biggest' list = case list of 
 [] -> error "Empty list!"
 [x] ->  x
 (x:xs) -> if x>maxtail then x else maxtail
           where maxtail = biggest xs

biggest'' :: (Ord a) => [a] -> a
biggest'' list = case list of
 [] -> error "empty list"
 [x] -> x
 (x:xs) -> max x (biggest'' xs)

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x
 | n <=0 = []
 | True = x: (replicate' (n - 1) x)

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n xs 
 | n<=0 || null xs = []
 | True = y:(take' (n-1) ys)
 where (y:ys) = xs

repeat' :: a -> [a]
repeat' x = x:(repeat' x)

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys
 | (null xs) || (null ys) = []
 | True = (u,v):(zip' us vs)
 where {(u:us) = xs ;  (v:vs) = ys}

elem' :: (Eq a) => a -> [a] -> Bool
elem' x ys
 | null ys = False
 | x == head ys = True
 | True = elem' x (tail ys)

quicksort :: (Ord a) => [a] -> [a]
quicksort xs 
 | null xs = []
 | True = smallersorted ++ [y] ++ biggersorted
 where (y:ys) = xs
       smallersorted = quicksort [t | t<- ys, t<=y]
       biggersorted = quicksort [t | t<- ys, t>y]

iterateapply :: (a -> a) -> Int  -> (a -> a)
iterateapply f n 
 | n<=0 = error "not applying anything at all!"
 | n == 1 = f
 | True = f . (iterateapply f (n-1))

iterateforever :: (a -> a) -> a -> [a]
iterateforever f x = map ($ x) $ map (iterateapply f) [1..]

zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' f xs ys
 | null xs || null ys = []
 | True = f u v : zipwith' f us vs
 where (u:us) = xs
       (v:vs) = ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f xs
 | null xs = []
 | True = f y : map' f ys
 where (y:ys) = xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs
 | null xs = []
 | p y = y : filter' p ys
 | True = filter' p ys
 where (y:ys) = xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' xs
 | null xs = []
 | True = smallersorted ++ [y] ++ biggersorted
 where (y:ys) =  xs
       smallersorted = quicksort' (filter' (<=y) ys)
       biggersorted = quicksort' (filter' (>y) ys)
      
biggestMultiple :: (Integral a) => a -> a -> a
biggestMultiple l f = head (filter p [l, l-1..0])
 where p x  = mod x f == 0

takewhile :: (a-> Bool) -> [a] -> [a]
takewhile p xs 
 | null xs || (not (p y))= []
 | p y = y: takewhile p ys
 where (y:ys) = xs

sumoddsquares :: Int -> Int
sumoddsquares l = sum (takewhile (<l) (filter odd (map (^2) [1..])))

collatz :: (Integral a) => a -> [a]
collatz n
 | n==1 = n:[]
 | even n = n:(collatz (div n 2))
 | odd n = n:(collatz (3*n + 1))

numlongchains :: Int -> Int -> Int
numlongchains m l = length (filter (>l) (map length (map collatz (take m [1..]))))

eval :: [(a->b)] -> [a] -> [b]
eval f l
 | null l = []
 | True = (head f) (head l) : (eval (tail f) (tail l))

getindex :: [a] -> Int -> a
getindex l n
 | n<=0 = head l
 | True = getindex (tail l) (n-1)


addthree :: Int  -> Int -> Int -> Int
addthree = \x -> \y -> \z -> x+y+z

flip'' :: (a -> b -> c) -> (b-> a -> c)
flip'' f = \x -> \y -> f y x

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f c l
 | null l = c
 | True = myfoldl f (f c (head l)) (tail l)

-- implementing elem using myfoldl
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = myfoldl (\c y -> if x==y then True else c) False xs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f c l
 | null l = c
 | True = f (head l) (myfoldr f c (tail l))

map'' :: (a -> b) -> [a] -> [b]
map'' g xs = myfoldr (\y c -> (g y): c ) [] xs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x c -> if x>c then x else x)

reverse'' :: [a] -> [a]
reverse'' = myfoldl (\c x -> x:c) []

product'' :: (Num a) => [a] -> a
product'' = foldr1 (\x c -> x*c)


head'' :: [a] -> a
head'' = foldr1 (\x _ -> x)

last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = myfoldr (\x c -> if p x then x: c else c) []

evallist :: a -> [(a-> b)] -> [b]
evallist arg l = map ($ arg) l

oddsquaresum :: Integer
--oddsquaresum = sum (takeWhile (<10000) (map filter odd ((^2) [1..])))
oddsquaresum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

myintersperse :: a -> [a] -> [a]
myintersperse x l = case l of
 (a:[]) -> a:[]
 (a:xs) -> a:x:(myintersperse x xs)

myintercalate :: [t] -> [[t]] -> [t]
myintercalate l ls = case ls of 
 (a:[]) -> a++[]
 (a:xs) -> a++l++(myintercalate l xs)

myany :: (a -> Bool) -> [a] -> Bool
myany p l
 | null l = False
 | True = if p $ head l then True else myany p $tail l

myall :: (a -> Bool) -> [a] -> Bool
myall p l
 | null l = True
 | True = p (head l) && (myall p $ tail l)

mytranspose :: [[a]] -> [[a]]
mytranspose l 
 | myany id $ map null l = []
 | True = (map head l) : (mytranspose $ map tail l)

myconcat :: [[a]] -> [a]
myconcat l
 | null l = []
 | True = head l ++ (myconcat $ tail l)

myconcatmap :: (a -> b) -> [[a]] -> [b]
myconcatmap f l
 | null l = []
 | True = (map f (head l)) ++ (myconcatmap f $ tail l)

chomp :: Int -> [a] -> [a]
chomp n l
 | null l || n<=0 = []
 | True = (head l) : chomp (n-1) (tail l)

mytailshelper :: [a] -> [[a]]
mytailshelper l
 | null l = []
 | True = (tail l) : (mytailshelper (tail l))

mytails :: [a] -> [[a]]
mytails l = mytailshelper $ (head l):l

mylast :: [a] -> a
mylast l = case l of
 [] -> error "empty list"
 (x:[]) -> x
 _ -> mylast $ tail l

myinit :: [a] -> [a]
myinit l = case l of
 [] -> []
 (x:[]) -> []
 _ -> (head l) : (myinit $ tail l)

myinitshelper :: [a] -> [[a]]
myinitshelper l
 | null l  = []
 | True = (myinit l):(myinitshelper $myinit l)

myinits :: [a] -> [[a]]
myinits l = reverse $ myinitshelper $ l ++ (mylast l):[]

mysplitat :: Int -> [a] -> ([a], [a])
mysplitat n l = (take n l, drop n l)

mytakewhile :: (a -> Bool) -> [a] -> [a]
mytakewhile p l
 | null l = l
 | p $ head l = (head l): (mytakewhile p $ tail l)
 | True = []

mydropwhile :: (a -> Bool) -> [a] -> [a]
mydropwhile p l
 | null l = []
 | p $ head l = mydropwhile p $ tail l
 | True = l

myspan :: (a -> Bool) -> [a] -> ([a],[a])
myspan p l = (mytakewhile p l, mydropwhile p l)

mybreak :: (a -> Bool) -> [a] -> ([a],[a])
mybreak p l = myspan (not . p) l

-- mygrouphelper :: (Eq a) => [[a]] -> [a] -> [[a]]
-- mygrouphelper s l
--  | null l = reverse s
--  | null s = mygrouphelper (((head l):[]):s) (tail l)
--  | (head $ head s) == head l = mygrouphelper (((head l):(head s)):(tail s)) $ tail l
--  | True = mygrouphelper (((head l):[]):s) $ tail l

-- mygroup :: (Eq a) => [a] -> [[a]]
-- mygroup l = mygrouphelper [] l

mysearch :: (Eq a) => [a] -> [a] -> Bool
mysearch needle haystack = myfoldl (\c x -> if (take (length needle) x == needle) then True else c) False $ mytails haystack

myisprefixof :: (Eq a) => [a] -> [a] -> Bool
myisprefixof s t = if take (length s) t == s then True else False

myissuffixof :: (Eq a) => [a] -> [a] -> Bool
myissuffixof s t = myisprefixof (reverse s) (reverse t)

myelem :: (Eq a) => a -> [a] -> Bool
myelem e l 
 | null l = False
 | True = if e == head l then True else myelem e $ tail l

mypartitionhelper :: (a -> Bool) -> [a] -> [a] -> [a] -> ([a],[a])
mypartitionhelper p l g b
 | null l = (g,b)
 | True = if (p $ head l) then mypartitionhelper p (tail l) ((head l):g) b else mypartitionhelper p (tail l) g ((head l):b)

mypartition :: (a -> Bool) -> [a] -> ([a],[a])
mypartition p l = mypartitionhelper p (reverse l) [] []

myfind :: (a -> Bool) -> [a] -> Maybe a
myfind p l
 | null l = Nothing
 | p $ head l = Just (head l)
 | True = myfind p $ tail l

myelemindexhelper :: (Eq a) => a -> [a] -> Int -> Maybe Int
myelemindexhelper e l i
 | null l = Nothing
 | e == head l = Just i
 | True = myelemindexhelper e (tail l) (i+1)

myelemindex :: (Eq a) => a -> [a] -> Maybe Int
myelemindex e l  = myelemindexhelper e l 1

myelemindiceshelper :: (Eq a) => a -> [a] -> Int -> [Int]
myelemindiceshelper e l i
 | null l = []
 | e == head l = i:(myelemindiceshelper e (tail l) (i+1))
 | True = myelemindiceshelper e (tail l) (i+1)

myelemindices :: (Eq a) => a -> [a] -> [Int]
myelemindices e l = myelemindiceshelper e l 1

myfindindexhelper :: (a -> Bool) -> [a] -> Int -> Maybe Int
myfindindexhelper p l i
 | null l = Nothing
 | p $ head l = Just i
 | True = myfindindexhelper p (tail l) (i+1)

myfindindex :: (a -> Bool) -> [a] -> Maybe Int
myfindindex p l  = myfindindexhelper p l 1

myfindindiceshelper :: (a -> Bool) -> [a] -> Int -> [Int]
myfindindiceshelper p l i
 | null l = []
 | p $ head l = i:(myfindindiceshelper p (tail l) (i+1))
 | True = myfindindiceshelper p (tail l) (i+1)

myfindindices :: (a -> Bool) -> [a] -> [Int]
myfindindices p l = myfindindiceshelper p l 1

myzip :: [a] -> [b] -> [(a,b)]
myzip l m
 | null l || null m = []
 | True = (head l, head m):(myzip (tail l) (tail m))

myzipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipwith f l m
 | null l || null m = []
 | True = (f (head l) (head m)):(myzipwith f (tail l) (tail m))

mylineshelper :: [Char] -> [Char] -> [[Char]] -> [[Char]]
mylineshelper l a b
 | null l = reverse ((reverse a):b)
 | (head l) == '\n' = mylineshelper (tail l) [] ((reverse a):b)
 | True = mylineshelper (tail l) ((head l):a) b

mylines :: [Char] -> [[Char]]
mylines l = mylineshelper l [] []

myunlines :: [[Char]] -> [Char]
myunlines l
 | null l = ""
 | True = (head l) ++ "\n" ++ (myunlines (tail l))

bite :: [Char] -> [Char]
bite l
 | null l = []
 | (head l) == ' ' || null l = []
 | True = (head l) : (bite $ tail l)

spit :: [Char] -> [Char]
spit l
 | null l = []
 | (head l) == ' ' = tail l
 | True = (spit $ tail l)

mywords :: [Char] -> [[Char]]
mywords l
 | null l = []
 | True = (bite l) : (mywords (spit l))

myunwords :: [[Char]] -> [Char]
myunwords l
 | null l = []
 | True = (head l) ++ (if null (tail l) then "" else " ") ++ (myunwords $ tail l)

mynubbyhelper :: (a -> a -> Bool) -> [a] -> [a] -> [a] -> [a]
mynubbyhelper p l s r
 | null l = reverse r
 | myany (==True) (map (p (head l)) s) = mynubbyhelper p (tail l) s r
 | True = mynubbyhelper p (tail l) ((head l):s) ((head l):r)

mynubby :: (a -> a -> Bool) -> [a] -> [a]
mynubby p l = mynubbyhelper p l [] []

mynub :: (Eq a) => [a] -> [a]
mynub l = mynubby (==) l

mydeleteby :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteby p x l
 | null l = []
 | p (head l) x = (tail l)
 | True = (head l) : (mydeleteby p x $ tail l)

mydelete :: (Eq a) => a -> [a] -> [a]
mydelete x l = mydeleteby (==) x l

mysetdiff :: (Eq a) => [a] -> [a] -> [a]
mysetdiff a b
 | null b = a
 | elem (head b) a = mysetdiff (tail b) $ mydelete (head b) a
 | True = mysetdiff (tail b) a

myunionby :: (a -> a -> Bool) -> [a] -> [a] -> [a]
myunionby p a b
 | null a = b
 | myany (==True) (map (p (head a)) b) = myunionby p (tail a) b
 | True = (head a): (myunionby p (tail a) b)

myunion :: (Eq a) => [a] -> [a] -> [a]
myunion a b = myunionby (==) a b

myintersectby :: (a -> a -> Bool) -> [a] -> [a] -> [a]
myintersectby p a b
 | null a = []
 | myany (==True) (map (p (head a)) b) = (head a) : (myintersectby p (tail a) b)
 | True = myintersectby p (tail a) b

myintersect :: (Eq a) => [a] -> [a] -> [a]
myintersect = myintersectby (==)

myinserthelper' :: (Ord a) => [a] -> [a] -> [a]
myinserthelper' a b
 | null a = b
 | null b = a
 | (head a) < (head b) = (head a) : b
 | True = (head b) : (myinserthelper' a (tail b))

myinsert' :: (Ord a) => a -> [a] -> [a]
myinsert' x l = myinserthelper' (x:[]) l

myinsert :: (Ord a) => a-> [a] -> [a]
myinsert x l
 | null l = x:[]
 | x < (head l) = x:l
 | True = (head l) : (myinsert x $ tail l)

mygroupbyhelper :: (Eq a) => (a -> a-> Bool) -> [a] -> [a] -> [[a]] -> [[a]]
mygroupbyhelper p l s r
 | null l = reverse $ (reverse s):r
 | null s = mygroupbyhelper p (tail l) ((head l):s) r
 | p (head l) (head s) = mygroupbyhelper p (tail l) ((head l) : s) r
 | True = mygroupbyhelper p (tail l) ((head l):[]) ((reverse s):r)

mygroupby :: (Eq  a) => (a -> a-> Bool) -> [a] -> [[a]]
mygroupby p l = mygroupbyhelper p l [] []

mygroup :: (Eq  a) => [a] -> [[a]]
mygroup l = mygroupbyhelper (==) l [] []

-- placeholder for sortby, insertby, ...

myfindkey :: (Eq k) => k -> [(k,v)] -> Maybe v
myfindkey key hash
 | null hash = Nothing
 | key == test = Just value
 | True = myfindkey key (tail hash)
 where
  (test, value) = head hash

myfindkey' :: (Eq k) => k -> [(k,v)] -> Maybe v
myfindkey' k  = myfoldr (\(test, value) c -> (if k==(test) then (Just value) else c) ) Nothing 

l = [("1", "Soumya"), ("2", "Sarah"), ("3", "Bronzie"), ("4", "Scarlet"), ("5", "Sam")]

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

data Point = Point Float Float deriving (Show)

-- A type is a collection of values. A value constructor is a function mapping parameters to values.
--
--
--

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1-x2) * (abs $ y2-y1)

nudge :: Shape -> Point -> Shape
nudge (Circle (Point x1 y1) r) (Point dx dy) = Circle (Point (x1+dx) (y1+dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) (Point dx dy) = Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect x y = Rectangle (Point 0 0) (Point x y) 


