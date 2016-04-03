double x = x + x

righttriangle a b c = (square a) + (square b) - (square c) == 0

square :: (Num a) => a -> a
square x = x^2

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

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f c l
 | null l = c
 | True = foldl' f (f c (head l)) (tail l)

-- implementing elem using foldl'
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldl' (\c y -> if x==y then True else c) False xs












