double x = x + x

righttriangle a b c = (square a) + (square b) - (square c) == 0

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





