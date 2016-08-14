module Tree where

data Tree a = E | Node a (Tree a) (Tree a)
    deriving (Show, Eq)


isE :: (Eq a) => Tree a -> Bool
isE t = (t == E)


contains :: (Ord a, Eq a) => a -> Tree a -> Bool
contains _ E = False
contains x t = let (Node root left right) = t in 
    if x == root then True else if (x<root) then contains x left else contains x right


insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert x t
    | isE t = Node x E E
    | True = if (x<root) then (Node root (insert x left) right) else if (x>root) then (Node root left (insert x right)) else t
    where (Node root left right) = t

a = Node 1 E E
b = Node 2 E E
c = Node 3 a b
d = Node 4 a c

eqTree :: (Eq a) => Tree a -> Tree a -> Bool
eqTree l r
 | (isE l && isE r) = True
 | (isE l || isE r) = False
 | (lnode == rnode) = (eqTree lleft rleft) && (eqTree rleft rright)
 | True = False
 where (Node lnode lleft lright) = l
       (Node rnode rleft rright) = r

fromList :: (Ord a, Eq a) => [a] -> Tree a
fromList = (foldr insert E).reverse



