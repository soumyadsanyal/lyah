
leftfold :: (c -> x -> c) -> c -> [x] -> c
leftfold g c l
 | null l = c
 | True = leftfold g (g c $ head l) $ tail l

rightfold :: (x -> c -> c) -> [x] -> c -> c
rightfold g l c
 | null l = c
 | True = g (head l) (rightfold g (tail l) c)



