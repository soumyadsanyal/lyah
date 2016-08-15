module StringTransmitter where


type Bit = Int

binToInt :: [Bit] -> Int
binToInt bins = foldr (\x y -> x+2*y) 0 bins

binToIntExec :: [Bit] -> Int
binToIntExec = binToInt.reverse

toBin :: Int -> [Bit]
toBin x = (remainder x) : if x==0 then [] else (toBin $ (\x -> div x 2) x)
	where remainder = (\x -> mod x 2)


make8 :: [Bit] -> [Bit]
make8 bits = take 8 $ bits ++ repeat 0


