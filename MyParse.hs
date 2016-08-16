module MyParser where
import Data.Char (digitToInt)

type Parser a = String -> [(a, String)]

-- need three kinds of parsers: (1) that returns whatever its argument is and doesn't touch the string to be parsed, (2) that fails on any input and returns no result, (3) that bites off the first character and returns the character with the tail 
--


returnParse :: a -> Parser a
returnParse = \value -> \input -> [(value, input)]

rereturnParse :: Char -> String -> [(Char, String)]
rereturnParse = \c ->
	\inp ->
		[(c, inp)]

failParse :: Parser a
failParse = \input -> []

refailParse :: String -> [(Char, String)] 
refailParse = \inp -> []

biteParse :: Parser Char
biteParse = \input -> case input of
	[] -> []
	(x:xs) -> [(x,xs)]

rebiteParse :: String -> [(Char, String)] 
rebiteParse = \inp ->
	case inp of
		[] -> []
		(x:xs) -> [(x, xs)]

parseEval :: Parser a -> String -> [(a, String)]
parseEval p input = p input 

reparseEval :: (String -> [(Char, String)]) -> String -> [(Char, String)]
reparseEval = \p ->
	\s ->
		p s

-- want to sequence parsers
--

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = \input -> case parseEval p input of
	[] -> []
	[(result, residue)] -> parseEval (f result) residue 

-- this basically has the overall structure of: bind p f = \inp -> case p inp of 
--	Fail -> Fail
--	[this, that] -> (f this) that

--where (a -> Parser b) is a function selecting a parser for each result of type a
--

--let's try something like that
--

q = bind biteParse (\x -> if x=='a' then failParse else biteParse)
r = bind (returnParse 'b') (\x -> if x == 'a' then failParse else returnParse 'c' )

s :: Char -> Parser Char
s initial = bind (biteParse) (\x1 ->
	if x1==initial then failParse else (returnParse initial))

failsWheneverProhibitedChar :: Char -> Parser Char
failsWheneverProhibitedChar c = bind biteParse (\result ->
	if result==c then (returnParse 'x') else (failsWheneverProhibitedChar c))

addFirstTwo :: Parser Int
addFirstTwo = bind biteParse (\x ->
  bind biteParse (\y ->
		returnParse ( (digitToInt x) + (digitToInt y)  )
		))


--addAll = process p f = \inp -> case p inp of
--	Fail -> Fail
--	[this, that] -> (f this) that

--need that (f this) = if this is safe then p else Fail
--that is that
--p "" = Success
--p xxx = Fail
-- Success = [('1', "")]
-- Fail = [] 
-- what does this have to do with binding? need to branch on the result of the parse step - if prohibited, then fail, else recurse

noCharIn :: Char -> String -> [(Char, String)]
noCharIn = \c -> \inp -> (case parseEval biteParse inp of 
	[] -> [('1', "")]
	[(x, rest)] -> if x==c then [] else noCharIn c rest)

noCharsIn :: [Char] -> String -> [(Char, String)]
noCharsIn = \s -> \inp -> (case parseEval biteParse inp of 
	[] -> [('1', "")]
	[(x, rest)] -> if elem x s then [] else noCharsIn s rest)

-- example collecting three results and returning the value of a function applied to them
--

soumya :: ([Int] -> Int) -> Parser Int
soumya = \f -> bind biteParse (\x ->
	bind biteParse (\y ->
		bind biteParse (\z ->
			returnParse (f $ map Data.Char.digitToInt [x,y,z]) ) ) )

data Try a = Fail | Succeed a
	deriving (Show, Eq, Read, Ord)

--soumya :: (a -> b) -> ([a] -> Try [a]) -> Try a
--soumya = \f ->
--	\g ->
--		\inp ->
--			case g inp of
--				Fail -> Fail
--				Succeed (x:xs) -> soumya (f x) (g xs)

--g = 
--
--This needs to be finished ...

try :: Parser a -> Parser a -> Parser a
try p q = \inp -> case parseEval p inp of
	[] -> parseEval q inp
	return -> return

predParse :: (Char -> Bool) -> Parser Char
predParse p = bind biteParse (\x ->
	if p x then returnParse x else failParse)

repred :: (Char -> Bool) -> String -> [(Char, String)]
repred = \p ->
	\inp ->
		case biteParse inp of 
			[] -> []
			[(x,xs)] -> if p x then [(x,xs)] else []




