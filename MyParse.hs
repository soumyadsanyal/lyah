module MyParser where

type Parser a = String -> [(a, String)]

-- need three kinds of parsers: (1) that returns whatever its argument is and doesn't touch the string to be parsed, (2) that fails on any input and returns no result, (3) that bites off the first character and returns the character with the tail 
--


returnParse :: a -> Parser a
returnParse = \value -> \input -> [(value, input)]

failParse :: Parser a
failParse = \input -> []

biteParse :: Parser Char
biteParse = \input -> case input of
	[] -> []
	(x:xs) -> [(x,xs)]

parseEval :: Parser a -> String -> [(a, String)]
parseEval p input = p input 


-- want to sequence parsers
--

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = \input -> case parseEval p input of
	[] -> []
	[(result, residue)] -> parseEval (f result) residue 


--where (a -> Parser b) is a function selecting a parser for each result of type a
--

--let's try something like that
--

q = bind biteParse (\x -> if x=='a' then failParse else biteParse)
r = bind (returnParse 'b') (\x -> if x == 'a' then failParse else returnParse 'c' )

s :: Char -> Parser Char
s initial = bind (biteParse) (\x1 ->
	if x1==initial then failParse else (returnParse initial))



