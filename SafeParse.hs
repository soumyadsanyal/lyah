module MyParser where
import Data.Char 

data Try a = Fail | Succeed a
	deriving (Show, Eq, Read, Ord)

type Parser a = String -> [(a, String)]

-- need three kinds of parsers: (1) that returns whatever its argument is and doesn't touch the string to be parsed, (2) that fails on any input and returns no result, (3) that bites off the first character and returns the character with the tail 
--

safeReturnParse :: a -> [a] -> Try (a, [a])
safeReturnParse = \c ->
	\s ->
		Succeed (c,s)

safeFailParse :: [a] -> Try (a, [a])
safeFailParse = \inp ->
	Fail

safeBiteParse :: [a] -> Try (a, [a])
safeBiteParse = \inp ->
	case inp of
		[] -> Fail
		(x:xs) -> Succeed (x, xs)

safeParseEval :: ([a] -> Try (a, [a])) -> [a] -> Try (a, [a])
safeParseEval p input = p input 

-- want to sequence parsers
--

safeBind :: ([a] -> Try (a, [a])) -> (a -> ([a] -> Try (a, [a]))) -> ([a] -> Try (a, [a]))
safeBind p f = \input -> case safeParseEval p input of
	Fail -> Fail
	Succeed (result, residue) -> safeParseEval (f result) residue 

safeTry :: ([a] -> Try (a, [a])) -> ([a] -> Try (a, [a])) -> ([a] -> Try (a, [a])) 
safeTry p q = \inp -> case safeParseEval p inp of
	Fail -> safeParseEval q inp
	return -> return

safePredParse :: (a -> Bool) -> [a] -> Try (a, [a])
safePredParse = \p ->
	\inp ->
		case safeBiteParse inp of 
			Fail -> Fail
			Succeed (x,xs) -> if p x then Succeed (x,xs) else Fail

safeDigit :: String -> Try (Char, String)
safeDigit = safePredParse isDigit

safeLower :: String -> Try (Char, String)
safeLower = safePredParse isLower

safeUpper :: String -> Try (Char, String)
safeUpper = safePredParse isUpper

safeLetter :: String -> Try (Char, String)
safeLetter = safePredParse isAlpha

safeAlphanum :: String -> Try (Char, String)
safeAlphanum = safePredParse isAlphaNum

safeChar :: Char -> String -> Try (Char, String)
safeChar = \c -> safePredParse ( == c)






