module MyParser where
import Data.Char 

data Try a = Fail | Succeed a
	deriving (Show, Eq, Read, Ord)

type Parser a = [a] -> Try (a, [a])

type ParseValues a = [a]


-- need three kinds of parsers: (1) that returns whatever its argument is and doesn't touch the string to be parsed, (2) that fails on any input and returns no result, (3) that bites off the first character and returns the character with the tail 
--

safeReturnParse :: a -> Parser a
safeReturnParse = \c ->
	\s ->
		Succeed (c,s)

safeFailParse :: Parser a
safeFailParse = \inp ->
	Fail

safeBiteParse :: Parser a
safeBiteParse = \inp ->
	case inp of
		[] -> Fail
		(x:xs) -> Succeed (x, xs)

safeParseEval :: (Parser a) -> Parser a
safeParseEval p input = p input 

-- want to sequence parsers
--

safeBind :: (Parser a) -> (a -> (Parser a)) -> (Parser a)
safeBind p f = \input -> case safeParseEval p input of
	Fail -> Fail
	Succeed (result, residue) -> safeParseEval (f result) residue 

safeTry :: (Parser a) -> (Parser a) -> (Parser a) 
safeTry p q = \inp -> case safeParseEval p inp of
	Fail -> safeParseEval q inp
	returned -> returned

safePredParse :: (a -> Bool) -> Parser a
safePredParse = \p ->
	\inp ->
		case safeParseEval safeBiteParse inp of 
			Fail -> Fail
			Succeed (x,xs) -> if p x then Succeed (x,xs) else Fail

safeDigit :: Parser Char
safeDigit = safePredParse isDigit

safeLower :: Parser Char
safeLower = safePredParse isLower

safeUpper :: Parser Char
safeUpper = safePredParse isUpper

safeLetter :: Parser Char
safeLetter = safePredParse isAlpha

safeAlphanum :: Parser Char
safeAlphanum = safePredParse isAlphaNum

safeChar :: Char -> Parser Char
safeChar = \c -> safePredParse ( == c)

-- let's try to write our own do blocks

safeDoHelper :: [(Parser a)] -> ParseValues a -> ((ParseValues a) -> b) -> ((ParseValues a) -> b)
safeDoHelper plist store reducer = \inp -> 
	case (plist, store) of
		([], store) -> reducer $ reverse store
		((p:plist'), store) -> safeDoHelper plist' store' reducer residue
			where (store', residue) = case safeParseEval p inp of
				Fail -> (store, inp)
				Succeed (result, residue) -> ((result:store), residue)

	
safeDo :: [(Parser a)] -> ((ParseValues a) -> b) -> ((ParseValues a) -> b) 
safeDo plist reducer = safeDoHelper plist [] reducer


