import Data.Char

--main = do
--	putStrLn "What's your first name?"
--	firstname <- getLine
--	putStrLn "What's your last name?"
--	lastname <- getLine
--	let bigfirstname = map toUpper firstname
--	let biglastname = map toUpper lastname
--	putStrLn $ "hey " ++ bigfirstname ++ " " ++ biglastname ++ ", how are you?"

--main = do
--	line <- getLine
--	if null line
--		then return ()
--		else do
--			putStrLn $reverseWords line
--			main

--reverseWords :: String -> String
--reverseWords = unwords . map reverse . words

main = do
	a <- return "hell"
	b <- return "yeah"
	putStrLn $ a ++ " " ++ b







