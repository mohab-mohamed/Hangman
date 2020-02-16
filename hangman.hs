import System.Random
import Control.Monad

main :: IO ()
main = do
	content <- readFile "words.txt"
	let linesOfFiles = lines content
	let numberedList = zip [1..length linesOfFiles] linesOfFiles
	randomNumber <- getStdRandom(randomR (1, 45407))
	let word = grabRandomWord numberedList randomNumber
	let wordLength = length word
	let testword = "For testing purposes the word is: " ++ word
	putStrLn testword --COMMENT THIS LINE OUT TO NOT GIVE AWAY THE ANSWER
	let numberOfLetters = show wordLength ++ " letters"
	putStrLn numberOfLetters
	let initialD = initialDash word wordLength
	putStrLn initialD
	let zipWord = zip [0..wordLength] word
	myLoop word initialD
	return()

myLoop :: String -> String -> IO ()
myLoop word guess = do 
                     line <- getLine
                     let c = head line
                     let newGuess = letterBeatsDash guess (dashOrLetter word c)
		     putStrLn (letterBeatsDash guess (dashOrLetter word c))
                     putStrLn (check word newGuess)
                     if (check word newGuess) == "You Win!" then
                         return()
                     else
                         myLoop word (letterBeatsDash guess (dashOrLetter word c))

check :: String -> String -> String
check word guess = if word == guess then "You Win!" else "Pick another letter:"

letterBeatsDash :: String -> String -> String
letterBeatsDash [] [] = ""
letterBeatsDash (x : xs) (y : ys)
	|x == y					= x : letterBeatsDash xs ys
	|x >= 'a' && x <= 'z' && y == '_'	= x : letterBeatsDash xs ys
	|x == '_' && y >= 'a' && y <= 'z'	= y : letterBeatsDash xs ys
	|x == '_' && y == '_'			= '_' : letterBeatsDash xs ys

dashOrLetter :: String -> Char -> String
dashOrLetter [] w = ""  
dashOrLetter (x : zs) w 
	|x == w		= x : dashOrLetter zs w
	|x /= w 	= '_' : dashOrLetter zs w
	|otherwise 	= ""

initialDash :: String -> Int -> String 
initialDash word n
	|n > 0		= "_" ++ initialDash word (n - 1)
	|otherwise	= ""

grabRandomWord :: [(Int, String)] -> Int -> String
grabRandomWord ((x, y):zs) n
	|x == n		= y
	|otherwise = grabRandomWord (zs) n

{- USED TO FIND OUT HOW MANY WORDS WERE IN "words.txt"
printTuples :: [(Int, String)] -> IO ()
printTuples xs = forM_ xs (putStrLn . formatOne)

formatOne :: (Int, String) -> String
formatOne (s, i) = show s ++ " : " ++ show i
 -}
