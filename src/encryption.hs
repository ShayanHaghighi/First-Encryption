import Crypto.Hash
import qualified Data.ByteString.Char8 as C8
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.Time
import System.IO.Unsafe
import System.Random
import Data.Time.Clock.POSIX
import System.Environment


type Hex = Int

--module encryption where

main :: IO ()
main = do
        args <- getArgs
        putStrLn $ encode (concatStrings args)


concatStrings :: [String] -> String
concatStrings [] = []
concatStrings (x:xs) = x ++ " " ++ (concatStrings xs)

------------------------------Getting the Key-----------------------------------------


-- Calculate SHA-256 hash
sha256Hash :: String -> Digest SHA256
sha256Hash input = hash (C8.pack input)

digestToString :: Digest SHA256 -> String
digestToString = unpack . convertToBase Base16

charToHex :: Char -> Hex
charToHex 'a' = 10
charToHex 'b' = 11
charToHex 'c' = 12
charToHex 'd' = 13
charToHex 'e' = 14
charToHex 'f' = 15
charToHex x = ((ord x) - ord '1') + 1

stringtToHex :: String -> Hex
stringtToHex [] = 0
stringtToHex (x:[]) = (charToHex x)
stringtToHex (x:y:zs) = ((charToHex x )*16) + (charToHex y)

hexToList :: String -> [Hex]
hexToList [] = []
hexToList (x:[]) = (stringtToHex [x]):[]
hexToList (x:zs) = (stringtToHex [x,(head zs)]):(hexToList zs)

modList :: Int -> [Hex] -> [Hex]
modList _ [] = []
modList 0 xs = []
modList n (x:xs) = (x `mod` n):(modList (n-1) xs)

--is called with the second argument as [A..Z]
makeAlphabet :: [Char] -> [Hex] -> [Char]
makeAlphabet [] _ = []
makeAlphabet (ys) (x:xs) = (ys !! x):(makeAlphabet (removeIndex x ys) xs)

removeIndex :: Int -> [a] -> [a]
removeIndex x [] = []
removeIndex 0 (y:ys) = ys
removeIndex x (y:ys) = y:(removeIndex (x-1) ys)

genList :: [Char]
genList = makeAlphabet (['A'..'Z']++[' ']) $ modList 27 $ hexToList $ digestToString $ sha256Hash $ getDate $ toString getCurrentDate


--------------------------Subbing in the key---------------------------------------------


substitution :: String -> String -> String
substitution [] _ = ""
substitution (x:xs) key = if not (isLetter x) && not (x == ' ') then x : (substitution xs key) else changeLetter x key : substitution xs key

changeLetter :: Char -> String -> Char
changeLetter c ys = if isUpper c then toUpper (ys !! charLabel c) else toLower (ys !! charLabel c)

charLabel :: Char -> Int
charLabel ' ' = 26                                   -----Character list used here
charLabel char =  ord (toUpper char) - ord 'A'                          -----Character list used here

subKey :: String -> String
subKey string = substitution string genList


------------------------------Getting Date-----------------------------------------


getCurrentDate :: IO String
getCurrentDate = do
    now <- getCurrentTime
    let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d" now
    return formattedDate

toString :: IO String -> String
toString ioStr = unsafePerformIO ioStr

getDate :: String -> String
getDate [] = []
getDate (xs) = [(xs !! 8), (xs !! 9), (xs !! 5), (xs !! 6), (xs !! 0), (xs !! 1), (xs !! 2), (xs !! 3)]



-----------------------------------adding salt----------------------------------------------------

noSaltEncode :: String -> String
noSaltEncode string = [] ++ (subKey string)

getCurrentTimeInSeconds :: IO Integer
getCurrentTimeInSeconds = do
                            round <$> getPOSIXTime

toInt :: IO Integer -> Int
toInt ioInteger = unsafePerformIO $ do
                                        fromIntegral <$> ioInteger

generateRandomNumber :: Int
generateRandomNumber = let
    gen = mkStdGen (toInt getCurrentTimeInSeconds)
    (randomNum, _) = randomR (1, 100) gen
    in randomNum

salt :: [Hex]
salt = hexToList $ digestToString $ sha256Hash $ show $ toInt getCurrentTimeInSeconds

saltLength :: Hex -> Int -> Int
saltLength x n = if (x `mod` (n*2)) < n then (x `mod` (n*2)) + n else x `mod` n*2

createSalt :: Int -> [Hex] -> String
createSalt 0 _ = []
createSalt y [] = createSalt y (reverse salt)
createSalt y (x:xs) = (getRandomChar x):(createSalt (y-1) (xs))

getRandomChar :: Int -> Char
getRandomChar x = (['a'..'z']++[' ']++['A'..'Z']) !! (x `mod` 53)                     -----Character list used here

finalSalt :: String -> String
finalSalt string = createSalt (saltLength (head salt) (length string)) salt ++ string ++ createSalt (saltLength (salt !! 1) (length string)) (reverse salt)

encode :: String -> String
encode string = finalSalt (noSaltEncode ([' '] ++ string ++ [' ']))