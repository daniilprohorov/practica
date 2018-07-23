{-# LANGUAGE MultiWayIf #-}
module Main where
import Data.Char
import Data.Maybe
nameOfFile = "input.txt"

inputFile :: String -> IO [String]
inputFile filename = do
    file <- readFile nameOfFile
    let listOfLines = lines file 
    return listOfLines

xySize :: String -> [Int]
xySize str = map read $ words str

translate = fromMaybe (error "FILE DECODE ERROR")  

decode :: Char -> Maybe Int
decode elem = case elem of
            'w' -> Just 0
            'W' -> Just 0
            'b' -> Just 1
            'B' -> Just 1
            _   -> Nothing


checkHigh :: Int -> [[Int]] -> Bool
checkHigh high list = if
    | high == length list -> True
    | otherwise -> False

checkWidth :: Int -> [[Int]] -> Bool
checkWidth width list = if
    | all ((\w l -> w == length l) width) list -> True
    | otherwise -> False

initMatrix :: [String] ->  Int ->  Int -> [[Int]] -> [[Int]]
initMatrix (x:xs) x_s y_s list_out = if 
    | any isDigit x && null list_out-> initMatrix xs width high  list_out
    | otherwise -> if 
        | not (null xs) -> initMatrix xs x_s y_s $ map tr x : list_out   
        | otherwise -> if 
            | checkHigh y_s out && checkWidth x_s out -> out
            | otherwise -> error "COLUMNS AND ROWS ERROR"

    where xy = xySize x 
          high = head xy 
          width =  head (tail xy)
          tr = translate . decode
          out = map tr x : list_out



main :: IO()
main = do 
    lst <- inputFile nameOfFile  
    print $ initMatrix lst 0 0 [] 
    print lst     
     
     
    





