{-# LANGUAGE MultiWayIf #-}
module Main where
import Data.Char
import Data.Maybe
import Data.List

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


initMatrix_ :: [String] ->  Int ->  Int -> [[Int]] -> [[Int]]
initMatrix_ (x:xs) x_s y_s list_out = if 
    | any isDigit x && null list_out-> initMatrix_ xs width high  list_out
    | otherwise -> if 
        | not (null xs) -> initMatrix_ xs x_s y_s $ map tr x : list_out   
        | otherwise -> if 
            | checkHigh y_s out && checkWidth x_s out -> out
            | otherwise -> error "COLUMNS AND ROWS ERROR"

    where xy = xySize x 
          high = head xy 
          width =  head (tail xy)
          tr = translate . decode
          out = map tr x : list_out

initMatrix list = initMatrix_ list 0 0 []

findFirst_ :: [Int] -> Int -> Maybe Int
findFirst_ list index = if
                    | list !! index == 1 -> Just index
                    | otherwise -> if
                                | index < (length list -1)  -> findFirst_ list $ index + 1
                                | otherwise -> Nothing


findFirst :: [Int] -> Maybe Int
findFirst list = findFirst_ list 0


minX :: [[Int]] -> Int
minX matrix = minimum $ map fromJust $ filter isJust $ map findFirst matrix


maxX :: [[Int]] -> Int
maxX matrix = let matrixR = map reverse matrix
                  len = (length . head) matrix -1
              in  len -  minX  matrixR


minY :: [[Int]] -> Int
minY matrix = let matrixT = transpose matrix
              in minX matrixT

maxY :: [[Int]] -> Int
maxY matrix = let matrixT = transpose matrix
              in maxX matrixT
 

matrixSum :: [[Int]] -> Int
matrixSum matrix = sum $ map sum matrix


mainCheck :: [[Int]] -> Int
mainCheck matrix = case matrixSum matrix of 
                    0 -> if null matrix  then -1 else 1 
                    1 -> 0
                    x -> if
                        | (a > max_width) || (a > max_high) -> -1 
                        | otherwise -> (a*a) - x

                    where min_x = minX matrix
                          min_y = minY matrix
                          max_x = maxX matrix
                          max_y = maxY matrix
                          width = max_x - min_x
                          high = max_y - min_y
                          max_width = length $ head matrix
                          max_high = length matrix
                          a = maximum [width, high] + 1

--input monad IO string => apply string in context to combine of mainCheck and
--initMatrix ==> print information in context of IO string
main :: IO()
main = (mainCheck . initMatrix <$> inputFile nameOfFile) >>= print


