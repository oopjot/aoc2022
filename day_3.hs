-- DAY 3
import Control.Monad
import Data.Char
import System.IO

halve :: [a] -> ([a], [a])
halve l = (take s l, drop s l)
  where
    s = (length l) `div` 2

prioritize :: Char -> Int
prioritize item
  | isUpper item = (ord item) - 38
  | otherwise = (ord item) - 96

searchCompartments :: String -> String -> Char
searchCompartments c1 c2 = fst . head $ [(a, b) | a <- c1, b <- c2, a == b]

-- PART ONE
--main = do
--        contents <- readFile "input_3.txt"
--        print $ foldl (+) 0 
--            . map (prioritize . uncurry searchCompartments . halve) 
--            $ lines contents
-- PART TWO
myFst :: (a, a, a) -> a
myFst (a, _, _) = a

tuplify :: [a] -> (a, a, a)
tuplify [a, b, c] = (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

searchRucksacks :: String -> String -> String -> Char
searchRucksacks r1 r2 r3 =
  myFst . head $ [(a, b, c) | a <- r1, b <- r2, c <- r3, a == b && a == c]

groups :: [a] -> [(a, a, a)]
groups [] = []
groups l = (tuplify $ take 3 l) : (groups (drop 3 l))

main = do
  contents <- readFile "input_3.txt"
  print $
    foldl (+) 0 . map (prioritize . uncurry3 searchRucksacks) $
    groups $ lines contents
