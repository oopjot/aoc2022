-- DAY 4
import Control.Monad
import Data.List
import System.IO

mySplit :: Char -> String -> [String]
mySplit _ "" = []
mySplit at s = helper s ""
  where
    helper :: String -> String -> [String]
    helper "" acc = [acc]
    helper (h:t) acc =
      if h == at
        then acc : helper t ""
        else helper t (acc ++ [h])

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

subList :: ([Int], [Int]) -> Bool
subList (a, b)
  | length a > length b = intersect a b == b
  | otherwise = intersect a b == a

overlaps :: ([Int], [Int]) -> Bool
overlaps (a, b) = length (intersect a b) > 0

tuplify :: [a] -> (a, a)
tuplify (a:b:_) = (a, b)

rangify :: (Int, Int) -> [Int]
rangify (a, b) = [a .. b]

parseInt :: String -> Int
parseInt = read

-- PART ONE
--main = do
--  contents <- readFile "input_4.txt"
--  print
--    $ length 
--    $ filter id 
--    $ map (subList .
--            (mapTuple 
--                (rangify . tuplify . 
--                    (map parseInt . mySplit '-')))) 
--                $ map (tuplify . mySplit ',')
--    $ lines contents
-- PART TWO
main = do
  contents <- readFile "input_4.txt"
  print $
    length $
    filter id $
    map
      (overlaps . (mapTuple (rangify . tuplify . (map parseInt . mySplit '-')))) $
    map (tuplify . mySplit ',') $ lines contents
