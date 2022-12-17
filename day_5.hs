-- DAY 5
import Control.Monad
import Data.Char (ord)
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

every :: Int -> [a] -> [a]
every _ [] = []
every n xs = head xs : every n (drop n xs)

flipStack :: [String] -> [String]
flipStack a = reverse $ map (filter (\x -> x /= ' ')) $ reverse . transpose $ a

parseInstruction :: String -> (Int, Int, Int)
parseInstruction ins =
    let (_:n:_:from:_:to:_) = words ins
     in (read n :: Int, read from :: Int, read to :: Int)

replaceAt :: a -> Int -> [a] -> [a]
replaceAt e n xs =
    let (x, _:ys) = splitAt n xs
     in x ++ e : ys

moveStack :: [String] -> (Int, Int, Int) -> [String]
moveStack stack (n, from, to) =
    let newSource = drop n (stack !! (from - 1))
        toMove = take n (stack !! (from - 1))
        newDest = toMove ++ (stack !! (to - 1))
     in replaceAt newDest (to - 1) $ replaceAt newSource (from - 1) stack

moveByOne :: [String] -> (Int, Int, Int) -> [String]
moveByOne stack (n, from, to) =
    let newSource = drop n (stack !! (from - 1))
        toMove = take n (stack !! (from - 1))
        newDest = (reverse toMove) ++ (stack !! (to - 1))
     in replaceAt newDest (to - 1) $ replaceAt newSource (from - 1) stack

main = do
    contents <- readFile "data/input_5.txt"
    let stack = flipStack $ map (every 4 . drop 1) $ take 8 $ lines contents
    let instructions = map parseInstruction $ drop 10 $ lines contents
  --Part ONE
    let newStack = foldl moveByOne stack instructions
  -- Part TWO 
    let newStack = foldl moveStack stack instructions
    let result = map head newStack
    print newStack
    print result
