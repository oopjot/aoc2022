-- DAY 11
import Control.Monad
import Data.List
import System.IO

data Monkey =
    Monkey
        { items :: [Int]
        , op :: Int -> Int
        , to :: Int -> Int
        , ins :: Int
        }

instance Show Monkey where
    show (Monkey items _ _ ins) = show (ins, items)

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = left ++ (x : (tail right))
  where
    (left, right) = splitAt n xs

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy x xs = splitBy' xs []
  where
    splitBy' [] acc = [acc]
    splitBy' (x':xs') acc
        | x == x' = (acc : (splitBy' xs' []))
        | otherwise = splitBy' xs' (acc ++ [x'])

parseItems :: String -> [Int]
parseItems input = map read (splitBy ',' s)
  where
    [_, s] = splitBy ':' input

parseWorry :: String -> Double -> Int -> Int
parseWorry input divisor worry =
    floor . (/ divisor) . fromIntegral $
    case (left, right) of
        ("old", "old") -> f worry worry
        ("old", a) -> f worry (read a)
        (a, "old") -> f worry (read a)
        _ -> f (read left) (read right)
  where
    [_, s] = splitBy '=' input
    [left, op, right] = words s
    f =
        if op == "*"
            then (*)
            else (+)

parseWorry' :: Int -> String -> Double -> Int -> Int
parseWorry' by input divisor worry = (mod $ parseWorry input divisor worry) by

parseTo :: String -> String -> String -> Int -> Int
parseTo test true false worry
    | worry `mod` by == 0 = read [last true]
    | otherwise = read [last false]
  where
    by = read $ last $ words test :: Int

monkey :: Double -> [String] -> Monkey
monkey divisor [_, i, o, t, true, false] =
    Monkey (parseItems i) (parseWorry o divisor) (parseTo t true false) 0

monkey' :: Int -> Double -> [String] -> Monkey
monkey' by divisor [_, i, o, t, true, false] =
    Monkey (parseItems i) (parseWorry' by o divisor) (parseTo t true false) 0

throw :: (Int, Monkey) -> [Monkey] -> [Monkey]
throw (i, thrower) monkeys =
    case (items thrower) of
        (item:rest) -> throw (i, thrower') monkeys'
            where worry = op thrower $ item
                  ir = to thrower $ worry
                  thrower' = thrower {items = rest, ins = (ins thrower) + 1}
                  receiver = monkeys !! ir
                  receiver' = receiver {items = (items receiver) ++ [worry]}
                  monkeys' = insertAt ir receiver' $ insertAt i thrower' monkeys
        _ -> monkeys

turn :: [Monkey] -> Int -> [Monkey]
turn monkeys 0 = monkeys
turn monkeys round = turn monkeys' (round - 1)
  where
    n = (length monkeys) - 1
    monkeys' = foldl f monkeys $ [0 .. n]
    f ms from = throw (from, ms !! from) ms

main = do
    contents <- readFile "data/input_11.txt"
    let input = splitBy "" $ lines contents
    let monkeys = map (monkey 3) input
    -- Part ONE
    let result = product . take 2 . reverse . sort . map ins $ turn monkeys 20
    print result
    -- Part TWO - modular arithmetic goes brrrrrr
    -- done with explanation from https://www.reddit.com/user/heyitsmattwade/
    -- in his/her JS example
    let by = foldl (*) 1 $ map (read . last . words) $ map (!! 3) input
    let monkeys = map (monkey' by 1) input
    let result =
            product $ take 2 . reverse . sort . map ins $ turn monkeys 10000
    print result
