-- DAY 9
import Control.Monad
import Data.List
import System.IO

moveH :: (Int, Int) -> String -> (Int, Int)
moveH (x, y) direction =
    case direction of
        "U" -> (x, y + 1)
        "D" -> (x, y - 1)
        "L" -> (x - 1, y)
        "R" -> (x + 1, y)

moveT :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveT (xh, yh) (xt, yt)
    | (dx, dy) == (1, 1) = (xt, yt)
    | (dx, dy) == (2, 2) = (xt', yt')
    | xh == xt =
        case () of
            ()
                | dy <= 1 -> (xt, yt)
                | otherwise -> (xt, yt')
    | yh == yt =
        case () of
            ()
                | dx <= 1 -> (xt, yt)
                | otherwise -> (xt', yt)
    | dx == 2 = (xt', yh)
    | otherwise = (xh, yt')
  where
    dx = abs (xh - xt)
    dy = abs (yh - yt)
    xt' =
        if xh > xt
            then xt + 1
            else xt - 1
    yt' =
        if yh > yt
            then yt + 1
            else yt - 1

moves :: [(Int, Int)] -> [(String, Int)] -> [(Int, Int)]
moves _ [] = []
moves rope ((direction, times):cmds)
    | times == 0 = moves rope cmds
    | otherwise = ((last rope') : (moves rope' ((direction, times - 1) : cmds)))
  where
    (h:tail) = rope
    rope' = foldl f [moveH h direction] tail
    f acc k = acc ++ [moveT (last acc) k]

toCmd :: String -> (String, Int)
toCmd cmd = (direction, read times)
  where
    [direction, times] = words cmd

main = do
    contents <- readFile "data/input_9.txt"
    let commands = map toCmd $ lines contents
    let result = length $ nub $ moves [(0, 0), (0, 0)] commands
    print result
    let result = length $ nub $ moves (take 10 $ repeat (0, 0)) commands
    print result
