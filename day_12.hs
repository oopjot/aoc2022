-- DAY 12
import Control.Monad
import Data.Char
import System.IO

location :: [String] -> Char -> (Int, Int)
location maze c =
    head
        [ (x, y)
        | (y, row) <- zip [0 ..] maze
        , (x, p) <- zip [0 ..] (maze !! y)
        , p == c
        ]

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) 'U' = (x, y - 1)
move (x, y) 'D' = (x, y + 1)
move (x, y) 'L' = (x - 1, y)
move (x, y) 'R' = (x + 1, y)
move pos _ = pos

height :: [String] -> (Int, Int) -> Int
height maze (x, y)
    | c == 'S' = ord 'a'
    | c == 'E' = ord 'z'
    | otherwise = ord c
  where
    c = maze !! y !! x

validMoves :: [String] -> (Int, Int) -> String
validMoves maze pos = filter f possibleMoves
  where
    xMax = (length $ (maze !! (snd pos))) - 1
    yMax = (length maze) - 1
    diff m = (height maze $ move pos m) - (height maze pos)
    f m = diff m == 0 || diff m == 1
    possibleMoves =
        case pos of
            (0, 0) -> "DR"
            (0, y)
                | y == yMax -> "UR"
                | otherwise -> "UDR"
            (x, 0)
                | x == xMax -> "DL"
                | otherwise -> "DLR"
            (x, y)
                | x == xMax && y == yMax -> "UL"
                | x == xMax -> "UDL"
                | y == yMax -> "ULR"
                | otherwise -> "UDLR"

solve ::
       [String]
    -> (Int, Int)
    -> [(Int, Int)]
    -> [(Int, Int)]
    -> [[(Int, Int)]]
    -> [(Int, Int)]
solve maze end [] _ (path:paths) = solve maze end positions path paths
  where
    p = head path
    moves = validMoves maze p
    positions = filter f $ map (move p) moves
    f p = not . (elem p) $ path
solve maze end (p:positions) path paths
    | p == end = path'
    | otherwise = solve maze end positions path (paths ++ [path'])
  where
    path' = p : path

main = do
    contents <- readFile "data/input_12_tmp.txt"
    let maze = lines contents
    -- Part ONE
    let start = location maze 'S'
    let end = location maze 'E'
    let result = solve maze end [start] [] []
    print result
