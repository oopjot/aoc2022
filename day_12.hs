-- DAY 12
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import System.IO

type Position = (Int, Int)

grid :: [String] -> (M.Map Position Int, Position, Position)
grid input = foldr f (M.empty, (-1, -1), (-1, -1)) positions
  where
    positions =
        [((x, y), c) | (y, row) <- zip [0 ..] input, (x, c) <- zip [0 ..] row]
    f (pos, c) (grid, start, end)
        | c == 'S' = (grid', pos, end)
        | c == 'E' = (grid', start, pos)
        | otherwise = (grid', start, end)
      where
        grid' = M.insert pos (score c) grid
    score c
        | c == 'E' = ord 'z'
        | c == 'S' = ord 'a'
        | otherwise = ord c

neighbours :: M.Map Position Int -> Position -> [Position]
neighbours grid pos@(x, y) =
    filter f [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
  where
    f pos' =
        case (M.lookup pos grid, M.lookup pos' grid) of
            (Just n, Just n') -> n' - n <= 1
            _ -> False

solve :: M.Map Position Int -> Position -> Position -> Maybe Int
solve _ (-1, -1) _ = Nothing
solve _ _ (-1, -1) = Nothing
solve grid end start = search [] [(start, 0)]
  where
    search :: [Position] -> [(Position, Int)] -> Maybe Int
    search _ [] = Nothing
    search visited ((next, d):toVisit)
        | next == end = Just d
        | otherwise = search visited' more
      where
        found = filter (not . (`elem` visited)) $ neighbours grid next
        visited' = visited ++ found
        more = toVisit ++ (map (\p -> (p, d + 1)) found)

main = do
    contents <- readFile "data/input_12.txt"
    let maze = grid $ lines contents
    let (grid, start, end) = maze
    -- Part ONE
    print $ solve grid end start
    -- Part TWO
    let lowestPoints =
            [ (x, y)
            | (y, row) <- zip [0 ..] $ lines contents
            , (x, c) <- zip [0 ..] row
            , c == 'a' || c == 'S'
            ]
    print $
        head $ sort $ filter (/= Nothing) $ map (solve grid end) lowestPoints
