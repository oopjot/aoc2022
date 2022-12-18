-- DAY 8
import Control.Monad
import Data.Char
import Data.List
import System.IO

imap :: (a -> Int -> b) -> [a] -> [b]
imap f l = zipWith f l [0 ..]

visibleH :: Int -> String -> Bool
visibleH i row =
    let nums = map digitToInt row
        tree = nums !! i
        (left, _:right) = splitAt i nums
        leftVisible = foldl (\acc t -> acc && (t < tree)) True left
        rightVisible = foldr (\t acc -> acc && (t < tree)) True right
     in leftVisible || rightVisible

visible :: Int -> Int -> [String] -> Bool
visible row col grid = visibleH row (grid !! col) || visibleH col (grid' !! row)
  where
    grid' = transpose grid

scoreSide :: Int -> [Int] -> Int
scoreSide _ [] = 0
scoreSide tree (t:ts)
    | tree > t = 1 + (scoreSide tree ts)
    | otherwise = 1

scoreH :: Int -> String -> Int
scoreH i row =
    let nums = map digitToInt row
        tree = nums !! i
        (left, _:right) = splitAt i nums
        scoreLeft = scoreSide tree (reverse left)
        scoreRight = scoreSide tree right
     in scoreLeft * scoreRight

score :: Int -> Int -> [String] -> Int
score row col grid = scoreH row (grid !! col) * (scoreH col (grid' !! row))
  where
    grid' = transpose grid

main = do
    contents <- readFile "data/input_8.txt"
    let grid = lines contents
    -- Part ONE
    let visibilityGrid =
            [ imap (\t row -> visible row col grid) r
            | (col, r) <- zip [0 ..] grid
            ]
    let result = length $ filter id $ [v | r <- visibilityGrid, v <- r]
    print result
    -- Part TWO
    let scoreGrid =
            [ imap (\t row -> score row col grid) r
            | (col, r) <- zip [0 ..] grid
            ]
    let result = head $ reverse $ sort $ [v | r <- scoreGrid, v <- r]
    print result
