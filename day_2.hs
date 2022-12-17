-- DAY 2
import Control.Monad
import System.IO

data Move
    = Rock
    | Paper
    | Scissors
    deriving (Show)

data Result
    = Win
    | Draw
    | Loss
    deriving (Show)

moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

resultScore :: Result -> Int
resultScore Loss = 0
resultScore Draw = 3
resultScore Win = 6

parseMove :: Char -> Move
parseMove 'X' = Rock
parseMove 'A' = Rock
parseMove 'Y' = Paper
parseMove 'B' = Paper
parseMove 'Z' = Scissors
parseMove 'C' = Scissors

parseGame :: String -> (Move, Move)
parseGame game = (parseMove $ head game, parseMove $ last game)

play :: Move -> Move -> Result
play Rock Paper = Win
play Rock Scissors = Loss
play Paper Scissors = Win
play Paper Rock = Loss
play Scissors Rock = Win
play Scissors Paper = Loss
play _ _ = Draw

score :: (Move, Move) -> Int
score (a, b) = moveScore b + resultScore (play a b)

parseResult :: Char -> Result
parseResult 'X' = Loss
parseResult 'Y' = Draw
parseResult 'Z' = Win

findMove :: Move -> Result -> Move
findMove Rock Win = Paper
findMove Rock Loss = Scissors
findMove Paper Win = Scissors
findMove Paper Loss = Rock
findMove Scissors Win = Rock
findMove Scissors Loss = Paper
findMove move Draw = move

parseGameTwo :: String -> (Move, Result)
parseGameTwo game = (parseMove $ head game, parseResult $ last game)

scoreTwo :: (Move, Result) -> Int
scoreTwo (move, result) =
    resultScore result + (moveScore $ findMove move result)

main = do
    contents <- readFile "data/input_2.txt"
    let input = lines contents
  -- Part ONE
    let result = foldl (+) 0 . map (score . parseGame) $ input
  -- Part TWO
    let result = foldl (+) 0 . map (scoreTwo . parseGameTwo) $ input
    print result
