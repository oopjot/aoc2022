-- DAY 9
import Control.Monad
import Data.List.Split
import System.IO

signal :: Int -> Int -> Int -> [String] -> Int -> Int
signal cycle register _ [] _ = register
signal cycle register 0 (cmd:cmds) end
    | cycle == end = register * cycle
    | cmd == "noop" = signal cycle register 1 cmds end
    | otherwise = signal cycle (register + x) 2 cmds end
  where
    [_, s] = words cmd
    x = read s :: Int
signal cycle register clock cmds end
    | cycle == end = register * cycle
    | otherwise = signal (cycle + 1) register (clock - 1) cmds end

crt :: Int -> Int -> [String] -> String -> String
crt _ _ [] screen = screen
crt register 0 (cmd:cmds) screen
    | cmd == "noop" = crt register 1 cmds screen
    | otherwise = crt (register + x) 2 cmds screen
  where
    [_, s] = words cmd
    x = read s :: Int
crt register clock cmds screen = crt register (clock - 1) cmds (screen ++ pixel)
  where
    cycle = length screen
    position = cycle `mod` 40
    pixel
        | register == position = "#"
        | register == (position + 1) = "#"
        | register == (position + 2) = "#"
        | otherwise = "."

main = do
    contents <- readFile "data/input_9.txt"
    let cmds = lines contents
    -- Part ONE
    let result = sum $ map (signal (-1) 1 3 cmds) [20, 60, 100, 140, 180, 220]
    print $ result
    -- Part TWO
    let result = chunksOf 40 $ crt 1 1 cmds ""
    mapM_ print result
