-- DAY 7
import Control.Monad
import Data.List
import System.IO

data Entity
    = Dir String [Entity]
    | File String Int
    deriving (Show)

filesystem :: [String] -> Entity -> ([String], Entity)
filesystem [] fs = ([], fs)
filesystem (cmd:cmds) (Dir name es)
    | cmd == "$ cd .." = (cmds, (Dir name es))
    | (take 3 cmd) == "dir" = filesystem cmds (Dir name es)
    | (take 4 cmd) == "$ ls" = filesystem cmds (Dir name es)
    | (take 4 cmd) == "$ cd" = filesystem cmds' (Dir name (fs' : es))
    | otherwise = filesystem cmds (Dir name (file : es))
  where
    (fsize:fname:_) = words cmd
    file = File fname (read fsize :: Int)
    (cmds', fs') = filesystem cmds (Dir (drop 5 cmd) [])

size :: Entity -> Int
size (File _ size) = size
size (Dir _ es) = foldl (\sum e -> sum + (size e)) 0 es

extractDirSizes :: Entity -> [Int]
extractDirSizes (File _ _) = []
extractDirSizes dir =
    [size dir] ++ (foldl (\acc e -> acc ++ (extractDirSizes e)) [] es)
  where
    (Dir name es) = dir

main = do
    contents <- readFile "data/input_7.txt"
    let input = drop 2 $ lines contents
    let fs = snd $ filesystem input (Dir "/" [])
    let dirSizes = extractDirSizes fs
    -- Part One
    let result = sum $ filter (<= 100000) dirSizes
    print result
    -- Part Two
    let neededSpace = 30000000 - (70000000 - (size fs))
    let result = head $ (sort . filter (>= neededSpace)) dirSizes
    print result
