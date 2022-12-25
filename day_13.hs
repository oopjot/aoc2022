-- DAY 13
import Data.Char
import Data.List

data Packet
    = N Int
    | L [Packet]
    deriving (Show, Eq, Read)

instance Ord Packet where
    compare (N a) (N b) = compare a b
    compare a@(L _) b@(N _) = compare a (L [b])
    compare a@(N _) b@(L _) = compare (L [a]) b
    compare (L a) (L b) = compare a b

packet :: String -> Packet
packet = read . annotate
  where
    annotate :: String -> String
    annotate [] = ""
    annotate (c:rest)
        | c == '[' = "L " ++ [c] ++ annotate rest
        | isDigit c =
            "N " ++
            (c : takeWhile isDigit rest) ++ annotate (dropWhile isDigit rest)
        | otherwise = c : annotate rest

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy x xs = splitBy' xs []
  where
    splitBy' [] acc = [acc]
    splitBy' (x':xs') acc
        | x == x' = acc : splitBy' xs' []
        | otherwise = splitBy' xs' (acc ++ [x'])

main = do
    contents <- readFile "data/input_13.txt"
    -- Part ONE
    let pairs = [(packet a, packet b) | [a, b] <- splitBy "" $ lines contents]
    print $ sum $ map fst $ filter (uncurry (<) . snd) $ zip [1 ..] pairs
    -- Part TWO
    let signal =
            zip [1 ..] $
            sort $
            map packet $ filter (/= "") $ lines contents ++ ["[[2]]", "[[6]]"]
    let d1 = head $ [i | (i, a) <- signal, a == packet "[[2]]"]
    let d2 = head $ [i | (i, a) <- signal, a == packet "[[6]]"]
    print $ d1 * d2
