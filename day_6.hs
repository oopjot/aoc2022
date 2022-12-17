-- DAY 6
import Control.Monad
import System.IO

splitAtChar :: Char -> String -> (String, String)
splitAtChar c (x:xs) = splitAtChar' c (x : xs) ""
  where
    splitAtChar' :: Char -> String -> String -> (String, String)
    splitAtChar' _ [] acc = (acc, "")
    splitAtChar' c (x:xs) acc
      | c == x = (acc, xs)
      | otherwise = splitAtChar' c xs (acc ++ [x])

detect :: Int -> String -> Int
detect n stream = detect' stream "" 0
  where
    detect' :: String -> String -> Int -> Int
    detect' (signal:stream) acc res
      | length acc == n = res
      | elem signal acc =
        let (_, xs) = splitAtChar signal acc
         in detect' stream (xs ++ [signal]) (res + 1)
      | otherwise = detect' stream (acc ++ [signal]) (res + 1)

main = do
  contents <- readFile "input_6.txt"
  let input = head $ lines contents
    -- Part ONE
  let result = detect 4 input
    -- Part TWO
  let result = detect 14 input
  print result
