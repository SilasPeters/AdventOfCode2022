import Data.List

answer :: Int -> IO ()
answer question = do
  inputRaw <- readFile "Day1.input"
  let groupedInput = groupPerElf $ lines inputRaw
  let parsedInput = map (map read) groupedInput :: [[Int]]
  let totalPerElf = map sum parsedInput
  case question of
    1 -> print $ maximum totalPerElf
    2 -> print $ sum $ take 3 $ sortBy (\a b -> if a < b then GT else if a == b then EQ else LT) totalPerElf

groupPerElf :: [String] -> [[String]]
groupPerElf = groupPerElf' [] []
  where
    groupPerElf' :: [[String]] -> [String] -> [String] -> [[String]]
    groupPerElf' result acc []     = result ++ [acc]
    groupPerElf' result acc (x:ys) | null x    = groupPerElf' (result ++ [acc]) [] ys
                                   | otherwise = groupPerElf' result (acc ++ [x]) ys
