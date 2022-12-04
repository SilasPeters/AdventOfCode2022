import Data.List
import Data.Char

answer :: Int -> IO ()
answer question = do
  inputRaw <- readFile "Day3Input.txt"
  let parsedInput = lines inputRaw
  let duplicatesPerRucksack = map findDuplicateInRucksack parsedInput
  case question of
    1 -> print $ sum $ map priority duplicatesPerRucksack
    2 -> print $ "Not implemented yet"

findDuplicateInRucksack :: String -> Char
findDuplicateInRucksack contents = head $ uncurry intersect $ splitAt (length contents `div` 2) contents

priority :: Char -> Int
priority c = let asciiValue = ord c in
  if asciiValue < ord 'a'
    then asciiValue - ord '@' + 26
    else asciiValue - ord '`'