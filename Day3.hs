import Data.List
import Data.Char

answer :: Int -> IO ()
answer question = do
  inputRaw <- readFile "Day3.input"
  let parsedInput = lines inputRaw
  let duplicatesPerRucksack = map findDuplicateInRucksack parsedInput
  case question of
    1 -> print $ sum $ map priority duplicatesPerRucksack
    2 -> print $ sum $ map priority $ findBadges parsedInput

findDuplicateInRucksack :: String -> Char
findDuplicateInRucksack contents = head $ uncurry intersect $ splitAt (length contents `div` 2) contents

findBadges :: [String] -> [Char]
findBadges []        = []
findBadges rucksacks = findBadge (take 3 rucksacks) : findBadges (drop 3 rucksacks)
  where
    findBadge :: [String] -> Char
    findBadge [a,b,c] = head $ intersect a $ intersect b c

priority :: Char -> Int
priority c = let asciiValue = ord c in
  if asciiValue < ord 'a'
    then asciiValue - ord '@' + 26
    else asciiValue - ord '`'