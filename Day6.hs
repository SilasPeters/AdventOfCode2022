{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

answer :: Int -> IO ()
answer question = do
  input <- readFile "Day6.input"
  case question of
    1 -> print $ findDistinctGroupOfLength 4 input
    2 -> print $ findDistinctGroupOfLength 14 input

findDistinctGroupOfLength :: Eq a => Int -> [a] -> Int
findDistinctGroupOfLength length stream = search stream 0
  where
    search :: Eq a => [a] -> Int -> Int
    search stream count
      | not $ duplicate $ take length stream = count + length
      | otherwise                            = search (drop 1 stream) (count + 1)
    duplicate :: Eq a => [a] -> Bool
    duplicate []     = False
    duplicate (x:xs) | x `elem` xs = True
                     | otherwise   = duplicate xs