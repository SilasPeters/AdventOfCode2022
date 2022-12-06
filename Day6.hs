
answer :: Int -> IO ()
answer question = do
  input <- readFile "Day6.input"
  case question of
    1 -> print $ startOfFile input
    2 -> print $ "Not implemented yet"

startOfFile :: String -> Int
startOfFile = search 0
  where
    search :: Int -> String -> Int
    search count stream | not $ duplicate $ take 4 stream = count + 4
                        | otherwise                       = search (count + 1) (drop 1 stream)
    duplicate :: String -> Bool
    duplicate []     = False
    duplicate (x:xs) | x `elem` xs = True
                     | otherwise   = duplicate xs