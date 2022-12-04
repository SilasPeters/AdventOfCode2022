answer :: Int -> IO ()
answer question = do
  input <- readFile "Day2Input.txt"
  let scoresPerMove = map scoreOfMove $ parseInput input
  case question of
    1 -> print $ sum scoresPerMove

parseInput :: String -> [(Char, Char)]
parseInput = map (\line -> (line !! 0, line !! 2)) . lines

scoreOfMove :: (Char, Char) -> Int
scoreOfMove (other, you) = (scoreOfWinning other you) + (scoreOfShape you)
  where
    scoreOfWinning :: Char -> Char -> Int
                    -- Other -> You -> Score
    scoreOfWinning 'A' 'X' = 3
    scoreOfWinning 'A' 'Y' = 6
    scoreOfWinning 'A' 'Z' = 0
    scoreOfWinning 'B' 'X' = 0
    scoreOfWinning 'B' 'Y' = 3
    scoreOfWinning 'B' 'Z' = 6
    scoreOfWinning 'C' 'X' = 6
    scoreOfWinning 'C' 'Y' = 0
    scoreOfWinning 'C' 'Z' = 3
    scoreOfWinning _   _   = error "Illegal move operator"

    scoreOfShape :: Char -> Int
    scoreOfShape 'X' = 1
    scoreOfShape 'Y' = 2
    scoreOfShape 'Z' = 3
