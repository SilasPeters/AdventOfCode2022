import Data.Char

answer :: Int -> IO ()
answer question = do
  input <- readFile "Day2.input"
  let parsedInput = parseInput input
  case question of
    1 -> print $ sum $ map scoreOfMove parsedInput
    2 -> print $ sum $ map (scoreOfMove . determineMove) parsedInput

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
    scoreOfShape c = ord c - ord 'W'

determineMove :: (Char, Char) -- Other, required outcome
              -> (Char, Char) -- Other, your move
determineMove (other, outcome) =
  let otherOffset = ord other - ord 'A'
      outcomeOffset = ord outcome - ord 'X'
   in (other, ['Z', 'X', 'Y'] !! ((otherOffset + outcomeOffset) `rem` 3))
