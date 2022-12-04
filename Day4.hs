
import Data.List ( elemIndex )
import Data.Maybe ( fromJust, mapMaybe )

data Range = Range { lowerIncl :: Int, upperIncl :: Int } deriving (Show)

answer :: Int -> IO ()
answer question = do
  inputRaw <- readFile "Day4Input.txt"
  let parsedInput = map parseRange $ lines inputRaw
  case question of
    1 -> print $ length $ mapMaybe strictlyOverlap parsedInput
    2 -> print $ length $ mapMaybe partiallyOverlap parsedInput

parseRange :: String -> (Range, Range)
parseRange range = let (harry, delton) = splitOn ',' range
                       (p, q) = splitOn '-' harry
                       (x, y) = splitOn '-' delton
                    in (Range (read p) (read q),
                        Range (read x) (read y))
  where
    splitOn :: Char -> String -> (String, String)
    splitOn c x = let i = fromJust $ elemIndex c x in
      (take i x, drop (i + 1) x)

strictlyOverlap :: (Range, Range) -> Maybe Range
strictlyOverlap (a, b)
  | lowerIncl a <= lowerIncl b && upperIncl a >= upperIncl b = Just a
  | lowerIncl b <= lowerIncl a && upperIncl b >= upperIncl a = Just b
  | otherwise                                                = Nothing

partiallyOverlap :: (Range, Range) -> Maybe (Range, Range)
partiallyOverlap (a, b)
  | upperIncl a >= lowerIncl b
      && lowerIncl a <= upperIncl b = Just (a, b)
  | otherwise                       = Nothing