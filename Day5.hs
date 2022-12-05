{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List ( transpose )
import Data.Maybe ( fromJust, mapMaybe )
import Data.Char ( isSpace )

newtype Stack a = Stack [a] deriving (Show)
data    Delta   = Delta { n :: Int, from :: Int, to :: Int }
                    deriving (Show)

instance Functor Stack where
   fmap f (Stack s) = Stack $ map f s

putStack :: Stack a -> [a] -> Stack a
putStack (Stack s) value = Stack $ s ++ value

popNFromStack :: Int -> Stack a -> ([a], Stack a)
popNFromStack n (Stack s) = let s' = reverse s in
  (take n s', Stack $ reverse $ drop n s')

answer :: Int -> IO ()
answer question = do
  inputRaw <- lines <$> readFile "Day5.input"
  let stacks = parseStacks $ take 8  inputRaw
  let deltas = parseDeltas $ drop 10 inputRaw
  case question of
    1 -> print $ concatMap (fst . popNFromStack 1) $ performMoves stacks deltas
    2 -> print $ "Not implemented yet"

parseStacks :: [String] -> [Stack Char] -- Yes, this is wonky. No, I won't comment it
parseStacks = map ((Stack . filter (not . isSpace)) . reverse) . every 4 . (["",""] ++) . transpose

parseDeltas :: [String] -> [Delta]
parseDeltas = map $ makeDelta . every 2 . words
  where
    makeDelta :: [String] -> Delta
    makeDelta [a,b,c] = Delta (read a) (read b - 1) (read c - 1)

performMoves :: [Stack Char] -> [Delta] -> [Stack Char]
performMoves = foldl f
  where
    f :: [Stack Char] -> Delta -> [Stack Char]
    f stacks (Delta nOfCrates fromStack toStack) =
      let (crates, shrinkedFromStack) = popNFromStack nOfCrates (stacks !! fromStack)
          stacks'         = replaceAt fromStack stacks shrinkedFromStack
          expandedToStack = putStack (stacks' !! toStack) crates
          stacks''        = replaceAt toStack stacks' expandedToStack
       in stacks''
    replaceAt :: Int -> [a] -> a -> [a]
    replaceAt i xs v = take i xs ++ [v] ++ drop (i + 1) xs

-- Thanks to Nefrubyr, https://stackoverflow.com/a/2028218/15746803
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              y : ys -> y : every n ys
              []     -> []