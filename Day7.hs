{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad
import Data.IORef

type Path = String
--data File = File String | Directory String

root :: Path
root = "C:/Day7.Simulation"

answer :: Int -> IO ()
answer question = do
  input <- lines <$> readFile "Day7.input"
  createDirectory root
  executeCommands input
  case question of
    1 -> do
      acc <- newIORef 0
      sumSizeOfFilesWithSizeOfAtMost 100000 acc root
      print =<< readIORef acc
    2 -> print $ "Question not published yet"

executeCommands :: [String] -> IO Path
executeCommands = foldM executeCommand root

executeCommand :: Path -> String -> IO Path
executeCommand path "$ cd .."                 = return $ folderUp path -- cd ..
executeCommand path "$ cd /"                  = return root            -- cd /
executeCommand path ('$':' ':'c':'d':' ':dir) = cd path dir            -- cd x
executeCommand path "$ ls"                    = return path            -- ls
executeCommand path ('d':'i':'r':' ':_)       = return path            -- dir x
executeCommand path cmd = let (size, name)    = splitAt (fromJust (elemIndex ' ' cmd) + 1) cmd in
                                                  mkFile path name size >> return path
                                                                       -- 69420 name.txt

folderUp :: Path -> Path
folderUp path = let pathReverse = reverse path
                    slashIndex  = fromJust $ elemIndex '/' pathReverse
                    result      = reverse $ drop (slashIndex + 1) pathReverse
                 in if length path <= length root then root else result
expandedWith :: Path -> String -> Path
expandedWith path expansion = path ++ "/" ++ expansion

cd :: Path -> String -> IO Path
cd path name = do
  let dirPath = path `expandedWith` name
  print $ "Making dir " ++ dirPath
  createDirectory dirPath
  return dirPath

mkFile :: Path -> String -> String -> IO ()
mkFile path name = writeFile (path `expandedWith` name)

mapOverFolderSizes :: Int -> IORef Int -> Path -> (Int -> IO ()) -> IO Int
sumSizeOfFilesWithSizeOfAtMost maxSize f = getSize
  where
    getSize :: IORef Int -> Path -> IO Int
    getSize acc path = do
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then do
          directoryContents <- listDirectory path
          size <- sum <$> mapM (getSize acc . expandedWith path) directoryContents
          if size <= maxSize
            then do
              f size
              return size
            else return size
        else read <$> readFile path


-- oldAcc <- readIORef acc
-- writeIORef acc (oldAcc + size)