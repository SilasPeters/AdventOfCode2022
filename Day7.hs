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
      accRef <- newIORef 0
      mapOverFolderSizesWhenSizeAtMost 100000 (\x -> modifyIORef accRef (+ x)) root
      print =<< readIORef accRef
    2 -> do
      -- Determine total disk space used
      totalSizeRef <- newIORef 0
      mapOverFolderSizesWhenSizeAtMost 70000000 (writeIORef totalSizeRef) root

      -- Determine space needed
      totalSize <- readIORef totalSizeRef -- size of root folder
      let minimumSize = 30000000 - (70000000 - totalSize)
      print minimumSize

      -- Find the smallest folder larget than the space needed
      sizeOfDirectoryToDelete <- newIORef totalSize -- root folder by default
      mapOverFolderSizesWhenSizeAtMost 70000000
        (\x -> modifyIORef sizeOfDirectoryToDelete
          (\current -> if x >= minimumSize then min current x else current)
        ) root
      print =<< readIORef sizeOfDirectoryToDelete

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

mapOverFolderSizesWhenSizeAtMost :: Int -> (Int -> IO ()) -> Path -> IO Int
mapOverFolderSizesWhenSizeAtMost maxSize f = getSize
  where
    getSize :: Path -> IO Int
    getSize path = do
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then do
          directoryContents <- listDirectory path
          size <- sum <$> mapM (getSize . expandedWith path) directoryContents
          if size <= maxSize
            then f size >> return size
            else return size
        else read <$> readFile path