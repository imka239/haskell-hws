{-# LANGUAGE FlexibleContexts #-}

module FileManager (
  MyException (..), FSInterface(..), Command
) where

import Control.Exception (Exception (..), throw)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (lift, liftIO)
import Data.IORef (IORef, readIORef, writeIORef)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findFile, getFileSize, getModificationTime, getPermissions, listDirectory, removePathForcibly)
import System.FilePath (isRelative, pathSeparator, takeExtension)
import System.IO.Error (ioeGetErrorString, tryIOError)

-- | instance for Exception to erase problems during work
data MyException
  = -- | If you don't have access to create directory
    CreatingDirectoryError String
  | -- | If you don't have access to create file
    CreatingFileError String
  | -- | If you don't have access due to there are no directory
    NoDirectoryError String
  | -- | If you don't have access to list of directory and files
    ListDirectoryError String
  | -- | If you don't have access to remove directory
    RemovingDirectoryError String
  | -- | If you have system troubles
    SystemError String
  | -- | If you don't have access due to there are no file
    NoFileError String
  deriving (Show, Eq)

instance Exception MyException

-- | helper function made to fold list of directories to get tuple of size and amount of files in searched directory
getSizeForListOfDirs :: (Integer, Integer) -> [FilePath] -> IO (Integer, Integer)
getSizeForListOfDirs ans [] = do
  return ans
getSizeForListOfDirs (x, y) (resultPath : els) = do
  exists1 <- liftIO $ doesDirectoryExist resultPath
  if (exists1)
    then do
      ab <- getSize resultPath
      getSizeForListOfDirs (x + (fst ab), y + (snd ab)) els
    else do
      exists2 <- liftIO $ doesFileExist resultPath
      if (exists2)
        then do
          size <- liftIO $ getFileSize resultPath
          getSizeForListOfDirs (x + size, y + 1) els
        else do
          getSizeForListOfDirs (x, y) els

-- | main helper function to count size and amount of files returns tuple
getSize :: FilePath -> IO (Integer, Integer)
getSize resultPath = do
  list <- liftIO (listDirectory resultPath)
  let lst = map (\x -> (resultPath ++ [pathSeparator] ++ x)) list
  getSizeForListOfDirs (0, 0) lst

-- | helper function for `findfile` which search file in list of `FilePath`
letsFindInHere :: [FilePath] -> String -> IO (Maybe FilePath)
letsFindInHere [] name = return Nothing
letsFindInHere (resultPath : els) name = do
  exists1 <- liftIO $ doesDirectoryExist resultPath
  if (exists1)
    then do
      res <- liftIO $ findfile resultPath name
      case res of
        Just fil -> return (Just fil)
        Nothing -> do
          res <- letsFindInHere els name
          return (res)
    else do
      res <- letsFindInHere els name
      return (res)

-- | Main helper to find file. returns Nothing if there are no file and Just FilePath for file otherwise.
findfile :: FilePath -> String -> IO (Maybe FilePath)
findfile resultPath name = do
  list <- liftIO (listDirectory resultPath)
  let lst = map (\x -> (resultPath ++ [pathSeparator] ++ x)) list
  exists2 <- liftIO $ findFile [resultPath] name
  case exists2 of
    Just filePath -> return (Just filePath)
    Nothing -> do
      res <- letsFindInHere lst name
      return (res)

-- | Helper function to develop absolute path for given one.
-- This function uses `isRelative` to check path for absoluteness and `canonicalizePath` to get path without dots.
-- Easy implementation for this functions are in test directory
makePath path = do
  cur <- ask
  curPath <- liftIO $ readIORef cur
  let newPath
        | isRelative path = (curPath ++ [pathSeparator] ++ path)
        | otherwise = path
  resultPath <- liftIO $ tryIOError $ canonicalizePath newPath
  case resultPath of
    Left ex -> throw $ SystemError $ ioeGetErrorString ex
    Right res -> return res

-- | Command is the main idea to work with real Directories. The first Idea was type
-- Command m a = ExceptT FSError (StateT FilePath m) a, but a reject it due to problems of ExceptT and IO
type Command a = ReaderT (IORef FilePath) a

-- | interface that will be deployed.
-- for help write help in console to get List of commands to use.
class FSInterface m where
  cd :: FilePath -> Command m ()
  ls :: FilePath -> Command m ()
  dir :: Command m ()
  help :: Command m ()
  mkdir :: FilePath -> Command m ()
  rmdir :: FilePath -> Command m ()
  cat :: FilePath -> Command m ()
  mkfile :: FilePath -> Command m ()
  wrfile :: FilePath -> String -> Command m ()
  info :: FilePath -> Command m ()
  find :: FilePath -> Command m ()

-- | instance for IO one
instance FSInterface IO where
  -- | 'cd' get the path to get to
  -- path could be either absolute or relative
  -- can throw NoDirectoryError if Directory doesn't exist
  cd :: FilePath -> Command IO ()
  cd path = do
    cur <- ask
    resultPath <- makePath path
    exists <- lift $ tryIOError $ doesDirectoryExist resultPath
    case exists of
      Left ex -> throw $ SystemError $ ioeGetErrorString ex
      Right e -> do
        case e of
          True -> liftIO $ writeIORef cur resultPath
          False -> throw $ NoDirectoryError "This Repository doesn't exist"
  -- | 'ls' get the path to get all directories and files as list
  -- path could be either absolute or relative
  -- can throw NoDirectoryError if Directory doesn't exist and ListDirectoryError if listDirectory return IOError
  ls :: FilePath -> Command IO ()
  ls path = do
    cur <- ask
    resultPath <- makePath path
    exists <- lift $ tryIOError $ doesDirectoryExist resultPath
    case exists of
      Left ex -> throw $ SystemError $ ioeGetErrorString ex
      Right val -> do
        if (val)
          then do
            result <- lift $ tryIOError (listDirectory resultPath)
            case result of
              Right res -> liftIO $ mapM_ putStrLn res
              Left ex -> throw $ ListDirectoryError $ ioeGetErrorString ex
            return ()
          else do
            throw $ NoDirectoryError "This Repository doesn't exist"
  -- | `mkdir` get the path to make all directories in it
  -- path could be either absolute or relative
  -- can throw `CreatingDirectoryError` if directory can't be made.
  mkdir :: FilePath -> Command IO ()
  mkdir path = do
    cur <- ask
    resultPath <- makePath path
    result <- lift $ tryIOError (createDirectoryIfMissing True resultPath)
    case result of
      Right _ -> return ()
      Left ex -> throw $ CreatingDirectoryError $ ioeGetErrorString ex
    return ()
  -- | `mkfile` get the name for file to make
  -- name should be real one (without directories)
  -- can throw `CreatingFileError` if file can't be created.
  mkfile :: FilePath -> Command IO ()
  mkfile name = do
    cur <- ask
    resultPath <- makePath name
    result <- lift $ tryIOError (writeFile resultPath "")
    case result of
      Right _ -> return ()
      Left ex -> throw $ CreatingFileError $ ioeGetErrorString ex
    return ()
  -- | `find` get the name for file to find
  -- name should be real one (without directories)
  find :: FilePath -> Command IO ()
  find name = do
    cur <- ask
    curPath <- liftIO $ readIORef cur
    resultPath <- makePath name
    result <- liftIO $ findfile curPath name
    case result of
      Just path -> liftIO $ mapM_ putStrLn ["Found file " ++ path]
      Nothing -> liftIO $ mapM_ putStrLn ["There are no file with name " ++ name]
    return ()
  -- | `info` get the name for file or directory to get information
  -- name could be either absolute or relative
  -- `NoDirectoryError` will appear if this name isn't a real one.
  info :: FilePath -> Command IO ()
  info name = do
    cur <- ask
    resultPath <- makePath name
    exists1 <- lift $ doesDirectoryExist resultPath
    if (exists1)
      then do
        permissions <- lift $ getPermissions resultPath
        time <- lift $ getModificationTime resultPath
        sizeAndFiles <- lift $ getSize resultPath
        liftIO $
          mapM_
            putStrLn
            [ "Path is " ++ (show resultPath),
              "Permissions " ++ (show permissions),
              "Last modified " ++ (show time),
              "Size " ++ (show $ fst sizeAndFiles),
              "There are " ++ (show $ snd sizeAndFiles) ++ " files"
            ]
        return ()
      else do
        exists2 <- lift $ doesFileExist resultPath
        if (exists2)
          then do
            permissions <- lift $ getPermissions resultPath
            time <- lift $ getModificationTime resultPath
            size <- lift $ getFileSize resultPath
            liftIO $
              mapM_
                putStrLn
                [ "Path is " ++ (show resultPath),
                  "Permissions " ++ (show permissions),
                  "Extension " ++ (show $ takeExtension resultPath),
                  "Last modified " ++ (show time),
                  "Size " ++ (show size)
                ]
          else do
            throw $ NoDirectoryError "This isn't a file or a dir"
            return ()
    return ()
  -- | `wrfile` get the name and text to write
  -- name could be either absolute or relative
  -- `CreatingFileError` will appear if there is no name in filder.
  wrfile :: FilePath -> String -> Command IO ()
  wrfile name text = do
    cur <- ask
    resultPath <- makePath name
    result <- lift $ tryIOError (writeFile resultPath text)
    case result of
      Right _ -> return ()
      Left ex -> throw $ CreatingFileError $ ioeGetErrorString ex
    return ()
  -- | `rmdir` get the path to dir to remove
  -- path could be either absolute or relative
  -- `RemovingDirectoryError` will appear if there is a trouble during
  rmdir :: FilePath -> Command IO ()
  rmdir path = do
    cur <- ask
    resultPath <- makePath path
    result <- lift $ tryIOError (removePathForcibly resultPath)
    case result of
      Right _ -> return ()
      Left ex -> throw $ RemovingDirectoryError $ ioeGetErrorString ex
    return ()
  -- | `cat` get the path to file to read
  -- path could be either absolute or relative
  -- `RemovingDirectoryError` will appear if there is a trouble during
  cat :: FilePath -> Command IO ()
  cat path = do
    cur <- ask
    resultPath <- makePath path
    contents <- liftIO $ tryIOError $ readFile resultPath
    case contents of
      Right c -> liftIO $ mapM_ putStrLn [c]
      Left ex -> throw $ NoFileError $ ioeGetErrorString ex
    return ()
  -- | `dir` show all folders and files in cur directory
  -- `RemovingDirectoryError` will appear if there is a trouble during `removePathForcibly` operation.
  dir :: Command IO ()
  dir = do
    cur <- ask
    curPath <- liftIO $ readIORef cur
    exists <- lift $ tryIOError $ doesDirectoryExist curPath
    case exists of
      Left ex -> throw $ NoDirectoryError $ ioeGetErrorString ex
      Right e -> do
        case e of
          True -> do
            result <- lift (listDirectory curPath)
            liftIO $ mapM_ putStrLn result
          False -> throw $ NoDirectoryError "error with cur path"
        return ()
  -- | `help` special function to present options for this program
  help :: Command IO ()
  help = do
    liftIO $
      mapM_
        putStrLn
        [ "cd <folder> -- перейти в директорию",
          "dir -- показать содержимое текущей директории",
          "ls <folder> -- показать содержимое выбранной директории",
          "create-folder 'folder-name' -- создать директорию в текущей",
          "cat <file> -- показать содержимое файла",
          "create-file 'file-name' -- создать пустой файл в текущей директории",
          "remove <folder | file> -- удалить выборанную директорию или файл",
          "write-file <file> 'text' -- записать текст в файл",
          "find-file 'file-name' --  поиск файла в текущией директории и поддиректориях",
          "info <file> -- показать информацию о файле",
          "info <folder> -- показать информацию о директории",
          "help --  показать руководство по использованию"
        ]
    return ()
