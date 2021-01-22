{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Exception (Exception (..), throw)
import Control.Monad.Catch (catch)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (lift, liftIO)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import FileManager (Command, FSInterface (..), MyException (..))
import System.Directory (doesDirectoryExist)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Util (global)

countWord :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
countWord splitter ch (x :| xs)
  | splitter == ch = ([] :| (x : xs))
  | otherwise = ((ch : x) :| xs)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn splitter str = foldr (countWord splitter) ([] :| []) str

commandLoop :: Command IO ()
commandLoop = do
  cur <- ask
  curPath <- liftIO $ readIORef cur
  liftIO $ putStr (curPath ++ "> ")
  cmd <- liftIO getLine
  do
    case (splitOn ' ' cmd) of
      "find" :| [name] -> do
        answer <- find name
        commandLoop
      "ls" :| [] -> do
        answer <- dir
        commandLoop
      "dir" :| [] -> do
        answer <- dir
        commandLoop
      "cat" :| [path] -> do
        answer <- cat path
        commandLoop
      "help" :| [] -> do
        answer <- help
        commandLoop
      "ls" :| [path] -> do
        answer <- ls path
        commandLoop
      "cd" :| [path] -> do
        answer <- cd path
        commandLoop
      "info" :| [path] -> do
        answer <- info path
        commandLoop
      "create-file" :| [name] -> do
        answer <- mkfile name
        commandLoop
      "write-file" :| [name, text] -> do
        answer <- wrfile name text
        commandLoop
      "create-folder" :| [path] -> do
        answer <- mkdir path
        commandLoop
      "remove" :| [path] -> do
        answer <- rmdir path
        commandLoop
      _ -> do
        liftIO $ putStrLn "type help for help, this command isn't in documentation"
        commandLoop
    `catch` (\ex -> (exceptionIgnorer ex))
  commandLoop

exceptionIgnorer :: MyException -> Command IO ()
exceptionIgnorer ex = do
  liftIO $ putStrLn (show ex)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Enter your directory: "
  path <- getLine
  exists <- liftIO $ doesDirectoryExist path
  case exists of
    True -> runReaderT commandLoop (global path)
    False -> main
