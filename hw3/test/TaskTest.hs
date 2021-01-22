module TaskTest where

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.State (State, evalState, execState, get, lift, put)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as MP
import FileManager (MyException (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

data FileSystem = FileSystem
  { getRoot :: Dir,
    getCurPath :: FilePath
  }
  deriving (Show, Eq)

type GameCommandIO = ExceptT MyException (State FileSystem)

data FD
  = F File
  | D Dir
  deriving (Show, Eq)

data Dir = Dir
  { dirName :: FilePath,
    catalog :: (MP.Map FilePath FD)
  }
  deriving (Show, Eq)

data File = File
  { fileName :: FilePath,
    content :: [Char]
  }
  deriving (Show, Eq)

isRelative :: String -> Bool
isRelative ('$' : _) = False
isRelative _ = True

splitOn :: Char -> String -> [String]
splitOn splitter str = foldr (countWord splitter) [[]] str

addWord :: Char -> String -> String -> String
addWord joiner str xs = str ++ [joiner] ++ xs

joinWith :: Char -> [String] -> String
joinWith joiner masStr = init $ foldr (addWord joiner) "" masStr

countWord :: Char -> Char -> [String] -> [String]
countWord splitter ch val@(x : xs)
  | splitter == ch = ([] : val)
  | otherwise = ((ch : x) : xs)

checkDirInMap :: MP.Map FilePath FD -> FilePath -> Maybe Dir
checkDirInMap mp path = do
  let res = MP.lookup path mp
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D a -> Just a
      F a -> Nothing

letsGoToPath :: Dir -> [FilePath] -> Maybe Dir
letsGoToPath nowPath [] = Just nowPath
letsGoToPath nowPath (x : xs) = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D dir -> letsGoToPath dir xs
      F file -> Nothing

getFileData :: Dir -> [FilePath] -> Maybe String
getFileData nowPath [] = Nothing
getFileData nowPath (x : []) = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D dir -> Nothing
      F file -> Just (content file)
getFileData nowPath (x : xs) = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D dir -> getFileData dir xs
      F file -> Nothing

listLetsGoToPath :: Dir -> [FilePath] -> Maybe [FilePath]
listLetsGoToPath nowPath [] = Just (MP.keys (catalog nowPath))
listLetsGoToPath nowPath (x : xs) = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D dir -> listLetsGoToPath dir xs
      F file -> Nothing

makeDir :: Dir -> [FilePath] -> Maybe Dir
makeDir nowPath [] = Just nowPath
makeDir nowPath (x : xs) = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Nothing -> do
      let dir =
            Dir
              { dirName = x,
                catalog = MP.empty
              }
      let realDir = makeDir dir xs
      case realDir of
        Nothing -> Just nowPath
        Just val -> do
          let newDir =
                Dir
                  { dirName = dirName nowPath,
                    catalog = MP.insert x (D val) (catalog nowPath)
                  }
          Just newDir
    Just y -> case y of
      D dir -> do
        let realDir = makeDir dir xs
        case realDir of
          Nothing -> Nothing
          Just val -> do
            let newDir =
                  Dir
                    { dirName = dirName nowPath,
                      catalog = MP.insert x (D val) (catalog nowPath)
                    }
            Just newDir
      F file -> Just nowPath

removeDir :: Dir -> [FilePath] -> Maybe Dir
removeDir nowPath (x : []) = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Nothing -> Just nowPath
    Just y -> case y of
      D dir -> do
        let newDir =
              Dir
                { dirName = dirName nowPath,
                  catalog = MP.delete x (catalog nowPath)
                }
        Just newDir
      F file -> Just nowPath
removeDir nowPath (x : xs) = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Just y -> case y of
      D dir -> do
        let realDir = removeDir dir xs
        case realDir of
          Just val -> do
            let newDir =
                  Dir
                    { dirName = dirName nowPath,
                      catalog = MP.insert x (D val) (catalog nowPath)
                    }
            Just newDir
      F file -> Nothing

addFile :: Dir -> [FilePath] -> String -> String -> Maybe Dir
addFile nowPath [] name cont = do
  let res = MP.lookup name (catalog nowPath)
  case res of
    Nothing -> do
      let newDir =
            Dir
              { dirName = dirName nowPath,
                catalog =
                  MP.insert
                    name
                    ( F
                        File
                          { fileName = name,
                            content = cont
                          }
                    )
                    (catalog nowPath)
              }
      Just newDir
    Just y -> case y of
      D dir -> Nothing
      F file -> do
        let newDir =
              Dir
                { dirName = dirName nowPath,
                  catalog =
                    MP.insert
                      name
                      ( F
                          File
                            { fileName = name,
                              content = (content file) ++ cont
                            }
                      )
                      (catalog nowPath)
                }
        Just newDir
addFile nowPath (x : xs) name cont = do
  let res = MP.lookup x (catalog nowPath)
  case res of
    Nothing -> Nothing
    Just y -> case y of
      D dir -> do
        let realDir = addFile dir xs name cont
        case realDir of
          Nothing -> Nothing
          Just val -> do
            let newDir =
                  Dir
                    { dirName = dirName nowPath,
                      catalog = MP.insert x (D val) (catalog nowPath)
                    }
            Just newDir
      F file -> Just nowPath

findSmth :: Dir -> FilePath -> [FilePath] -> String -> Maybe FilePath
findSmth nowDir nowPath (y : ys) name = do
  let res = MP.lookup y (catalog nowDir)
  case res of
    Just x -> case x of
      D dir -> do
        let res = findFile dir (nowPath ++ "\\" ++ (dirName dir)) [] name
        case res of
          Nothing -> findSmth nowDir nowPath ys name
          Just r -> Just r
      F file -> findSmth nowDir nowPath ys name

findFile :: Dir -> FilePath -> [FilePath] -> String -> Maybe FilePath
findFile nowDir nowPath [] name = do
  let res = MP.lookup name (catalog nowDir)
  case res of
    Nothing -> do
      let keys = MP.keys (catalog nowDir)
      findSmth nowDir nowPath keys name
    Just x -> case x of
      D dir -> Nothing
      F file -> Just (nowPath ++ "\\" ++ name)
findFile nowDir nowPath (x : xs) name = do
  let res = MP.lookup x (catalog nowDir)
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D dir -> findFile dir (nowPath ++ ['\\'] ++ (dirName dir)) xs name
      F file -> Nothing

getSizes :: Dir -> [FilePath] -> Integer
getSizes nowDir [] = 0
getSizes nowDir (x : xs) = do
  let res = MP.lookup x (catalog nowDir)
  case res of
    Just x -> case x of
      D dir -> (getSize dir + getSizes nowDir xs)
      F file -> toInteger (length $ content file)

getSize :: Dir -> Integer
getSize nowDir = do
  let res = MP.keys (catalog nowDir)
  getSizes nowDir res

getInfo :: Dir -> FilePath -> [FilePath] -> String -> Maybe (FD, Integer, FilePath)
getInfo nowDir nowPath [] name = do
  let res = MP.lookup name (catalog nowDir)
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D dir -> Just (D dir, (getSize dir), (nowPath ++ ['\\'] ++ (dirName dir)))
      F file -> Just (F file, toInteger (length $ content file), (nowPath ++ ['\\'] ++ (fileName file)))
getInfo nowDir nowPath (x : xs) name = do
  let res = MP.lookup x (catalog nowDir)
  case res of
    Nothing -> Nothing
    Just x -> case x of
      D dir -> getInfo dir (nowPath ++ ['\\'] ++ (dirName dir)) xs name
      F file -> Nothing

getRealPath :: [String] -> [String] -> String
getRealPath stack [] = joinWith '\\' ("$" : (reverse stack))
getRealPath [] (".." : xs) = getRealPath [] xs
getRealPath (x : xs) (".." : ys) = getRealPath xs ys
getRealPath xs (y : ys) = getRealPath (y : xs) ys

cd :: FilePath -> GameCommandIO ()
cd path = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let resultPath
        | isRelative path = (curPath ++ ['\\'] ++ path)
        | otherwise = path
  let splPath = (tail (splitOn '\\' resultPath))
  let canon = getRealPath [] splPath

  let result = letsGoToPath root (tail (splitOn '\\' canon))
  case result of
    Nothing -> return ()
    Just _ -> put (cur {getRoot = root, getCurPath = canon})
  return ()

ls :: FilePath -> GameCommandIO ([FilePath])
ls path = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let resultPath
        | isRelative path = (curPath ++ ['\\'] ++ path)
        | otherwise = path
  let splPath = (tail (splitOn '\\' resultPath))
  let canon = getRealPath [] splPath

  let result = listLetsGoToPath root (tail (splitOn '\\' canon))
  case result of
    Nothing -> return ([])
    Just x -> return (x)

dir :: GameCommandIO ([FilePath])
dir = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)

  let result = listLetsGoToPath root (tail (splitOn '\\' curPath))
  case result of
    Nothing -> return ([])
    Just x -> return (x)

cat :: FilePath -> GameCommandIO ([Char])
cat path = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let resultPath
        | isRelative path = (curPath ++ ['\\'] ++ path)
        | otherwise = path
  let splPath = (tail (splitOn '\\' resultPath))
  let canon = getRealPath [] splPath

  let result = getFileData root (tail (splitOn '\\' canon))
  case result of
    Nothing -> return ([])
    Just x -> return (x)

mkdir :: FilePath -> GameCommandIO ()
mkdir path = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let resultPath
        | isRelative path = (curPath ++ ['\\'] ++ path)
        | otherwise = path
  let splPath = (tail (splitOn '\\' resultPath))
  let canon = getRealPath [] splPath

  let result = makeDir root (tail (splitOn '\\' canon))
  case result of
    Nothing -> return ()
    Just x -> put (cur {getRoot = x, getCurPath = curPath})

rmdir :: FilePath -> GameCommandIO ()
rmdir path = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let resultPath
        | isRelative path = (curPath ++ ['\\'] ++ path)
        | otherwise = path
  let splPath = (tail (splitOn '\\' resultPath))
  let canon = getRealPath [] splPath
  let result = makeDir root (tail (splitOn '\\' canon))
  case result of
    Nothing -> return ()
    Just y -> do
      let res = removeDir y (tail (splitOn '\\' canon))
      case res of
        Just z -> do
          put (cur {getRoot = z, getCurPath = curPath})

wrfile :: String -> String -> GameCommandIO ()
wrfile name cont = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let result = addFile root (tail (splitOn '\\' curPath)) name cont
  case result of
    Nothing -> return ()
    Just y -> put (cur {getRoot = y, getCurPath = curPath})

mkfile :: String -> GameCommandIO ()
mkfile name = wrfile name ""

find :: String -> GameCommandIO (FilePath)
find name = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let result = findFile root "$" (tail (splitOn '\\' curPath)) name
  case result of
    Nothing -> return ""
    Just y -> return y

info :: String -> GameCommandIO (String)
info name = do
  cur <- get
  let root = (getRoot cur)
  let curPath = (getCurPath cur)
  let result = getInfo root "$" (tail (splitOn '\\' curPath)) name
  case result of
    Nothing -> return ""
    Just (F x, y, z) -> return ("This is file with size " ++ (show y) ++ " in " ++ z)
    Just (D x, y, z) -> return ("This is dir, which size is " ++ (show y) ++ " in " ++ z)

--    info :: FilePath -> Command m ()

txt1 :: File
txt1 =
  File
    { fileName = "1.txt",
      content = "Три охотника одновременно и независимо стреляют в кабана."
    }

txt2 :: File
txt2 =
  File
    { fileName = "2.txt",
      content = "В кабане найдено 2 пули."
    }

txt2empty :: File
txt2empty =
  File
    { fileName = "2.txt",
      content = ""
    }

dir1 :: Dir
dir1 =
  Dir
    { dirName = "dir1",
      catalog = MP.fromList [("2.txt", F txt2)]
    }

dir2 :: Dir
dir2 =
  Dir
    { dirName = "dir2",
      catalog = MP.empty
    }

dir3 :: Dir
dir3 =
  Dir
    { dirName = "dir3",
      catalog = MP.empty
    }

dir2secondVar :: Dir
dir2secondVar =
  Dir
    { dirName = "dir2",
      catalog = MP.fromList [("dir3", D dir3)]
    }

easyDir1 :: Dir
easyDir1 =
  Dir
    { dirName = "$",
      catalog = MP.fromList [("1.txt", F txt1), ("dir1", D dir1)]
    }

easyDir2 :: Dir
easyDir2 =
  Dir
    { dirName = "$",
      catalog = MP.fromList [("1.txt", F txt1), ("dir1", D dir1), ("dir2", D dir2)]
    }

easyDir3 :: Dir
easyDir3 =
  Dir
    { dirName = "$",
      catalog = MP.fromList [("1.txt", F txt1), ("dir1", D dir1), ("dir2", D dir2secondVar)]
    }

easyDir5 :: Dir
easyDir5 =
  Dir
    { dirName = "$",
      catalog = MP.fromList [("1.txt", F txt1), ("dir1", D dir1), ("2.txt", F txt2empty)]
    }

easyDir6 :: Dir
easyDir6 =
  Dir
    { dirName = "$",
      catalog = MP.fromList [("1.txt", F txt1), ("dir1", D dir1), ("2.txt", F txt2)]
    }

easyFileSystem1 :: FileSystem
easyFileSystem1 = do
  let fs =
        FileSystem
          { getRoot = easyDir1,
            getCurPath = "$"
          }
  fs

easyFileSystem2 :: FileSystem
easyFileSystem2 = do
  let fs =
        FileSystem
          { getRoot = easyDir1,
            getCurPath = "$\\dir1"
          }
  fs

easyFileSystem3 :: FileSystem
easyFileSystem3 = do
  let fs =
        FileSystem
          { getRoot = easyDir2,
            getCurPath = "$"
          }
  fs

easyFileSystem4 :: FileSystem
easyFileSystem4 = do
  let fs =
        FileSystem
          { getRoot = easyDir3,
            getCurPath = "$"
          }
  fs

easyFileSystem5 :: FileSystem
easyFileSystem5 = do
  let fs =
        FileSystem
          { getRoot = easyDir5,
            getCurPath = "$"
          }
  fs

easyFileSystem6 :: FileSystem
easyFileSystem6 = do
  let fs =
        FileSystem
          { getRoot = easyDir6,
            getCurPath = "$"
          }
  fs

specTests :: Spec
specTests = do
  describe "isRelative" $ do
    it "isRelative dir1" $
      (isRelative "dir1") `shouldBe` (True)
    it "isRelative $\\dir1" $
      (isRelative "$\\dir1") `shouldBe` (False)
    it "getRealPath" $
      (getRealPath [] (tail (splitOn '\\' "$\\dir1\\..\\a\\b\\c\\..\\..\\d"))) `shouldBe` ("$\\a\\d")
  describe "cd" $ do
    it "cd dir1" $
      (execState (runExceptT $ cd "dir1") easyFileSystem1) `shouldBe` (easyFileSystem2)
    it "cd $\\dir1" $
      (execState (runExceptT $ cd "$\\dir1") easyFileSystem1) `shouldBe` (easyFileSystem2)
    it "cd $" $
      (execState (runExceptT $ cd "$") easyFileSystem1) `shouldBe` (easyFileSystem1)
    it "cd .." $
      (execState (runExceptT $ cd "..") easyFileSystem2) `shouldBe` (easyFileSystem1)
  describe "ls" $ do
    it "ls dir1" $
      (evalState (runExceptT $ ls "dir1") easyFileSystem1) `shouldBe` Right (["2.txt"])
    it "ls $\\dir1" $
      (evalState (runExceptT $ ls "$\\dir1") easyFileSystem1) `shouldBe` Right (["2.txt"])
    it "ls $" $
      (evalState (runExceptT $ ls "$") easyFileSystem1) `shouldBe` Right (["1.txt", "dir1"])
  describe "dir" $ do
    it "dir" $
      (evalState (runExceptT $ dir) easyFileSystem1) `shouldBe` Right (["1.txt", "dir1"])
    it "dir" $
      (evalState (runExceptT $ dir) easyFileSystem2) `shouldBe` Right (["2.txt"])
  describe "cat" $ do
    it "cat txt1" $
      (evalState (runExceptT $ cat "1.txt") easyFileSystem1) `shouldBe` Right ("Три охотника одновременно и независимо стреляют в кабана.")
    it "cat txt2" $
      (evalState (runExceptT $ cat "2.txt") easyFileSystem2) `shouldBe` Right ("В кабане найдено 2 пули.")
    it "cat $\\dir1\\2.txt" $
      (evalState (runExceptT $ cat "$\\dir1\\2.txt") easyFileSystem1) `shouldBe` Right ("В кабане найдено 2 пули.")
  describe "mkdir" $ do
    it "mkdir dir2" $
      (execState (runExceptT $ mkdir "dir2") easyFileSystem1) `shouldBe` (easyFileSystem3)
    it "mkdir dir2\\dir3" $
      (execState (runExceptT $ mkdir "dir2\\dir3") easyFileSystem1) `shouldBe` (easyFileSystem4)
    it "mkdir $\\dir2\\dir3" $
      (execState (runExceptT $ mkdir "$\\dir2\\dir3") easyFileSystem1) `shouldBe` (easyFileSystem4)
    it "mkdir $\\dir1" $
      (execState (runExceptT $ mkdir "$\\dir1") easyFileSystem1) `shouldBe` (easyFileSystem1)
  describe "rmdir" $ do
    it "rmdir dir2" $
      (execState (runExceptT $ rmdir "dir2") easyFileSystem3) `shouldBe` (easyFileSystem1)
    it "rmdir dir2\\dir3" $
      (execState (runExceptT $ rmdir "dir2\\dir3") easyFileSystem4) `shouldBe` (easyFileSystem3)
    it "rmdir $\\dir2\\dir3" $
      (execState (runExceptT $ rmdir "$\\dir2\\dir3") easyFileSystem4) `shouldBe` (easyFileSystem3)
  describe "mkfile" $ do
    it "mkfile txt2" $
      (execState (runExceptT $ mkfile "2.txt") easyFileSystem1) `shouldBe` (easyFileSystem5)
    it "wrfile txt3 text" $
      (execState (runExceptT $ wrfile "2.txt" "В кабане найдено 2 пули.") easyFileSystem5) `shouldBe` (easyFileSystem6)
  describe "find" $ do
    it "find txt2" $
      (evalState (runExceptT $ find "2.txt") easyFileSystem1) `shouldBe` Right ("$\\dir1\\2.txt")
    it "find txt1" $
      (evalState (runExceptT $ find "1.txt") easyFileSystem1) `shouldBe` Right ("$\\1.txt")
  describe "info" $ do
    it "info txt1" $
      (evalState (runExceptT $ info "1.txt") easyFileSystem1) `shouldBe` Right ("This is file with size 57 in $\\1.txt")
    it "info $" $
      (evalState (runExceptT $ info "dir1") easyFileSystem1) `shouldBe` Right ("This is dir, which size is 24 in $\\dir1")

testsFileSystem :: IO TestTree
testsFileSystem = testSpec "All Tests" specTests
