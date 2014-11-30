module Fcd (
    run
  , lcs
  , sortCandidates
) where

import Control.Monad
import Control.Concurrent.MVar
import Data.Array
import Data.List
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Directory
import System.Posix.Signals
import System.Environment (getArgs)

import Graphics.Vty.Widgets.List
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Edit
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.EventLoop
import Graphics.Vty.Input

import Paths_fcd (version)
import Data.Version

data Command = Add | List | PrintVersion | Help | Select | Delete deriving (Eq, Show)
data Args = Args { command :: Command, name :: String, shortcut :: String, description :: String, execute :: [String] -> IO ()}

availableCommands :: [Args]
availableCommands = [ Args { command = Add, name = "add", shortcut = "a", description = "Add new bookmarks. If no additional parameter is provided, bookmark the current directory.", execute = addBookmarks }
                    , Args { command = List, name = "list", shortcut = "l", description = "List the available bookmarks.", execute = const listBookmarks}
                    , Args { command = PrintVersion, name = "version", shortcut = "v", description = "Print version number.", execute = const printVersion}
                    , Args { command = Help, name = "help", shortcut = "h", description = "Print this help.", execute = const printHelp}
                    , Args { command = Select, name = "select", shortcut = "s", description = "Select a bookmark. This is equivalent to calling fcd without arguments.", execute = selectBookmark }
                    , Args { command = Delete, name = "delete", shortcut = "d", description = "Delete a bookmark.", execute = deleteBookmark }
                    ]

-- |Main entry point for the program
run :: IO ()
run = do
  args <- getArgs
  let (cmdName:opts) = if null args then ["s"] else args
  case parseCommand cmdName of
    Nothing -> error $ "Unrecognized command: " ++ cmdName ++ ".\nAvailable commands are: " ++ commandList
    Just cmd -> execute cmd opts

-- |Given a command name or a short-hand version of a command name, returns the
-- corresponding command
parseCommand :: String -> Maybe Args
parseCommand cmd = find (\arg -> name arg == cmd || shortcut arg == cmd) availableCommands

-- |Add each path in the provided list to the bookmarks file.
addBookmarks :: [String] -> IO ()
addBookmarks [] = addBookmarks ["."]
addBookmarks xs = do
  allBookmarks <- readBookmarks
  mapM_ (addBookmark allBookmarks . T.pack) xs

-- |Print to stdout the list of bookmarks.
listBookmarks :: IO ()
listBookmarks = liftM (T.unpack . T.intercalate (T.pack "\n")) readBookmarks >>= putStrLn

-- |Print to stdout the version number.
printVersion :: IO ()
printVersion = putStrLn $ "fcd version " ++ showVersion version

-- |Print to stdout a short help about the program.
printHelp :: IO ()
printHelp = do
  printVersion
  putStrLn "Usage: fcd [select [PATH]] | fcd add [PATH1 [...]] | fcd delete [PATH] | fcd list | fcd help | ..."
  putStrLn ""
  putStrLn "Available commands:"
  forM_ availableCommands $ \cmd ->
    putStrLn $ name cmd ++ " (" ++ shortcut cmd ++ ") -- " ++ description cmd
  putStrLn ""
  putStrLn "Each command can be abbreviated to its first letter."
  putStrLn "When in prompt mode, use C-h and C-l to navigate the result list."

-- |Prompt the user to select a bookmark, then write the selected bookmark in
-- the bookmark file.
selectBookmark :: [String] -> IO ()
selectBookmark opts = displayPrompt (T.pack $ unwords opts) >>= writeResult

-- |Prompt the user to select a bookmark, then delete the selected bookmark from
-- the bookmark file.
deleteBookmark :: [String] -> IO ()
deleteBookmark opts = do
  toDelete <- displayPrompt $ T.pack $ unwords opts
  path <- bookmarkFile
  bookmarks <- liftM T.lines (T.IO.readFile path)
  let filtered = filter (/= toDelete) bookmarks
  T.IO.writeFile path $ T.concat $ map (flip T.append $ T.pack "\n") filtered

-- |Return the path to the bookmark file.
bookmarkFile :: IO String
bookmarkFile = fmap (++ "/.fcdbookmarks") getHomeDirectory

-- |Return the path to the result file.
resultFile :: IO String
resultFile = fmap (++ "/.fcdresult") getHomeDirectory

-- |Return the list of the available commands (list, add, etc.)
commandList :: String
commandList = intercalate ", " $ map name availableCommands

-- |Add a bookmark (a directory path) to the bookmark file.
-- If the bookmark already exists, then do nothing.
addBookmark :: [T.Text] -> T.Text -> IO ()
addBookmark allBookmarks bookmark = do
  path <- bookmarkFile
  bookmarkClean <- canonicalizePath $ T.unpack bookmark
  unless (T.pack bookmarkClean `elem` allBookmarks) $
    appendFile path (bookmarkClean ++ "\n")

-- |Prompt the user to select a bookmark. The given parameter is used to
-- prepopulate the prompt. Return the user selection.
displayPrompt :: T.Text -> IO T.Text
displayPrompt prefill = do
  result <- newMVar $ T.pack "."
  inputLine <- editWidget
  setEditText inputLine prefill

  candidates <- readBookmarks
  candidatesList <- newTextList candidates 1
  box <- vBox inputLine candidatesList

  mainFocusGroup <- newFocusGroup
  mainFocusGroup `onKeyPressed` \_ key modifiers ->
    if key == KChar 'c' && modifiers == [MCtrl] then
      raiseSignal sigINT >> return True
    else if key == KChar 'd' && modifiers == [MCtrl] then
      raiseSignal sigKILL >> return True
    else if key == KChar 'h' && modifiers == [MCtrl] then
      moveSelectionDown candidatesList >> return True
    else if key == KChar 'l' && modifiers == [MCtrl] then
      moveSelectionUp candidatesList >> return True
    else return False

  _ <- addToFocusGroup mainFocusGroup box

  collection <- newCollection
  _ <- addToCollection collection box mainFocusGroup

  inputLine `onChange` \inputText -> updateCandidates inputText candidates candidatesList

  inputLine `onActivate` \_ -> do
    choice <- getSelected candidatesList
    let selection = case choice of
                  Nothing -> T.pack "."
                  Just (_pos, (entry, _) ) -> entry
    modifyMVar_ result (const $ return selection)
    shutdownUi

  updateCandidates prefill candidates candidatesList

  runUi collection defaultContext
  takeMVar result

moveSelectionDown :: Widget (List T.Text FormattedText) -> IO ()
moveSelectionDown listWidget = do
  selected <- getSelected listWidget
  case selected of
    Nothing -> return ()
    Just (pos, _) -> unless (pos == 0) (setSelected listWidget (pos - 1))

moveSelectionUp :: Widget (List T.Text FormattedText) -> IO ()
moveSelectionUp listWidget = do
  selected <- getSelected listWidget
  case selected of
    Nothing -> return ()
    Just (pos, _) -> setSelected listWidget (pos + 1) -- vty-ui takes care of not going out of bounds

-- |Update a list widget by sorting the candidates list according to a reference
updateCandidates :: T.Text                              -- a reference (user provided input)
                  -> [T.Text]                           -- a list of candidates
                  -> Widget (List T.Text FormattedText) -- the list widget to update
                  -> IO ()
updateCandidates inputText candidates candidatesList = do
  let sortedCandidates = sortCandidates (map T.unpack candidates) (T.unpack inputText)
      displayedCandidates = map T.pack sortedCandidates
  setCandidates displayedCandidates candidatesList

-- |Write the selected bookmark to the result file
writeResult :: T.Text -> IO ()
writeResult selection = resultFile >>= flip T.IO.writeFile selection

-- |Retrieve the bookmark list from disk
readBookmarks :: IO [T.Text]
readBookmarks = do
  path <- bookmarkFile
  exist <- doesFileExist path
  if exist
  then liftM T.lines $ T.IO.readFile path
  else return []

setCandidates :: [T.Text] -> Widget (List T.Text FormattedText) -> IO ()
setCandidates candidates list = do
  clearList list
  forM_ candidates (\el -> addToList list el =<< plainText el)

-- |Compute the length of the longest common subsequence of two lists.
-- This is a simple implementation with memoization that uses quadratic space
-- (O(n*m) where n and m are the length of the inputs).
-- Note: a possible optimization would be to cache the memoized matrix as a lot
-- of it is still relevant when a new character is added.
lcs :: (Eq a) => [a] -> [a] -> Int
lcs xs ys = memoized ! (n,m)
  where memoized = array ((0,0),(n,m)) [((i,j), lcs' i j) | i <- [0..n], j <- [0..m] ]
        n = length xs
        m = length ys
        as = listArray (1, n) xs
        bs = listArray (1, m) ys
        lcs' _ 0 = 0
        lcs' 0 _ = 0
        lcs' u v = if as ! u == bs ! v
                   then memoized ! (u - 1, v - 1) + 1
                   else max (memoized ! (u - 1, v)) (memoized ! (u, v - 1))

sortCandidates :: [String] -> String -> [String]
sortCandidates candidates reference = sortBy comparator candidates
  where comparator x y =
         let distRefToX = distance (map toLower x) (map toLower reference)
             distRefToY = distance (map toLower y) (map toLower reference)
             distance a b  = - lcs a b
         in compare distRefToX distRefToY


