{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Copier
  ( main
  ) where

import           Control.Monad (when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Stack (HasCallStack)
import           Options.Applicative (Parser, argument, str, option, auto, value, help, metavar, long, short)
import qualified Options.Applicative as Opts
import           System.Directory (copyFile, removeDirectoryRecursive)
import           System.Exit (die)
import           System.IO.Error (catchIOError)
import           System.Posix.Directory.Traversals (allDirectoryContents')
import           System.Posix.FilePath (RawFilePath, (</>), takeFileName)
import           System.Posix.Files.ByteString (getFileStatus, getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink, fileMode, fileExist, directoryMode, createSymbolicLink, readSymbolicLink, removeLink)
import           System.Posix.Directory.ByteString (createDirectory, removeDirectory)
import           System.Posix.Types (FileMode)

import           PooledMapConcurrently (pooledMapConcurrently')
import           Sendfile (copyFileSendfile)


-- | Command line arguments of this program.
data CLIArgs = CLIArgs
  { sourceDir :: ByteString
  , destDir :: ByteString
  , numJobs :: Int
  } deriving (Eq, Ord, Show)


-- | Definition of the parser this program.
cliArgsParser :: Parser CLIArgs
cliArgsParser = CLIArgs
  <$> (T.encodeUtf8 . T.pack <$> argument str (metavar "SOURCE_DIR" <> help "Source directory"))
  <*> (T.encodeUtf8 . T.pack <$> argument str (metavar "DEST_DIR" <> help "Destination directory"))
  <*> option auto
      ( long "jobs"
        <> short 'j'
        <> metavar "N"
        <> Opts.showDefault
        <> value 1
        <> help "Number of parallel copy jobs"
      )


-- | Parses the command line arguments for this program.
parseArgs :: IO CLIArgs
parseArgs = Opts.execParser $
  Opts.info
    (Opts.helper <*> cliArgsParser)
    (Opts.fullDesc <> Opts.progDesc "Copies files in parallel")


-- | Decodes a ByteString of which we know it must be UTF-8,
-- dies if it isn't.
decodeUtf8OrDie :: (HasCallStack) => ByteString -> Text
decodeUtf8OrDie bs = case T.decodeUtf8' bs of
  Left unicodeException -> error $ "BUG: decodeUtf8OrDie: UnicodeException: " ++ show unicodeException
  Right text -> text


-- | Removes a file, link or directory if it exists.
-- Returns True when it did exist.
removeIfExists :: RawFilePath -> IO Bool
removeIfExists path = do
  mbStat <- (Just <$> getSymbolicLinkStatus path) `catchIOError` \_ -> return Nothing
  case mbStat of
    Nothing -> return False
    Just stat
      -- TODO Use a RawFilePath based method for this.
      | isDirectory stat -> removeDirectoryRecursive (T.unpack $ decodeUtf8OrDie path) >> return True
      | otherwise -> removeLink path >> return True


-- | Creates a directory with given permissions, if it doesn't exist.
mkdirIfMissing :: RawFilePath -> FileMode -> IO ()
mkdirIfMissing dir mode = do
  exists <- fileExist dir
  _ <- removeIfExists dir
  createDirectory dir mode


main :: IO ()
main = do
  CLIArgs
    { sourceDir
    , destDir
    , numJobs
    } <- parseArgs

  -- Arguments validation.
  when (numJobs < 1) $ die $ "--jobs must be positive; was: " ++ show numJobs

  pathsWithSourceDir <- allDirectoryContents' sourceDir
  case pathsWithSourceDir of
    [] -> return ()
    (start:sourcePaths) -> do
      when (start /= sourceDir) $ error $ "BUG: start /= sourceDir: " ++ show (start, sourceDir)

      sourceDirExists <- fileExist sourceDir
      when (not sourceDirExists) $ die $ "Source directory " ++ T.unpack (decodeUtf8OrDie sourceDir) ++ " does not exist"
      destDirExists <- fileExist destDir
      when (not destDirExists) $ die $ "Target directory " ++ T.unpack (decodeUtf8OrDie destDir) ++ " does not exist"

      let prefixLength = BS.length sourceDir + 1

      _ <- flip (pooledMapConcurrently' numJobs) sourcePaths $ \sourcePath -> do

        let pathInsideSourceDir = BS.drop prefixLength sourcePath
        let dest = destDir </> pathInsideSourceDir

        sourceStatus <- getSymbolicLinkStatus sourcePath
        if
          | isDirectory sourceStatus -> do
              mkdirIfMissing dest (fileMode sourceStatus) -- preserve dir perms
          | isSymbolicLink sourceStatus -> do
              linkPointerTarget <- readSymbolicLink sourcePath
              -- TODO: Now this is not pretty with the `catchIOError` to decide
              -- if the symlink dosn't exist, but that's how `doesFileExist`
              -- does it. Switch this to a proper `lstat()` ENOENT handling later.
              _ <- removeIfExists dest
              createSymbolicLink linkPointerTarget dest
          | isRegularFile sourceStatus -> do
              let toFileCreateMode = fileMode sourceStatus
              copyFileSendfile sourcePath (Just sourceStatus) dest toFileCreateMode
          | otherwise -> do
              die $ "Can only copy regular files, symlinks and directories, but this isn't one: " ++ T.unpack (decodeUtf8OrDie sourcePath)

      return ()
