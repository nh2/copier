{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Copier
  ( main
  ) where

import           Control.Monad (when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (diffTimeToPicoseconds, utctDayTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Traversable (for)
import           GHC.Stack (HasCallStack)
import           Options.Applicative (Parser, argument, str, option, auto, value, help, metavar, long, short)
import qualified Options.Applicative as Opts
import           System.Directory (removeDirectoryRecursive)
import           System.Exit (die)
import qualified System.FilePath as FP
import           System.IO.Error (catchIOError)
import           System.Posix.Directory.Traversals (allDirectoryContents')
import           System.Posix.FilePath (RawFilePath, (</>))
import           System.Posix.Files.ByteString (isDirectory, isRegularFile, isSymbolicLink, fileMode, fileExist, createSymbolicLink, readSymbolicLink, removeLink, modificationTimeHiRes, accessTimeHiRes, fileSize)
import           System.Posix.Directory.ByteString (createDirectory)
import           System.Posix.Types (FileMode)

import           InterruptibleUtimensat (setFileTimesHiResNonBlocking)
import           PooledMapConcurrently (pooledMapConcurrently')
import           Sendfile (copyFileSendfile)
import           SafeFileFFI (getSymbolicLinkStatusNonBlocking)


-- | Command line arguments of this program.
data CLIArgs = CLIArgs
  { sourceDir :: ByteString
  , destDir :: ByteString
  , numJobs :: Int
  , assumeMicrosecondTimeResolution :: Bool
  , verbose :: Bool
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
  <*> Opts.switch
      ( long "assume-microsecond-time-resolution"
        <> help ("Don't try to synchronise mtimes when the difference between"
                 ++ " source and target mtime is less than 1000 nanoseconds"
                 ++ " (useful e.g. for versions of GlusterFS that don't support"
                 ++ " nanosecond time stamps)")
      )
  <*> Opts.switch
      ( long "verbose"
        <> short 'v'
        <> help "Increase verbosity"
      )


-- | Parses the command line arguments for this program.
parseArgs :: IO CLIArgs
parseArgs = Opts.execParser $
  Opts.info
    (Opts.helper <*> cliArgsParser)
    (Opts.fullDesc <> Opts.progDesc "Copies files in parallel. Makes DEST_DIR contain at least all contents from SOURCE_DIR.")


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
  -- TODO: Now this is not pretty with the `catchIOError` to decide
  -- if the symlink dosn't exist, but that's how `doesFileExist`
  -- does it. Switch this to a proper `lstat()` ENOENT handling later.
  mbStat <- (Just <$> getSymbolicLinkStatusNonBlocking path) `catchIOError` \_ -> return Nothing
  case mbStat of
    Nothing -> return False
    Just stat
      -- TODO Use a RawFilePath based method for this.
      | isDirectory stat -> removeDirectoryRecursive (T.unpack $ decodeUtf8OrDie path) >> return True
      | otherwise -> removeLink path >> return True


-- | Creates a directory with given permissions, if it doesn't exist.
mkdirIfMissing :: RawFilePath -> FileMode -> IO ()
mkdirIfMissing dir mode = do
  _ <- removeIfExists dir
  putStrLn $ "creating dir " ++ show dir
  createDirectory dir mode


-- Missing from posix-paths
normaliseRawFilePath :: RawFilePath -> RawFilePath
normaliseRawFilePath = T.encodeUtf8 . T.pack . FP.normalise . T.unpack . decodeUtf8OrDie


main :: IO ()
main = do
  CLIArgs
    { sourceDir = sourceDirUnNormalised
    , destDir = destDirUnNormalised
    , numJobs
    , assumeMicrosecondTimeResolution
    , verbose
    } <- parseArgs

  -- Arguments validation.
  when (numJobs < 1) $ die $ "--jobs must be positive; was: " ++ show numJobs

  -- These are now guaranteed to have trailing slashes at the end.
  let sourceDir = normaliseRawFilePath (sourceDirUnNormalised <> "/")
  let destDir = normaliseRawFilePath (destDirUnNormalised <> "/")

  pathsWithSourceDir <- allDirectoryContents' sourceDir
  case pathsWithSourceDir of
    [] -> return ()
    (start:sourcePaths) -> do
      when (start /= sourceDir) $ error $ "BUG: start /= sourceDir: " ++ show (start, sourceDir)

      sourceDirExists <- fileExist sourceDir
      when (not sourceDirExists) $ die $ "Source directory " ++ T.unpack (decodeUtf8OrDie sourceDir) ++ " does not exist"
      destDirExists <- fileExist destDir
      when (not destDirExists) $ die $ "Target directory " ++ T.unpack (decodeUtf8OrDie destDir) ++ " does not exist"

      let prefixLength = BS.length sourceDir

      let copyCreate sourcePath = do

            when verbose $ putStrLn $ "Considering " ++ show sourcePath
            let pathInsideSourceDir = BS.drop prefixLength sourcePath
            let dest = destDir </> pathInsideSourceDir

            sourceStatus <- getSymbolicLinkStatusNonBlocking sourcePath
            if
              | isDirectory sourceStatus -> do
                  mkdirIfMissing dest (fileMode sourceStatus) -- preserve dir perms
              | isSymbolicLink sourceStatus -> do
                  linkPointerTarget <- readSymbolicLink sourcePath
                  _ <- removeIfExists dest
                  createSymbolicLink linkPointerTarget dest
              | isRegularFile sourceStatus -> do
                  let toFileCreateMode = fileMode sourceStatus
                  let performFullCopy = copyFileSendfile sourcePath (Just sourceStatus) dest toFileCreateMode

                  -- let sourceAtime = accessTimeHiRes sourceStatus
                  let sourceMtime = modificationTimeHiRes sourceStatus
                  let sourceSize = fileSize sourceStatus
                  m'destStatus <- (Just <$> getSymbolicLinkStatusNonBlocking dest) `catchIOError` \_ -> return Nothing

                  case m'destStatus of
                    Nothing -> do
                      when verbose $ putStrLn $ "  Performing copy (nonexistent) " ++ T.unpack (decodeUtf8OrDie sourcePath)
                      performFullCopy

                    Just destStatus -> do
                      let destAtime = accessTimeHiRes destStatus
                      let destMtime = modificationTimeHiRes destStatus
                      let destSize = fileSize destStatus

                      let mtimeMicrosDiff = sourceMtimeMicros - destMtimeMicros
                            where
                              sourceMtimeMicros = diffTimeToPicoseconds (utctDayTime (posixSecondsToUTCTime sourceMtime)) `quot` 1000000
                              destMtimeMicros = diffTimeToPicoseconds (utctDayTime (posixSecondsToUTCTime destMtime)) `quot` 1000000

                      let mtimesDisagree
                            | assumeMicrosecondTimeResolution = mtimeMicrosDiff > 0
                            | otherwise                       = sourceMtime > destMtime

                      if (sourceSize /= destSize) || mtimesDisagree
                        then do
                          when verbose $ putStrLn $ "  Performing copy (size differs) " ++ T.unpack (decodeUtf8OrDie sourcePath)
                          performFullCopy
                        else do
                          when verbose $ putStrLn $ "  Skipped copying " ++ T.unpack (decodeUtf8OrDie sourcePath)
                          return ()

                      when mtimesDisagree $ do
                        when verbose $ putStrLn $ "  Setting times " ++ T.unpack (decodeUtf8OrDie sourcePath)
                        setFileTimesHiResNonBlocking dest destAtime sourceMtime

              | otherwise -> do
                  die $ "Can only copy regular files, symlinks and directories, but this isn't one: " ++ T.unpack (decodeUtf8OrDie sourcePath)
            when verbose $ putStrLn $ "  done considering " ++ show sourcePath
            return ()

      (dirs, files) <- (partitionEithers <$>) $ for sourcePaths $ \sourcePath -> do
        stat <- getSymbolicLinkStatusNonBlocking sourcePath
        pure $ if isDirectory stat then Left sourcePath else Right sourcePath

      when verbose $ putStrLn $ "Syncing " ++ show (length dirs) ++ " dirs, " ++ show (length files) ++ " files"

      for_ dirs copyCreate
      _ <- pooledMapConcurrently' numJobs copyCreate files
      return ()
