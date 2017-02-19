{-# LANGUAGE ForeignFunctionInterface #-}

module Sendfile
  ( copyFileSendfile
  , copyFileSendfileCopyCreateMode
  ) where

import           Control.Exception (bracket)
import           Control.Monad (when)
import           Foreign
import           Foreign.C.Error
import           Foreign.C.Types
import           System.Posix.Files.ByteString
import           System.Posix.IO.ByteString
import           System.Posix.ByteString.FilePath
import           System.Posix.Types

import           SafeFileFFI (openFdNonBlocking, closeFdNonBlocking)

#define _LARGEFILE64_SOURCE 1
#include <sys/types.h>
#include <stdio.h>
#include <sys/sendfile.h>


-- sendfile64 gives LFS support
foreign import ccall unsafe "sendfile64" c_sendfile64
    :: Fd -> Fd -> Ptr (#type off64_t) -> (#type size_t) -> IO (#type ssize_t)


-- | See `openFd`; the actual implementation uses
-- `openFdNonBlocking` and `closeFdNonBlocking`.
withFileAsFd :: RawFilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> (Fd -> IO r) -> IO r
withFileAsFd name mode createMode flags =
  bracket (openFdNonBlocking name mode createMode flags) closeFdNonBlocking


-- | Copies a file using the `sendfile()` system call.
--
-- Consequently, it resolves symlinks.
--
-- Preserves the mode (permissions) of the source file (if the target file
-- doesn't exist and is created), but doesn't preserve ownership.
copyFileSendfile :: RawFilePath -> Maybe FileStatus -> RawFilePath -> FileMode -> IO ()
copyFileSendfile fromFilePath m'fileStatus toFilePath toFileCreateMode =  do
  -- Note: We don't use `Handle`s here, because we need FDs, and `handleToFd`
  -- leaks the FD unless we close it manually (which doesn't go well with
  -- exception safety, so we have `withFileAsFd` instead).
  withFileAsFd fromFilePath ReadOnly Nothing defaultFileFlags $ \in_fd -> do
    fileSizeBytes <- fromIntegral . fileSize <$> case m'fileStatus of
      Just status -> pure status
      Nothing -> getFileStatus fromFilePath
    withFileAsFd toFilePath WriteOnly (Just toFileCreateMode) defaultFileFlags{ trunc = True } $ \out_fd -> do
      let loop :: Int -> IO ()
          loop bytesWritten = do
            copied <- throwErrnoIfMinus1Retry "sendfile64" $ c_sendfile64 out_fd in_fd nullPtr (fromIntegral fileSizeBytes)
            let bytesWrittenNew = bytesWritten + fromIntegral copied
            when (bytesWrittenNew < fileSizeBytes) $
              loop bytesWrittenNew
      loop 0


-- | Like `copyFileSendfile`, but creates the file with the user's umask.
copyFileSendfileCopyCreateMode :: RawFilePath -> RawFilePath -> IO ()
copyFileSendfileCopyCreateMode fromFilePath toFilePath = do
  fromFileStatus <- getFileStatus fromFilePath
  let toFileCreateMode = fileMode fromFileStatus
  copyFileSendfile fromFilePath (Just fromFileStatus) toFilePath toFileCreateMode
