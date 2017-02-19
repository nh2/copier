{-# LANGUAGE ForeignFunctionInterface #-}

module SafeFileFFI
  ( c_open_safe
  , c_close_safe
  , c_stat_safe
  , c_lstat_safe
  , openFdNonBlocking
  , closeFdNonBlocking
  , getFileStatusNonBlocking
  , getSymbolicLinkStatusNonBlocking
  ) where

import           Data.Bits ((.|.))
import           Foreign (Ptr, withForeignPtr, mallocForeignPtrBytes)
import           Foreign.C.Error (throwErrnoIfMinus1_)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           System.Posix.ByteString.FilePath (RawFilePath, withFilePath, throwErrnoPathIfMinus1_, throwErrnoPathIfMinus1Retry_)
import           System.Posix.ByteString.FilePath (throwErrnoPathIfMinus1Retry)
import           System.Posix.Files.ByteString (FileStatus)
import           System.Posix.Internals (CStat)
import           System.Posix.IO (OpenMode(..))
import           System.Posix.IO.ByteString (OpenFileFlags(..))
import           System.Posix.Types (Fd(..), CMode(..), FileMode)
import           Unsafe.Coerce (unsafeCoerce)

#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>

foreign import ccall safe "open"
  c_open_safe :: CString -> CInt -> CMode -> IO CInt

-- | Like `open_` from `unix` "System.Posix.IO.Common", but with a
-- `safe` FFI call.
open_  :: CString
       -> OpenMode
       -> Maybe FileMode -- ^Just x => creates the file with the given modes, Nothing => the file must exist.
       -> OpenFileFlags
       -> IO Fd
open_ str how maybe_mode (OpenFileFlags appendFlag exclusiveFlag nocttyFlag
                                nonBlockFlag truncateFlag) = do
    fd <- c_open_safe str all_flags mode_w
    return (Fd fd)
  where
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if appendFlag    then (#const O_APPEND)   else 0) .|.
       (if exclusiveFlag then (#const O_EXCL)     else 0) .|.
       (if nocttyFlag    then (#const O_NOCTTY)   else 0) .|.
       (if nonBlockFlag  then (#const O_NONBLOCK) else 0) .|.
       (if truncateFlag  then (#const O_TRUNC)    else 0)

    (creat, mode_w) = case maybe_mode of
                        Nothing -> (0,0)
                        Just x  -> ((#const O_CREAT), x)

    open_mode = case how of
                   ReadOnly  -> (#const O_RDONLY)
                   WriteOnly -> (#const O_WRONLY)
                   ReadWrite -> (#const O_RDWR)

-- |Close this file descriptor.  May throw an exception if this is an
-- invalid descriptor.

closeFdNonBlocking :: Fd -> IO ()
closeFdNonBlocking (Fd fd) = throwErrnoIfMinus1_ "closeFdNonBlocking" (c_close_safe fd)

foreign import ccall safe "close"
  c_close_safe :: CInt -> IO CInt


-- | Like `openFd` but using a `safe` FFI call so that it doesn't
-- block when the open takes long (which happens on network file systems).
--
-- See https://ghc.haskell.org/trac/ghc/ticket/13296
openFdNonBlocking
  :: RawFilePath
  -> OpenMode
  -> Maybe FileMode -- ^Just x => creates the file with the given modes, Nothing => the file must exist.
  -> OpenFileFlags
  -> IO Fd
openFdNonBlocking name how maybe_mode flags = do
   withFilePath name $ \str -> do
     throwErrnoPathIfMinus1Retry "openFdNonBlocking" name $
       open_ str how maybe_mode flags


foreign import ccall safe "stat"
  c_stat_safe :: CString -> Ptr CStat -> IO CInt

-- | @getFileStatusNonBlocking path@ calls gets the @FileStatus@ information (user ID,
-- size, access times, etc.) for the file @path@.
--
-- Note: calls @stat@ using a @safe@ FFI call.
getFileStatusNonBlocking :: RawFilePath -> IO FileStatus
getFileStatusNonBlocking path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat))
  withForeignPtr fp $ \p ->
    withFilePath path $ \s ->
      throwErrnoPathIfMinus1Retry_ "getFileStatusNonBlocking" path (c_stat_safe s p)
  -- The `FileStatus` constructor is not exposed from
  -- `System.Posix.Files.ByteString`, so I have to use
  -- `unsafeCoerce` here, sorry.
  -- TODO: Change this when it's exposed:
  --   https://github.com/haskell/unix/issues/87
  return (unsafeCoerce {- FileStatus -} fp)

foreign import ccall safe "lstat"
  c_lstat_safe :: CString -> Ptr CStat -> IO CInt

-- | Acts as 'getFileStatusNonBlocking' except when the 'RawFilePath' refers to a symbolic
-- link. In that case the @FileStatus@ information of the symbolic link itself
-- is returned instead of that of the file it points to.
--
-- Note: calls @lstat@.
getSymbolicLinkStatusNonBlocking :: RawFilePath -> IO FileStatus
getSymbolicLinkStatusNonBlocking path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat))
  withForeignPtr fp $ \p ->
    withFilePath path $ \s ->
      throwErrnoPathIfMinus1_ "getSymbolicLinkStatusNonBlocking" path (c_lstat_safe s p)
  -- The `FileStatus` constructor is not exposed from
  -- `System.Posix.Files.ByteString`, so I have to use
  -- `unsafeCoerce` here, sorry.
  -- TODO: Change this when it's exposed:
  --   https://github.com/haskell/unix/issues/87
  return (unsafeCoerce {- FileStatus -} fp)
