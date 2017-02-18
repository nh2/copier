{-# LANGUAGE ForeignFunctionInterface #-}

module SafeFFIStat
  ( c_stat_safe
  , c_lstat_safe
  , getFileStatusNonBlocking
  , getSymbolicLinkStatusNonBlocking
  ) where

import           Foreign (Ptr, withForeignPtr, mallocForeignPtrBytes)
import           Foreign.C.Types (CInt(..))
import           Foreign.C.String (CString)
import           System.Posix.ByteString.FilePath (RawFilePath, withFilePath, throwErrnoPathIfMinus1_, throwErrnoPathIfMinus1Retry_)
import           System.Posix.Files.ByteString (FileStatus)
import           System.Posix.Internals (CStat)
import           Unsafe.Coerce (unsafeCoerce)

#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>

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
