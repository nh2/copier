{-# LANGUAGE ForeignFunctionInterface #-}

module InterruptibleUtimensat
  ( c_utimensat_safe
  , setFileTimesHiResNonBlocking
  ) where

import           Data.Time.Clock.POSIX (POSIXTime)
import           Foreign (Ptr)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..), CLong, CTime(..))
import           Foreign.Storable (Storable, sizeOf, alignment, peek, poke, pokeByteOff, peekByteOff)
import           Foreign.Marshal (withArray)
import           System.Posix.ByteString.FilePath (RawFilePath, withFilePath, throwErrnoPathIfMinus1Retry_)
import           System.Posix.Types (EpochTime)

#include <sys/fcntl.h>

{- Copied from System.Posix.Files.Common because that's a hidden module :( -}

data CTimeSpec = CTimeSpec EpochTime CLong

instance Storable CTimeSpec where
    sizeOf    _ = #size struct timespec
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeSpec sec nsec) = do
        (#poke struct timespec, tv_sec ) p sec
        (#poke struct timespec, tv_nsec) p nsec
    peek p = do
        sec  <- #{peek struct timespec, tv_sec } p
        nsec <- #{peek struct timespec, tv_nsec} p
        return $ CTimeSpec sec nsec

toCTimeSpec :: POSIXTime -> CTimeSpec
toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10^(9::Int) * frac)
  where
    (sec, frac) = if (frac' < 0) then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction $ toRational t

{- End copied from System.Posix.Files.Common -}


foreign import ccall safe "utimensat"
  c_utimensat_safe :: CInt -> CString -> Ptr CTimeSpec -> CInt -> IO CInt

-- | Like 'setFileTimes' but timestamps can have sub-second resolution.
--
-- Retries on @EINTR@, see https://github.com/haskell/unix/issues/86.
--
-- Note: calls @utimensat@.
setFileTimesHiResNonBlocking :: RawFilePath -> POSIXTime -> POSIXTime -> IO ()
setFileTimesHiResNonBlocking name atime mtime =
  withFilePath name $ \s ->
    withArray [toCTimeSpec atime, toCTimeSpec mtime] $ \times ->
      throwErrnoPathIfMinus1Retry_ "setFileTimesHiResNonBlocking" name $
        c_utimensat_safe (#const AT_FDCWD) s times 0
