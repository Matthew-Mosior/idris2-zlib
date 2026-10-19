module Codec.Compression.Zlib.Utilities

import Codec.Compression.Zlib.Proofs
import Control.Monad.Elin
import Control.Monad.MCancel
import Data.Buffer
import Data.Buffer.Core
import System.Posix.Timer
import System.Posix.Timer.Prim
import System.FFI
import System.Zlib.Constant
import System.Zlib.Error
import System.Zlib.Types
import System.Zlib.Raw

%default total

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

||| Grab CLOCK_MONOTONIC.
|||
export
grabMonotonicTime : Elin World [Errno] (IClock CLOCK_MONOTONIC)
grabMonotonicTime = getTime CLOCK_MONOTONIC

||| Grab CLOCK_REALTIME.
|||
export
grabRealTime : Elin World [Errno] (IClock CLOCK_REALTIME)
grabRealTime = getTime CLOCK_REALTIME

||| Converts a zlib status code to a human-readable description.
|||
export
zlibStatusMessage :  Int
                  -> String
zlibStatusMessage status =
  if status == (zlibconstantToInt Z_OK) then
    "Z_OK"
  else if status == (zlibconstantToInt Z_STREAM_END) then
    "Z_STREAM_END"
  else if status == (zlibconstantToInt Z_NEED_DICT) then
    "Z_NEED_DICT"
  else if status == (zlibconstantToInt Z_ERRNO) then
    "Z_ERRNO"
  else if status == (zlibconstantToInt Z_STREAM_ERROR) then
    "Z_STREAM_ERROR"
  else if status == (zlibconstantToInt Z_DATA_ERROR) then
    "Z_DATA_ERROR"
  else if status == (zlibconstantToInt Z_MEM_ERROR) then
    "Z_MEM_ERROR"
  else if status == (zlibconstantToInt Z_BUF_ERROR) then
    "Z_BUF_ERROR"
  else if status == (zlibconstantToInt Z_VERSION_ERROR) then
    "Z_VERSION_ERROR"
  else
    "Unknown zlib status: " ++ show status

||| Constructs a timestamped zlib error.
|||
||| If obtaining CLOCK_REALTIME itself fails, both the timer error and the
||| original zlib error are retained without timestamps.
|||
export
mkError :  String
        -> String
        -> IO (List ZlibError)
mkError location message = do
  now <- runElinIO grabRealTime
  case now of
    Right realtime =>
      pure [ MkZlibError
               location
               (MiscError message)
               (Just realtime)
           ]
    Left timererr  =>
      pure [ MkZlibError
               location
               (MiscError (show timererr))
               Nothing
           , MkZlibError
               location
               (MiscError message)
               Nothing
           ]

||| Constructs a timestamped `ZlibError` from a raw zlib status code.
|||
export
statusError :  String
            -> Int
            -> IO (List ZlibError)
statusError location status =
  mkError location (zlibStatusMessage status)

||| Copy the used prefix of a mutable output buffer into an independently
||| owned immutable buffer.
|||
export
freezeOutput :  (used : Nat)
             -> IOBuffer capacity
             -> {auto 0 bounds : LTE used capacity}
             -> IO AnyBuffer
freezeOutput used output = do
  frozen <- mbuffer used
  runIO (copy output 0 0 used {p1 = bounds} {p2 = lteReflexive used} frozen)
  pure
    ( AB used
         (unsafeMakeBuffer
         (unsafeFromMBuffer frozen))
    )
