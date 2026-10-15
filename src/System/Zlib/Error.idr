module System.Zlib.Error

import System.Posix.Timer
import System.Posix.Timer.Prim

||| Represents the possible errors reported by the zlib compression or decompression API.
|||
public export
data ZlibErr
  = StreamError
  | DataError
  | MemoryError
  | BufferError
  | VersionError
  | NeedDictionary
  | ErrnoError
  | UnknownError Int
  | MiscError String

||| Converts a zlib return code to the corresponding typed error.
|||
||| Returns `Nothing` for return codes that do not represent a zlib error, including `Z_OK` and `Z_STREAM_END`.
|||
export
zlibError :  Int
          -> Maybe ZlibErr
zlibError 2    = Just NeedDictionary
zlibError (-1) = Just ErrnoError
zlibError (-2) = Just StreamError
zlibError (-3) = Just DataError
zlibError (-4) = Just MemoryError
zlibError (-5) = Just BufferError
zlibError (-6) = Just VersionError
zlibError _    = Nothing

||| A wrapper around `ZlibErr`.
|||
||| Fields:
||| - `fnname`                : The function the error originated from
||| - `errormessage`          : The error formatted as a String
||| - `errormessagetimestamp` : The timestamp the error occurred at
|||
public export
record ZlibError where
  constructor MkZlibError
  fnname                : String
  error                 : ZlibErr
  errormessagetimestamp : Maybe (IClock CLOCK_REALTIME)
