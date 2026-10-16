module Codec.Compression.Zlib.Checksum

import Data.Buffer.Core
import System.Zlib.Raw

%default total

--------------------------------------------------------------------------------
--          CRC-32
--------------------------------------------------------------------------------

||| Update an existing CRC-32 checksum with the contents of a byte buffer.
|||
||| This function can be used incrementally by passing the checksum returned
||| from one invocation as the initial checksum of the next.
|||
||| The initial CRC-32 value for a new checksum is `0`.
|||
export
crc32Update :  {n : Nat}
            -> Bits32
            -> IBuffer n
            -> IO Bits32
crc32Update crc input =
  primIO (prim__crc32 crc (unsafeGetBuffer input) (cast n))

||| Compute the CRC-32 checksum of an immutable byte buffer.
|||
||| This is equivalent to calling `crc32Update` with zlib's initial CRC-32
||| value of `0`.
|||
export
crc32 :  {n : Nat}
      -> IBuffer n
      -> IO Bits32
crc32 =
  crc32Update 0

--------------------------------------------------------------------------------
--          Adler-32
--------------------------------------------------------------------------------

||| Update an existing Adler-32 checksum with the contents of a byte buffer.
|||
||| This function can be used incrementally by passing the checksum returned
||| from one invocation as the initial checksum of the next.
|||
||| The initial Adler-32 value for a new checksum is `1`.
|||
export
adler32Update :  {n : Nat}
              -> Bits32
              -> IBuffer n
              -> IO Bits32
adler32Update adler input =
  primIO (prim__adler32 adler (unsafeGetBuffer input) (cast n))

||| Compute the Adler-32 checksum of an immutable byte buffer.
|||
||| This is equivalent to calling `adler32Update` with zlib's initial
||| Adler-32 value of `1`.
|||
export
adler32 :  {n : Nat}
        -> IBuffer n
        -> IO Bits32
adler32 =
  adler32Update 1
