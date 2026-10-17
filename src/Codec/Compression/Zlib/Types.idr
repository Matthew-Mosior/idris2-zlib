module Codec.Compression.Zlib.Types

import Data.Buffer.Core
import System.Zlib.Types

%default total

--------------------------------------------------------------------------------
--          Compresssor/Decompressor
--------------------------------------------------------------------------------

||| Represents an initialized zlib compression stream owned by the
||| high-level compression API.
|||
||| A `Compressor` is created only after zlib has successfully initialized
||| the underlying stream. It must therefore eventually be finalized with
||| `deflateEnd` and its native stream storage released.
|||
public export
record Compressor where
  constructor MkCompressor
  stream : ZStreamHandle

||| Represents an initialized zlib decompression stream owned by the
||| high-level decompression API.
|||
||| A `Decompressor` is created only after zlib has successfully initialized
||| the underlying stream. It must therefore eventually be finalized with
||| `inflateEnd` and its native stream storage released.
|||
public export
record Decompressor where
  constructor MkDecompressor
  stream : ZStreamHandle

||| Describes the progress made by one zlib buffer operation.
|||
||| `consumed` is the number of input bytes consumed by zlib and `produced`
||| is the number of bytes written to the supplied output buffer.
|||
public export
record BufferResult where
  constructor MkBufferResult
  status   : Int
  consumed : Nat
  produced : Nat

--------------------------------------------------------------------------------
--          Streaming
--------------------------------------------------------------------------------

||| Result of incrementally compressing input.
|||
||| `output` contains zero or more compressed chunks generated while consuming
||| the supplied input.
|||
||| `finished` is true only after zlib has returned `Z_STREAM_END`.
|||
public export
record CompressStreamResult where
  constructor MkCompressStreamResult
  output   : List AnyBuffer
  finished : Bool

||| Result of incrementally decompressing input.
|||
||| `consumed` is the number of bytes consumed from the supplied input buffer.
||| This can be less than the input size when `Z_STREAM_END` is reached, allowing
||| the caller to retain trailing bytes or begin processing another stream.
|||
||| `output` contains zero or more decompressed chunks.
|||
||| `finished` is true when zlib returned `Z_STREAM_END`.
|||
public export
record DecompressStreamResult where
  constructor MkDecompressStreamResult
  consumed : Nat
  output   : List AnyBuffer
  finished : Bool
