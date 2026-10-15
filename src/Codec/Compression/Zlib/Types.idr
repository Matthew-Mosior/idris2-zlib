module Codec.Compression.Zlib.Types

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
