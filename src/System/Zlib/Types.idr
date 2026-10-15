module System.Zlib.Types

||| An opaque handle to a zlib `z_stream` structure.
|||
||| The structure is allocated and managed by the zlib implementation and is never inspected directly by Idris code.
|||
||| The contained pointer is passed to the low-level zlib FFI functions.
|||
public export
record ZStreamHandle where
  constructor MkZStreamHandle
  ptr : AnyPtr

||| Represents the compression level supplied to zlib.
|||
||| `NoCompression`, `BestSpeed`, `BestCompression`, and `DefaultCompression` correspond to zlib's predefined compression levels.
|||
||| `CompressionLevel` allows an explicit zlib compression level to be supplied.
|||
public export
data ZlibCompressionLevel
  = NoCompression
  | BestSpeed
  | BestCompression
  | DefaultCompression
  | CompressionLevel Int

||| Converts an Idris compression-level value to the integer expected by the zlib compression API.
|||
export
zlibCompressionLevelValue : ZlibCompressionLevel -> Int
zlibCompressionLevelValue NoCompression = 0
zlibCompressionLevelValue BestSpeed = 1
zlibCompressionLevelValue BestCompression = 9
zlibCompressionLevelValue DefaultCompression = (-1)
zlibCompressionLevelValue (CompressionLevel n) = n

||| Specifies when zlib should flush pending compressed output.
|||
public export
data FlushMode
  = NoFlush
  | PartialFlush
  | SyncFlush
  | FullFlush
  | Finish
  | Block

||| Converts an Idris flush mode to the integer constant expected by the zlib compression API.
|||
export
flushModeValue : FlushMode -> Int
flushModeValue NoFlush = 0
flushModeValue PartialFlush = 1
flushModeValue SyncFlush = 2
flushModeValue FullFlush = 3
flushModeValue Finish = 4
flushModeValue Block = 5

||| Specifies the wrapper format used around a DEFLATE stream.
|||
||| `ZlibWrapper` selects the standard zlib format.
|||
||| `GZipWrapper` selects the gzip format.
|||
||| `RawWrapper` selects a raw DEFLATE stream without a zlib or gzip wrapper.
|||
public export
data Wrapper
  = ZlibWrapper
  | GZipWrapper
  | RawWrapper

||| The default zlib window size used by the standard zlib wrapper.
|||
export
defaultWindowBits : Int
defaultWindowBits = 15

||| Converts a wrapper format to the corresponding `windowBits` value required by zlib's `deflateInit2` and `inflateInit2` functions.
|||
export
windowBits : Wrapper -> Int
windowBits ZlibWrapper = 15
windowBits GZipWrapper = 31
windowBits RawWrapper = (-15)

||| Specifies the compression strategy used by zlib.
|||
public export
data Strategy
  = DefaultStrategy
  | Filtered
  | HuffmanOnly
  | RLE
  | Fixed

||| Converts an Idris compression strategy to the integer constant expected by zlib.
|||
export
strategyValue : Strategy -> Int
strategyValue DefaultStrategy = 0
strategyValue Filtered = 1
strategyValue HuffmanOnly = 2
strategyValue RLE = 3
strategyValue Fixed = 4
