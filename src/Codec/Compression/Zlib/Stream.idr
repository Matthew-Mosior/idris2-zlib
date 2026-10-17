module Codec.Compression.Zlib.Stream

import Codec.Compression.Zlib.Internal
import Codec.Compression.Zlib.Proofs
import Codec.Compression.Zlib.Types
import Codec.Compression.Zlib.Utilities
import Data.Buffer
import Data.Buffer.Core
import Data.Buffer.Indexed
import Data.Linear.Token
import Data.Nat
import System.Zlib.Constants
import System.Zlib.Error
import System.Zlib.Types

%default total

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

||| Number of bytes allocated for each streaming output chunk.
|||
||| A fixed-size output buffer prevents repeated growth and copying while still
||| allowing arbitrarily large streams to be processed incrementally.
|||
private
streamChunkSize : Nat
streamChunkSize = 32768

--------------------------------------------------------------------------------
--          Stream Handles
--------------------------------------------------------------------------------

||| An initialized incremental compression stream.
|||
||| Values of this type are valid only for the duration of
||| `withStreamingCompressor`.
|||
export
record StreamingCompressor where
  constructor MkStreamingCompressor
  compressor : Compressor

||| An initialized incremental decompression stream.
|||
||| Values of this type are valid only for the duration of
||| `withStreamingDecompressor`.
|||
export
record StreamingDecompressor where
  constructor MkStreamingDecompressor
  decompressor : Decompressor

--------------------------------------------------------------------------------
--          Buffer Utilities
--------------------------------------------------------------------------------

||| A non-empty backing buffer used when invoking zlib with zero bytes of input.
|||
||| Keeping a valid backing bytevector avoids relying on the FFI representation
||| of a zero-length buffer when `avail_in` is zero.
|||
private
dummyInput : IBuffer 1
dummyInput =
  bufferL [0]

private
prependProduced :  {capacity : Nat}
                -> String
                -> (produced : Nat)
                -> IOBuffer capacity
                -> List AnyBuffer
                -> IO (Either (List ZlibError) (List AnyBuffer))
prependProduced {capacity} location produced output chunks =
  case produced of
    0 =>
      pure (Right chunks)
    _ =>
      case isLTE produced capacity of
        No _               => do
          errs <- mkError location
                          "zlib reported producing bytes beyond the output buffer"
          pure (Left errs)
        Yes producedBounds => do
          frozen <- freezeOutput produced output {bounds = producedBounds}
          pure (Right (frozen :: chunks))

--------------------------------------------------------------------------------
--          Resource Management
--------------------------------------------------------------------------------

||| Run an action with an initialized streaming compressor.
|||
||| The underlying zlib stream is finalized and freed when the action returns.
|||
export
withStreamingCompressor :  ZlibCompressionLevel
                        -> Wrapper
                        -> (StreamingCompressor -> IO (Either (List ZlibError) a))
                        -> IO (Either (List ZlibError) a)
withStreamingCompressor level wrapper action =
  withCompressor level wrapper $ \compressor =>
    action (MkStreamingCompressor compressor)

||| Run an action with an initialized streaming decompressor.
|||
||| The underlying zlib stream is finalized and freed when the action returns.
|||
export
withStreamingDecompressor :  Wrapper
                          -> (StreamingDecompressor -> IO (Either (List ZlibError) a))
                          -> IO (Either (List ZlibError) a)
withStreamingDecompressor wrapper action =
  withDecompressor wrapper $ \decompressor =>
    action (MkStreamingDecompressor decompressor)

--------------------------------------------------------------------------------
--          Streaming Compression
--------------------------------------------------------------------------------

private
compressInput :  {n : Nat}
              -> StreamingCompressor
              -> FlushMode
              -> IBuffer n
              -> IO (Either (List ZlibError) CompressStreamResult)
compressInput (MkStreamingCompressor compressor) flush input =
  go 0 (zeroLTE n) []
  where
    mutual
      continue :  Nat
               -> Nat
               -> Nat
               -> Nat
               -> Nat
               -> List AnyBuffer
               -> IO (Either (List ZlibError) CompressStreamResult)
      continue inputoffset inputlength consumed' produced' nextoffset chunks' =
          case isLTE nextoffset n of
            No _ => do
              errs <- mkError "Codec.Compression.Zlib.Stream.compressInput.continue"
                              "deflate reported consuming bytes beyond the supplied input"
              pure (Left errs)
            Yes nextbounds =>
              case nextoffset < n of
                True =>
                  assert_total (go nextoffset nextbounds chunks')
                False =>
                  -- All input has been consumed.  If the output buffer was
                  -- completely filled, zlib may still have pending output.
                  case produced' == streamChunkSize of
                    True =>
                      assert_total (go nextoffset nextbounds chunks')
                    False =>
                      case flush == Finish of
                        True => do
                          errs <- mkError "Codec.Compression.Zlib.Stream.compressInput.continue"
                                          "deflate did not reach Z_STREAM_END while finishing"
                          pure (Left errs)
                        False =>
                          pure
                            ( Right
                                ( MkCompressStreamResult
                                    nextoffset
                                    (reverse chunks')
                                    False
                                )
                            )
      go :  (inputoffset : Nat)
         -> (0 offsetbounds : LTE inputoffset n)
         -> List AnyBuffer
         -> IO (Either (List ZlibError) CompressStreamResult)
      go inputoffset offsetbounds chunks = do
        let inputlength : Nat
            inputlength = n `minus` inputoffset
        output         <- mbuffer streamChunkSize
        Right result   <-
          deflateBuffer
            compressor
            inputoffset
            inputlength
            input
            {inputbounds = plusMinusLTE inputoffset n offsetbounds}
            0
            streamChunkSize
            output
            {outputbounds = zeroOffsetInBounds streamChunkSize}
            flush
          | Left errs =>
              pure (Left errs)
        let consumed'  : Nat
            consumed'  = consumed result
            produced'  : Nat
            produced'  = produced result
            nextoffset : Nat
            nextoffset = inputoffset + consumed'
        Right chunks' <-
          prependProduced
            "Codec.Compression.Zlib.Stream.compressInput.go"
            produced'
            output
            chunks
          | Left errs =>
              pure (Left errs)
        case status result of
          1          =>
            pure
              ( Right
                  ( MkCompressStreamResult
                      nextoffset
                      (reverse chunks')
                      True
                  )
              )
          0          =>
            continue inputoffset inputlength consumed' produced' nextoffset chunks'
          (-5)       =>
            case consumed' == 0 && produced' == 0 of
              True =>
                case flush == Finish of
                  True => do
                    errs <- mkError "Codec.Compression.Zlib.Stream.compressInput.go"
                                    "deflate made no progress while finishing the stream"
                    pure (Left errs)
                  False =>
                    pure
                      ( Right
                          ( MkCompressStreamResult
                              nextoffset
                              (reverse chunks')
                              False
                          )
                      )
              False =>
                continue inputoffset inputlength consumed' produced' nextoffset chunks'
          unexpected => do
            errs <- mkError "Codec.Compression.Zlib.Stream.compressInput.go"
                            ( "Unexpected deflate status: "
                                ++ show unexpected 
                            )
            pure (Left errs)

||| Feed a chunk of uncompressed data into an incremental compressor.
|||
||| The input is compressed using `Z_NO_FLUSH`, allowing zlib to preserve
||| compression effectiveness across chunk boundaries.
|||
||| Zero or more compressed output chunks may be returned.
|||
export
compressChunk :  {n : Nat}
              -> StreamingCompressor
              -> IBuffer n
              -> IO (Either (List ZlibError) CompressStreamResult)
compressChunk stream input =
  compressInput stream NoFlush input

||| Feed input into an incremental compressor using an explicit flush mode.
|||
||| Frequent flushing can reduce compression effectiveness. `NoFlush` should
||| normally be used for ordinary input chunks.
|||
export
compressChunkWith :  {n : Nat}
                  -> FlushMode
                  -> StreamingCompressor
                  -> IBuffer n
                  -> IO (Either (List ZlibError) CompressStreamResult)
compressChunkWith flush stream input =
  compressInput stream flush input

||| Finish an incremental compression stream and return all remaining output.
|||
||| After this function successfully returns `finished = True`, the compressor
||| must not be used again unless its underlying zlib state is reset.
|||
export
finishCompressor :  StreamingCompressor
                 -> IO (Either (List ZlibError) CompressStreamResult)
finishCompressor (MkStreamingCompressor compressor) =
  finish []
  where
    finish :  List AnyBuffer
           -> IO (Either (List ZlibError) CompressStreamResult)
    finish chunks = do
      output        <- mbuffer streamChunkSize
      Right result  <-
        deflateBuffer
          compressor
          0
          0
          dummyInput
          {inputbounds = LTEZero}
          0
          streamChunkSize
          output
          {outputbounds = zeroOffsetInBounds streamChunkSize}
          Finish
        | Left errs =>
            pure (Left errs)
      let produced' : Nat
          produced' = produced result
      Right chunks' <-
        prependProduced
          "Codec.Compression.Zlib.Stream.finishCompressor.finish"
          produced'
          output
          chunks
        | Left errs =>
            pure (Left errs)
      case status result of
        1          =>
          pure
            ( Right
                ( MkCompressStreamResult
                    0
                    (reverse chunks')
                    True
                )
            )
        0          =>
          assert_total (finish chunks')
        (-5)       =>
          case produced' of
            0 => do
              errs <- mkError "Codec.Compression.Zlib.Stream.finishCompressor.finish"
                              "deflate made no progress while finishing the stream"
              pure (Left errs)
            _ =>
              assert_total (finish chunks')
        unexpected => do
          errs <- mkError "Codec.Compression.Zlib.Stream.finishCompressor.finish"
                          ( "Unexpected deflate status while finishing: "
                              ++ show unexpected
                          )
          pure (Left errs)

--------------------------------------------------------------------------------
--          Streaming Decompression
--------------------------------------------------------------------------------

||| Feed a compressed input chunk into an incremental decompressor.
|||
||| The function processes as much input as possible and returns zero or more
||| decompressed output chunks.
|||
||| `consumed` indicates exactly how many bytes from the supplied input were
||| consumed. If `finished` is true and `consumed` is smaller than the input
||| size, the remaining bytes follow the compressed stream and are left for
||| the caller.
|||
export
decompressChunk :  {n : Nat}
                -> StreamingDecompressor
                -> IBuffer n
                -> IO (Either (List ZlibError) DecompressStreamResult)
decompressChunk (MkStreamingDecompressor decompressor) input =
  go 0 (zeroLTE n) []
  where
    mutual
      continue :  Nat
               -> Nat
               -> Nat
               -> List AnyBuffer
               -> IO (Either (List ZlibError) DecompressStreamResult)
      continue consumed' produced' nextoffset chunks' =
        case isLTE nextoffset n of
          No _           => do
            errs <- mkError "Codec.Compression.Zlib.Stream.decompressChunk.continue"
                            "inflate reported consuming bytes beyond the supplied input"
            pure (Left errs)
          Yes nextbounds =>
            case nextoffset < n of
              True  =>
                assert_total (go nextoffset nextbounds chunks')
              False =>
                -- All currently supplied input has been consumed.  If the
                -- output buffer filled completely, inflate may still have
                -- output pending that can be emitted without additional
                -- input.
                case produced' == streamChunkSize of
                  True  =>
                    assert_total (go nextoffset nextbounds chunks')
                  False =>
                    pure
                      ( Right
                          ( MkDecompressStreamResult
                              nextoffset
                              (reverse chunks')
                              False
                          )
                      )
      go :  (inputoffset : Nat)
         -> (0 offsetbounds : LTE inputoffset n)
         -> List AnyBuffer
         -> IO (Either (List ZlibError) DecompressStreamResult)
      go inputoffset offsetbounds chunks = do
        let inputlength : Nat
            inputlength = n `minus` inputoffset
        output          <- mbuffer streamChunkSize
        Right result    <-
          inflateBuffer
            decompressor
            inputoffset
            inputlength
            input
            {inputbounds = plusMinusLTE inputoffset n offsetbounds}
            0
            streamChunkSize
            output
            {outputbounds = zeroOffsetInBounds streamChunkSize}
            NoFlush
          | Left errs =>
              pure (Left errs)
        let consumed'  : Nat
            consumed'  = consumed result
            produced'  : Nat
            produced'  = produced result
            nextoffset : Nat
            nextoffset = inputoffset + consumed'
        Right chunks'  <-
          prependProduced
            "Codec.Compression.Zlib.Stream.decompressChunk.go"
            produced'
            output
            chunks
          | Left errs =>
              pure (Left errs)
        case status result of
          1          =>
            pure
              ( Right
                  ( MkDecompressStreamResult
                      nextoffset
                      (reverse chunks')
                      True
                  )
              )
          0          =>
            continue consumed' produced' nextoffset chunks'
          (-5)       =>
            case consumed' == 0 && produced' == 0 of
              True =>
                -- No progress with the current input is not an error for an
                -- incremental decompressor.  The caller can supply more bytes.
                pure
                  ( Right
                      ( MkDecompressStreamResult
                          nextoffset
                          (reverse chunks')
                          False
                      )
                  )
              False =>
                continue consumed' produced' nextoffset chunks'
          unexpected => do
            errs <- mkError "Codec.Compression.Zlib.Stream.decompressChunk.go"
                            ( "Unexpected inflate status: "
                                ++ show unexpected
                            )
            pure (Left errs)
