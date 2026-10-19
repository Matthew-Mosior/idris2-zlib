module Codec.Compression.Zlib.Decompress

import Codec.Compression.Zlib.Internal
import Codec.Compression.Zlib.Proofs
import Codec.Compression.Zlib.Types
import Codec.Compression.Zlib.Utilities
import Data.Array.Index
import Data.Buffer
import Data.Buffer.Builder
import Data.Buffer.Core
import Data.Nat
import System.Zlib.Constants
import System.Zlib.Error
import System.Zlib.Types

%default total

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

||| Number of output bytes allocated for each decompression step.
|||
||| Decompression cannot generally determine the final uncompressed size in
||| advance, so output is produced incrementally into fixed-size chunks and
||| concatenated after `Z_STREAM_END` is reached.
|||
private
inflateChunkSize : Nat
inflateChunkSize = 32768

--------------------------------------------------------------------------------
--          Decompression
--------------------------------------------------------------------------------

||| Decompress an immutable byte buffer using the requested wrapper format.
|||
||| Decompression is performed incrementally because the final uncompressed
||| size cannot generally be determined before decoding.
|||
||| Each iteration allocates a fixed-size output buffer, calls `inflate`, and
||| retains only the prefix actually produced by zlib. Input consumption is
||| tracked using the `consumed` count returned by `inflateBuffer`, allowing
||| subsequent calls to continue from the correct offset without copying or
||| slicing the compressed input.
|||
||| Output chunks are accumulated until zlib returns `Z_STREAM_END`, after
||| which they are concatenated into a single `AnyBuffer`.
|||
||| Input and output buffers cross the Chez/C boundary without intermediate
||| copies. The C shim borrows their underlying bytevectors only for the
||| duration of each `inflate` call.
|||
export
decompressWith :  {n : Nat}
               -> Wrapper
               -> IBuffer n
               -> IO (Either (List ZlibError) AnyBuffer)
decompressWith wrapper input =
  withDecompressor wrapper $ \decompressor =>
    go decompressor 0 (zeroLTE n) []
  where
    go :  Decompressor
       -> (inputoffset : Nat)
       -> (0 offsetbounds : LTE inputoffset n)
       -> List AnyBuffer
       -> IO (Either (List ZlibError) AnyBuffer)
    go decompressor inputoffset offsetbounds chunks = do
      let inputlength : Nat
          inputlength = n `minus` inputoffset
      output       <- mbuffer inflateChunkSize
      Right result <-
        inflateBuffer
          decompressor
          inputoffset
          inputlength
          input
          {inputbounds = plusMinusLTE inputoffset n offsetbounds}
          0
          inflateChunkSize
          output
          {outputbounds = zeroOffsetInBounds inflateChunkSize}
          NoFlush
        | Left errs =>
            pure (Left errs)
      let consumed'   : Nat
          consumed'   = consumed result
          produced'   : Nat
          produced'   = produced result
          nextoffset  : Nat
          nextoffset  = inputoffset + consumed'
          chunkresult : IO (Either (List ZlibError) (List AnyBuffer))
          chunkresult =
            case produced' of
              0 =>
                pure (Right chunks)
              _ =>
                case isLTE produced' inflateChunkSize of
                  No _               => do
                    errs <- mkError "Codec.Compression.Zlib.Decompress.decompressWith.go"
                                    "inflate reported producing bytes beyond the output buffer"
                    pure (Left errs)
                  Yes producedBounds => do
                    frozen <- freezeOutput produced' output {bounds = producedBounds}
                    pure (Right (frozen :: chunks))
      Right chunks' <- chunkresult
        | Left errs =>
            pure (Left errs)
      case status result == (zlibconstantToInt Z_STREAM_END) of
        True  =>
          pure
            ( Right
               (fastConcat
                 (reverse chunks'))
            )
        False =>
          case consumed' == 0 && produced' == 0 of
            True  => do
              errs <- mkError "Codec.Compression.Zlib.Decompress.decompressWith.go"
                              "inflate made no progress: no input bytes were consumed and no output bytes were produced"
              pure (Left errs)
            False =>
              case isLTE nextoffset n of
                Yes nextbounds =>
                  assert_total (go decompressor nextoffset nextbounds chunks')
                No _ => do
                  errs <- mkError "Codec.Compression.Zlib.Decompress.decompressWith.go"
                                  ( "inflate reported consuming bytes beyond the input buffer: "
                                      ++ show nextoffset
                                      ++ " > "
                                      ++ show n
                                  )
                  pure (Left errs)

--------------------------------------------------------------------------------
--          Convenience Operations
--------------------------------------------------------------------------------

||| Decompress an immutable byte buffer containing a standard zlib stream.
|||
export
decompress :  {n : Nat}
           -> IBuffer n
           -> IO (Either (List ZlibError) AnyBuffer)
decompress =
  decompressWith ZlibWrapper

||| Decompress an immutable byte buffer containing a gzip stream.
|||
export
gzipDecompress :  {n : Nat}
               -> IBuffer n
               -> IO (Either (List ZlibError) AnyBuffer)
gzipDecompress =
  decompressWith GZipWrapper

||| Decompress an immutable byte buffer containing a raw DEFLATE stream with
||| no zlib or gzip wrapper.
|||
export
rawDecompress :  {n : Nat}
              -> IBuffer n
              -> IO (Either (List ZlibError) AnyBuffer)
rawDecompress =
  decompressWith RawWrapper
