module Codec.Compression.Zlib.Compress

import Codec.Compression.Zlib.Internal
import Codec.Compression.Zlib.Types
import Data.Buffer
import Data.Buffer.Core
import Data.Linear.Token
import System.Zlib.Constants
import System.Zlib.Error
import System.Zlib.Types

%default total

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

||| Wrap the used prefix of a mutable output buffer in an immutable buffer
||| without copying.
|||
||| The mutable buffer must not be used again after this function is called.
||| This function is safe at its call sites in this module because the output
||| buffer is freshly allocated by `compressWith`, never escapes the function,
||| and is discarded immediately after being wrapped.
|||
freezeOutput :  (used : Nat)
             -> IOBuffer capacity
             -> AnyBuffer
freezeOutput used output =
  AB used (unsafeMakeBuffer (unsafeFromMBuffer output))

||| Every natural number is less than or equal to itself.
|||
lteReflexive :  (n : Nat)
             -> LTE n n
lteReflexive 0     = LTEZero
lteReflexive (S n) = LTESucc (lteReflexive n)

||| Proof that a full buffer range beginning at offset zero is in bounds.
|||
zeroOffsetInBounds :  (n : Nat)
                   -> LTE (0 + n) n
zeroOffsetInBounds n =
  lteReflexive n

--------------------------------------------------------------------------------
--          Compression
--------------------------------------------------------------------------------

||| Compress an immutable byte buffer using the requested compression level
||| and wrapper format.
|||
||| The compressor is acquired and released using `withCompressor`, ensuring
||| that the underlying zlib stream is finalized and freed even if compression
||| fails.
|||
||| The output buffer is allocated once using `deflateBound`, which computes
||| sufficient capacity for compressing the complete input with `Finish`.
||| Compression therefore requires a single call to `deflateBuffer`.
|||
||| Input and output bytes are passed to zlib without intermediate copies.
||| On the Chez backend, their underlying bytevectors are borrowed directly by
||| the C shim for the duration of the foreign call.
|||
||| The returned `AnyBuffer` contains exactly the number of bytes produced by
||| zlib, although its underlying allocation may be larger because the used
||| prefix is wrapped without copying.
|||
||| Inputs too large to be represented by zlib's one-shot `avail_in` /
||| `avail_out` interface are rejected. Large-input support is provided by the
||| incremental compression API rather than by this one-shot operation.
|||
export
compressWith :  {n : Nat}
             -> ZlibCompressionLevel
             -> Wrapper
             -> IBuffer n
             -> IO (Either (List ZlibError) AnyBuffer)
compressWith level wrapper input =
  withCompressor level wrapper $ \compressor => do
    Right capacity <- deflateBound compressor n
      | Left errs =>
          pure (Left errs)
    output       <- mbuffer capacity
    Right result <-
      deflateBuffer
        compressor
        0
        n
        input
        {inputbounds = zeroOffsetInBounds n}
        0
        capacity
        output
        {outputbounds = zeroOffsetInBounds capacity}
        Finish
      | Left errs =>
          pure (Left errs)
    case status result == Z_STREAM_END of
      False => do
        errs <- mkError "Codec.Compression.Zlib.Compress.compressWith"
                        ( "Expected Z_STREAM_END after single-pass compression, but zlib returned "
                            ++ show (status result)
                        )
        pure (Left errs)
      True =>
        case consumed result == n of
          False => do
            errs <- mkError "Codec.Compression.Zlib.Compress.compressWith"
                            ( "zlib completed compression without consuming the complete input: "
                                ++ show (consumed result)
                                ++ " of "
                                ++ show n
                                ++ " bytes consumed"
                            )
            pure (Left errs)
          True =>
            pure
              ( Right
                 ( freezeOutput
                   (produced result)
                   output
                 )
              )

--------------------------------------------------------------------------------
--          Convenience Operations
--------------------------------------------------------------------------------

||| Compress an immutable byte buffer using the standard zlib wrapper and
||| zlib's default compression level.
|||
export
compress :  {n : Nat}
         -> IBuffer n
         -> IO (Either (List ZlibError) AnyBuffer)
compress =
  compressWith DefaultCompression ZlibWrapper

||| Compress an immutable byte buffer using the gzip wrapper and zlib's
||| default compression level.
|||
export
gzipCompress :  {n : Nat}
             -> IBuffer n
             -> IO (Either (List ZlibError) AnyBuffer)
gzipCompress =
  compressWith DefaultCompression GZipWrapper

||| Compress an immutable byte buffer as a raw DEFLATE stream with no zlib or
||| gzip wrapper, using zlib's default compression level.
|||
export
rawCompress :  {n : Nat}
            -> IBuffer n
            -> IO (Either (List ZlibError) AnyBuffer)
rawCompress =
  compressWith DefaultCompression RawWrapper
