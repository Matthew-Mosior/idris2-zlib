module Codec.Compression.Zlib.Internal

import Codec.Compression.Zlib.Types
import Codec.Compression.Zlib.Utilities
import Control.Monad.Elin
import Control.Monad.MCancel
import Data.Buffer
import Data.Buffer.Core
import Data.Linear.Token
import System.Posix.Timer
import System.Posix.Timer.Prim
import System.FFI
import System.Zlib.Constants
import System.Zlib.Error
import System.Zlib.Types
import System.Zlib.Raw

%default total

--------------------------------------------------------------------------------
--          Stream Lifecycle
--------------------------------------------------------------------------------

||| Allocate and initialize a zlib compression stream.
|||
||| If initialization fails, the native stream allocation is released before
||| returning the error. A successful result therefore represents a fully
||| initialized stream that must later be destroyed with `destroyCompressor`.
|||
export
createCompressor :  ZlibCompressionLevel
                 -> Wrapper
                 -> IO (Either (List ZlibError) Compressor)
createCompressor level wrapper = do
  ptr    <- primIO prim__newStream
  status <- primIO ( prim__deflateInit2
                     ptr
                     (zlibCompressionLevelValue level)
                     (zlibconstantToInt Z_DEFLATED)
                     (windowBits wrapper)
                     (zlibconstantToInt Z_DEFAULT_MEM_LEVEL)
                     (strategyValue DefaultStrategy)
                   )
  case status == (zlibconstantToInt Z_OK) of
    True  =>
      pure (Right (MkCompressor (MkZStreamHandle ptr)))
    False => do
      -- deflateInit2 did not successfully initialize the stream, so there is
      -- no initialized zlib state to finalize with deflateEnd. We only release
      -- the allocation owned by our shim.
      primIO (prim__freeStream ptr)
      err <- statusError "Codec.Compression.Zlib.Internal.createCompressor" status
      pure (Left err)

||| Finalize and release a zlib compression stream.
|||
||| `deflateEnd` releases resources owned internally by zlib, after which
||| `prim__freeStream` releases the opaque stream allocation owned by the
||| C shim.
|||
||| This function is intended primarily as the release action used by
||| `withCompressor`.
|||
export
destroyCompressor :  Compressor
                  -> IO ()
destroyCompressor (MkCompressor (MkZStreamHandle ptr)) = do
  _ <- primIO $ prim__deflateEnd ptr
  primIO $ prim__freeStream ptr

||| Allocate and initialize a zlib decompression stream.
|||
||| If initialization fails, the native stream allocation is released before
||| returning the error. A successful result therefore represents a fully
||| initialized stream that must later be destroyed with
||| `destroyDecompressor`.
|||
export
createDecompressor :  Wrapper
                   -> IO (Either (List ZlibError) Decompressor)
createDecompressor wrapper = do
  ptr    <- primIO prim__newStream
  status <- primIO (prim__inflateInit2 ptr (windowBits wrapper))
  case status == (zlibconstantToInt Z_OK) of
    True  =>
      pure (Right (MkDecompressor (MkZStreamHandle ptr)))
    False => do
      -- inflateInit2 did not successfully initialize the stream, so only the
      -- shim-owned stream allocation has to be released.
      primIO (prim__freeStream ptr)
      err <- statusError "Codec.Compression.Zlib.Internal.createDecompressor" status
      pure (Left err)

||| Finalize and release a zlib decompression stream.
|||
||| `inflateEnd` releases resources owned internally by zlib, after which
||| `prim__freeStream` releases the opaque stream allocation owned by the
||| C shim.
|||
||| This function is intended primarily as the release action used by
||| `withDecompressor`.
|||
export
destroyDecompressor :  Decompressor
                    -> IO ()
destroyDecompressor (MkDecompressor (MkZStreamHandle ptr)) = do
  _ <- primIO $ prim__inflateEnd ptr
  primIO $ prim__freeStream ptr

--------------------------------------------------------------------------------
--          Safe Resource Combinators
--------------------------------------------------------------------------------

||| Execute an action with a freshly initialized zlib compression stream.
|||
||| The compressor is finalized and released regardless of how the protected
||| action exits. Failure to initialize the compressor is returned without
||| executing the supplied action.
|||
export
withCompressor :  ZlibCompressionLevel
               -> Wrapper
               -> (Compressor -> IO (Either (List ZlibError) a))
               -> IO (Either (List ZlibError) a)
withCompressor level wrapper action = do
  result <- runElinIO withCompressor'
  case result of
    Left err    => do
      errs <- mkError "Codec.Compression.Zlib.Internal.withCompressor" "\{show err}"
      pure (Left errs)
    Right value =>
      case value of
        Left errs    =>
          pure (Left errs)
        Right value' =>
          pure (Right value')
  where
    acquire : F1 World (Either (List ZlibError) Compressor)
    acquire =
      ioToF1 (createCompressor level wrapper)
    use :  Either (List ZlibError) Compressor
        -> F1 World (Either (List ZlibError) a)
    use (Left err)         =
      ioToF1 (pure (Left err))
    use (Right compressor) =
      ioToF1 (action compressor)
    release :  Either (List ZlibError) Compressor
            -> F1' World
    release (Left _)           =
      ioToF1 (pure ())
    release (Right compressor) =
      ioToF1 (destroyCompressor compressor)
    withCompressor' : Elin World [] (Either (List ZlibError) a)
    withCompressor' =
      bracket (runIO acquire)
              (\resource => runIO (use resource))
              (\resource => runIO (release resource))

||| Execute an action with a freshly initialized zlib decompression stream.
|||
||| The decompressor is finalized and released regardless of how the protected
||| action exits. Failure to initialize the decompressor is returned without
||| executing the supplied action.
|||
export
withDecompressor :  Wrapper
                 -> (Decompressor -> IO (Either (List ZlibError) a))
                 -> IO (Either (List ZlibError) a)
withDecompressor wrapper action = do
  result <- runElinIO withDecompressor'
  case result of
    Left err    => do
      errs <- mkError "Codec.Compression.Zlib.Internal.withCompressor" "\{show err}"
      pure (Left errs)
    Right value =>
      case value of
        Left errs    =>
          pure (Left errs)
        Right value' =>
          pure (Right value')
  where
    acquire : F1 World (Either (List ZlibError) Decompressor)
    acquire =
      ioToF1 (createDecompressor wrapper)
    use :  Either (List ZlibError) Decompressor
        -> F1 World (Either (List ZlibError) a)
    use (Left err)           =
      ioToF1 (pure (Left err))
    use (Right decompressor) =
      ioToF1 (action decompressor)
    release :  Either (List ZlibError) Decompressor
            -> F1' World
    release (Left _)             =
      ioToF1 (pure ())
    release (Right decompressor) =
      ioToF1 (destroyDecompressor decompressor)
    withDecompressor' : Elin World [] (Either (List ZlibError) a)
    withDecompressor' =
      bracket (runIO acquire)
              (\resource => runIO (use resource))
              (\resource => runIO (release resource))

--------------------------------------------------------------------------------
--          Buffer Operations
--------------------------------------------------------------------------------

private
deflateBoundError : Bits64
deflateBoundError = 0xffffffffffffffff

||| Compute an upper bound on the number of bytes required to compress an
||| input of the given size with an initialized compressor.
|||
||| The returned capacity is sufficient for single-pass compression when the
||| complete input is supplied to `deflateBuffer` and the flush mode is
||| `Finish`. In that case, zlib guarantees that an output buffer of at least
||| this size is sufficient for `deflate` to complete with `Z_STREAM_END`.
|||
||| This helper additionally enforces the size limits of the raw buffer bridge:
||| both the input length and resulting output capacity must fit in the
||| `uInt`-sized lengths used by zlib's `avail_in` and `avail_out` fields.
|||
||| Returns an error when the requested input or resulting bound cannot be
||| represented safely by the one-shot buffer API.
|||
export
deflateBound :  Compressor
             -> Nat
             -> IO (Either (List ZlibError) Nat)
deflateBound (MkCompressor (MkZStreamHandle ptr)) size =
  case size > 0xffffffff of
    True =>
      do
        err <- mkError "Codec.Compression.Zlib.Internal.deflateBound"
                       "Input is too large for the one-shot zlib buffer interface"
        pure (Left err)
    False => do
      bound <- primIO (prim__deflateBound ptr (cast size))
      case bound == deflateBoundError of
        True => do
          err <- mkError "Codec.Compression.Zlib.Internal.deflateBound"
                         "Compressed-size bound cannot be represented by the one-shot zlib buffer interface"
          pure (Left err)
        False =>
          pure (Right (cast bound))

||| Compress a region of an immutable input buffer into a region of a mutable
||| output buffer.
|||
||| Neither buffer is copied before crossing the FFI boundary. On the Chez
||| backend, their underlying bytevectors are borrowed as `u8*` values for
||| the duration of the atomic C call.
|||
||| The C shim guarantees that the borrowed pointers are removed from the
||| underlying `z_stream` before returning.
|||
||| The returned `BufferResult` reports how many input bytes were consumed
||| and how many output bytes were produced.
|||
export
deflateBuffer :  Compressor
              -> (inputoffset : Nat)
              -> (inputlength : Nat)
              -> IBuffer n
              -> {auto 0 inputbounds : LTE (inputoffset + inputlength) n}
              -> (outputoffset : Nat)
              -> (outputlength : Nat)
              -> IOBuffer m
              -> {auto 0 outputbounds : LTE (outputoffset + outputlength) m}
              -> FlushMode
              -> IO (Either (List ZlibError) BufferResult)
deflateBuffer (MkCompressor (MkZStreamHandle ptr)) inputoffset inputlength input outputoffset outputlength output flush = do
  status <- primIO ( prim__deflateBuffer
                       ptr
                       (unsafeGetBuffer input)
                       (cast inputoffset)
                       (cast inputlength)
                       (unsafeFromMBuffer output)
                       (cast outputoffset)
                       (cast outputlength)
                       (flushModeValue flush)
                   )
  consumed <- primIO (prim__consumed ptr)
  produced <- primIO (prim__produced ptr)
  case status == (zlibconstantToInt Z_OK) || status == (zlibconstantToInt Z_STREAM_END) || status == (zlibconstantToInt Z_BUF_ERROR) of
    True  =>
      pure (Right (MkBufferResult status (cast consumed) (cast produced)))
    False => do
      err <- statusError "Codec.Compression.Zlib.Internal.deflateBuffer" status
      pure (Left err)

||| Decompress a region of an immutable input buffer into a region of a mutable
||| output buffer.
|||
||| Neither buffer is copied before crossing the FFI boundary. On the Chez
||| backend, their underlying bytevectors are borrowed as `u8*` values for
||| the duration of the atomic C call.
|||
||| The C shim guarantees that the borrowed pointers are removed from the
||| underlying `z_stream` before returning.
|||
||| `Z_OK` indicates that decompression can continue. `Z_STREAM_END` indicates
||| that the complete compressed stream has been decoded. Both are represented
||| as successful `BufferResult` values; the caller can use the stream-oriented
||| operation when it needs to distinguish them.
|||
export
inflateBuffer :  Decompressor
              -> (inputoffset : Nat)
              -> (inputlength : Nat)
              -> IBuffer n
              -> {auto 0 inputbounds : LTE (inputoffset + inputlength) n}
              -> (outputoffset : Nat)
              -> (outputlength : Nat)
              -> IOBuffer m
              -> {auto 0 outputbounds : LTE (outputoffset + outputlength) m}
              -> FlushMode
              -> IO (Either (List ZlibError) BufferResult)
inflateBuffer (MkDecompressor (MkZStreamHandle ptr)) inputoffset inputlength input outputoffset outputlength output flush = do
  status <- primIO ( prim__inflateBuffer
                       ptr
                       (unsafeGetBuffer input)
                       (cast inputoffset)
                       (cast inputlength)
                       (unsafeFromMBuffer output)
                       (cast outputoffset)
                       (cast outputlength)
                       (flushModeValue flush)
                   )
  consumed <- primIO (prim__consumed ptr)
  produced <- primIO (prim__produced ptr)
  case status == (zlibconstantToInt Z_OK) || status == (zlibconstantToInt Z_STREAM_END) || status == (zlibconstantToInt Z_BUF_ERROR) of
    True  =>
      pure (Right (MkBufferResult status (cast consumed) (cast produced)))
    False => do
      err <- statusError "Codec.Compression.Zlib.Internal.inflateBuffer" status
      pure (Left err)
