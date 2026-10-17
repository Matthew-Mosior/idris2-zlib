module Main

import Codec.Compression.Zlib
import Data.Buffer.Builder
import Data.Buffer.Core
import Data.Buffer.Indexed
import Data.Linear.Token
import Data.String
import Hedgehog
import System
import System.Zlib.Error
import System.Zlib.Constants
import System.Zlib.Raw
import System.Zlib.Types

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

0 BufferOperation : Type
BufferOperation = {n : Nat} -> IBuffer n -> IO (Either (List ZlibError) AnyBuffer)

--------------------------------------------------------------------------------
--          Test Utilities
--------------------------------------------------------------------------------

||| Lift an `IO` action into Hedgehog's general `TestT IO` test monad.
|||
liftIOTest :  IO a
           -> TestT IO a
liftIOTest action =
  mkTestT $ do
    value <- action
    pure (Right value, neutral)

||| Fail a test when an effectful zlib operation returns an error.
|||
evalZlib :  IO (Either (List ZlibError) a)
         -> TestT IO a
evalZlib action = do
  result <- liftIOTest action
  case result of
    Left errs =>
      failWith Nothing (show errs)
    Right value =>
      pure value

||| Convert an `AnyBuffer` to a list of bytes.
|||
bufferBytes :  AnyBuffer
            -> List Bits8
bufferBytes =
  unpack

||| Execute a compression/decompression round trip and compare the resulting
||| bytes with the original input.
|||
roundTrip :  BufferOperation
          -> BufferOperation
          -> List Bits8
          -> TestT IO ()
roundTrip compressor decompressor bytes = do
  let input = bufferL bytes
  compressed   <- evalZlib (compressor input)
  decompressed <- evalZlib (decompressor compressed.ibuf)
  bufferBytes decompressed === bytes

--------------------------------------------------------------------------------
--          Checksum Test Data
--------------------------------------------------------------------------------

checksumbytes : List Bits8
checksumbytes =
  [ 0x31 -- 1
  , 0x32 -- 2
  , 0x33 -- 3
  , 0x34 -- 4
  , 0x35 -- 5
  , 0x36 -- 6
  , 0x37 -- 7
  , 0x38 -- 8
  , 0x39 -- 9
  ]

--------------------------------------------------------------------------------
--          Streaming Test Utilities
--------------------------------------------------------------------------------

||| Split a list into chunks of at most the requested size.
|||
||| A zero chunk size returns the complete input as one chunk.
|||
private
chunksOf :  Nat
         -> List a
         -> List (List a)
chunksOf 0 [] =
  []
chunksOf 0 xs =
  [xs]
chunksOf n@(S _) xs =
  go xs
  where
    go :  List a
       -> List (List a)
    go [] =
      []
    go ys =
      let (chunk, rest) = splitAt n ys
       in chunk :: assert_total (go rest)

||| Insert empty chunks between existing chunks.
|||
private
withEmptyChunks :  List (List a)
                -> List (List a)
withEmptyChunks []        =
  []
withEmptyChunks [x]       =
  [x]
withEmptyChunks (x :: xs) =
  x :: [] :: withEmptyChunks xs


||| Remove the final element from a list.
|||
private
dropLast :  List a
         -> List a
dropLast []        =
  []
dropLast [_]       =
  []
dropLast (x :: xs) =
  x :: dropLast xs

||| Select the one-shot compressor corresponding to a wrapper.
|||
private
oneShotCompress :  Wrapper
                -> {n : Nat}
                -> IBuffer n
                -> IO (Either (List ZlibError) AnyBuffer)
oneShotCompress ZlibWrapper =
  compress
oneShotCompress GZipWrapper =
  gzipCompress
oneShotCompress RawWrapper =
  rawCompress

||| Select the one-shot decompressor corresponding to a wrapper.
|||
private
oneShotDecompress :  Wrapper
                  -> {n : Nat}
                  -> IBuffer n
                  -> IO (Either (List ZlibError) AnyBuffer)
oneShotDecompress ZlibWrapper =
  decompress
oneShotDecompress GZipWrapper =
  gzipDecompress
oneShotDecompress RawWrapper =
  rawDecompress

||| Compress a sequence of input chunks and finish the stream.
|||
private
streamCompress :
     Wrapper
  -> List (List Bits8)
  -> IO (Either (List ZlibError) AnyBuffer)
streamCompress wrapper chunks =
  withStreamingCompressor
    DefaultCompression
    wrapper
    $ \stream =>
        feed stream chunks []
  where
    feed :
         StreamingCompressor
      -> List (List Bits8)
      -> List AnyBuffer
      -> IO (Either (List ZlibError) AnyBuffer)

    feed stream [] acc = do
      Right result <- finishCompressor stream
        | Left errs =>
            pure (Left errs)

      pure $
        Right $
          Data.Buffer.Builder.fastConcat
            (acc ++ result.output)

    feed stream (bytes :: rest) acc = do
      Right result <-
        compressChunk
          stream
          (bufferL bytes)
        | Left errs =>
            pure (Left errs)

      let acc' =
            acc ++ result.output

          (_, remaining) =
            splitAt result.consumed bytes

      case remaining of
        [] =>
          feed stream rest acc'

        _ =>
          case rest of
            [] =>
              assert_total $
                feed stream [remaining] acc'

            next :: more =>
              assert_total $
                feed
                  stream
                  ((remaining ++ next) :: more)
                  acc'

||| Incrementally decompress a sequence of compressed chunks.
|||
||| The returned boolean indicates whether `Z_STREAM_END` was reached.
|||
private
streamDecompress :  Wrapper
                 -> List (List Bits8)
                 -> IO (Either (List ZlibError) (AnyBuffer, Bool))
streamDecompress wrapper chunks =
  withStreamingDecompressor
    wrapper
    $ \stream =>
        feed stream chunks []
  where
    feed :  StreamingDecompressor
         -> List (List Bits8)
         -> List AnyBuffer
         -> IO (Either (List ZlibError) (AnyBuffer, Bool))
    feed stream [] acc =
      pure
        ( Right
            ( Data.Buffer.Builder.fastConcat (reverse acc)
            , False
            )
        )
    feed stream (bytes :: rest) acc = do
      Right result <-
        decompressChunk
          stream
          (bufferL bytes)
        | Left errs =>
            pure (Left errs)
      let acc'           : List AnyBuffer
          acc'           =
            reverse result.output ++ acc
          consumed'      : Nat
          consumed'      =
            result.consumed
          remaining      =
            splitAt consumed' bytes
      case result.finished of
        True =>
          pure
            ( Right
                ( Data.Buffer.Builder.fastConcat (reverse acc')
                , True
                )
            )
        False =>
          case snd remaining of
            [] =>
              assert_total (feed stream rest acc')
            _ =>
              case rest of
                [] =>
                  -- zlib has not consumed the complete final input
                  -- and there is no additional input available.
                  pure
                    ( Right
                        ( Data.Buffer.Builder.fastConcat (reverse acc')
                        , False
                        )
                    )
                next :: more =>
                  -- `remaining` was not consumed by inflate.  Preserve it
                  -- and prepend it to the next supplied chunk.
                  assert_total (feed stream ((snd remaining ++ next) :: more) acc')

--------------------------------------------------------------------------------
--          Streaming Test Data
--------------------------------------------------------------------------------

private
payloadblock : List Bits8
payloadblock =
  [ 0x00
  , 0x01
  , 0x02
  , 0x03
  , 0x7f
  , 0x80
  , 0xfe
  , 0xff
  , 0x11
  , 0x22
  , 0x33
  , 0x44
  , 0x55
  , 0x66
  , 0x77
  , 0x88
  ]

||| 1 KiB.
|||
private
smallStreamingPayload : List Bits8
smallStreamingPayload =
  concat $
    replicate 64 payloadblock

||| 80 KiB, ensuring several 32 KiB boundaries are crossed.
|||
private
chunkBoundaryPayload : List Bits8
chunkBoundaryPayload =
  concat $
    replicate 5120 payloadblock

||| 144,000 bytes: comfortably over 100 KiB.
|||
private
largeStreamingPayload : List Bits8
largeStreamingPayload =
  concat $
    replicate 9000 payloadblock

--------------------------------------------------------------------------------
--          Round-trip Tests
--------------------------------------------------------------------------------

testZlibRoundTrip : TestT IO ()
testZlibRoundTrip =
  roundTrip
    compress
    decompress
    [ 0x00
    , 0x01
    , 0x02
    , 0x7f
    , 0x80
    , 0xfe
    , 0xff
    ]

testGzipRoundTrip : TestT IO ()
testGzipRoundTrip =
  roundTrip
    gzipCompress
    gzipDecompress
    [ 0x00
    , 0x01
    , 0x02
    , 0x7f
    , 0x80
    , 0xfe
    , 0xff
    ]

testRawRoundTrip : TestT IO ()
testRawRoundTrip =
  roundTrip
    rawCompress
    rawDecompress
    [ 0x00
    , 0x01
    , 0x02
    , 0x7f
    , 0x80
    , 0xfe
    , 0xff
    ]

--------------------------------------------------------------------------------
--          Edge Cases
--------------------------------------------------------------------------------

testEmptyZlib : TestT IO ()
testEmptyZlib =
  roundTrip compress decompress []

testEmptyGzip : TestT IO ()
testEmptyGzip =
  roundTrip gzipCompress gzipDecompress []

testEmptyRaw : TestT IO ()
testEmptyRaw =
  roundTrip rawCompress rawDecompress []

||| Exercise several 32 KiB inflate output chunks.
|||
testLargeZlib : TestT IO ()
testLargeZlib =
  roundTrip
    compress
    decompress
    (replicate 100000 0x41)


testLargeGzip : TestT IO ()
testLargeGzip =
  roundTrip
    gzipCompress
    gzipDecompress
    (replicate 100000 0x41)


testLargeRaw : TestT IO ()
testLargeRaw =
  roundTrip
    rawCompress
    rawDecompress
    (replicate 100000 0x41)

--------------------------------------------------------------------------------
--          Wrapper Mismatch Tests
--------------------------------------------------------------------------------

gzipmismatchbytes : List Bits8
gzipmismatchbytes =
  [ 0x67 -- g
  , 0x7a -- z
  , 0x69 -- i
  , 0x70 -- p
  , 0x20
  , 0x77 -- w
  , 0x72 -- r
  , 0x61 -- a
  , 0x70 -- p
  , 0x70 -- p
  , 0x65 -- e
  , 0x72 -- r
  ]

testGzipRejectedByZlib : TestT IO ()
testGzipRejectedByZlib = do
  compressed <- evalZlib (gzipCompress (bufferL gzipmismatchbytes))
  result     <- liftIOTest (decompress compressed.ibuf)
  case result of
    Left _ =>
      success
    Right _ =>
      failWith Nothing "gzip stream was incorrectly accepted by the zlib decompressor"

testZlibRejectedByGzip : TestT IO ()
testZlibRejectedByGzip = do
  compressed <- evalZlib (compress (bufferL gzipmismatchbytes))
  result     <- liftIOTest (gzipDecompress compressed.ibuf)
  case result of
    Left _ =>
      success
    Right _ =>
      failWith Nothing "zlib stream was incorrectly accepted by the gzip decompressor"

testRawRejectedByZlib : TestT IO ()
testRawRejectedByZlib = do
  compressed <- evalZlib (rawCompress (bufferL gzipmismatchbytes))
  result     <- liftIOTest (decompress compressed.ibuf)
  case result of
    Left _ =>
      success
    Right _ =>
      failWith Nothing "raw DEFLATE stream was incorrectly accepted by the zlib decompressor"

--------------------------------------------------------------------------------
--          Invalid Input Tests
--------------------------------------------------------------------------------

garbagebytes : List Bits8
garbagebytes =
  [ 0xde
  , 0xad
  , 0xbe
  , 0xef
  , 0x01
  , 0x02
  , 0x03
  , 0x04
  ]

testInvalidZlibRejected : TestT IO ()
testInvalidZlibRejected = do
  result <- liftIOTest (decompress (bufferL garbagebytes))
  case result of
    Left _ =>
      success
    Right _ =>
      failWith Nothing "invalid data was incorrectly accepted as a zlib stream"

testInvalidGzipRejected : TestT IO ()
testInvalidGzipRejected = do
  result <- liftIOTest (gzipDecompress (bufferL garbagebytes))
  case result of
    Left _ =>
      success
    Right _ =>
      failWith Nothing "invalid data was incorrectly accepted as a gzip stream"

--------------------------------------------------------------------------------
--          Compression Sanity
--------------------------------------------------------------------------------

testRepetitiveInputCompresses : TestT IO ()
testRepetitiveInputCompresses = do
  let bytes : List Bits8
      bytes = replicate 100000 0x41
  compressed <-
    evalZlib $
      compress (bufferL bytes)
  assert $
    compressed.size < length bytes

--------------------------------------------------------------------------------
--          Runner
--------------------------------------------------------------------------------

runTest :  String
        -> TestT IO ()
        -> IO Bool
runTest name test = do
  result <- runTestT test
  case result of
    (Right (), _) => do
      putStrLn $ "  ✓ " ++ name
      pure True
    (Left err, _) => do
      putStrLn $ "  ✗ " ++ name
      putStrLn $ "    " ++ show err
      pure False

runTests :  List (String, TestT IO ())
         -> IO Bool
runTests =
  go True
  where
    go :  Bool
       -> List (String, TestT IO ())
       -> IO Bool
    go acc [] =
      pure acc
    go acc ((name, test) :: xs) = do
      ok <- runTest name test
      go (acc && ok) xs

--------------------------------------------------------------------------------
--          Checksum Tests
--------------------------------------------------------------------------------

testCrc32KnownVector : TestT IO ()
testCrc32KnownVector = do
  actual <- liftIOTest (crc32 (bufferL checksumbytes))
  actual === 0xcbf43926

testAdler32KnownVector : TestT IO ()
testAdler32KnownVector = do
  actual <- liftIOTest (adler32 (bufferL checksumbytes))
  actual === 0x091e01de

testCrc32Empty : TestT IO ()
testCrc32Empty = do
  actual <- liftIOTest (crc32 (bufferL []))
  actual === 0

testAdler32Empty : TestT IO ()
testAdler32Empty = do
  actual <- liftIOTest (adler32 (bufferL []))
  actual === 1

testCrc32Incremental : TestT IO ()
testCrc32Incremental = do
  whole       <- liftIOTest (crc32 (bufferL checksumbytes))
  first       <- liftIOTest (crc32Update 0 (bufferL [0x31, 0x32, 0x33, 0x34]))
  incremental <- liftIOTest (crc32Update first (bufferL [0x35, 0x36, 0x37, 0x38, 0x39]))
  incremental === whole

testAdler32Incremental : TestT IO ()
testAdler32Incremental = do
  whole       <- liftIOTest (adler32 (bufferL checksumbytes))
  first       <-
    liftIOTest (adler32Update 1 (bufferL [0x31, 0x32, 0x33, 0x34]))
  incremental <-
    liftIOTest (adler32Update first (bufferL [0x35, 0x36, 0x37, 0x38, 0x39]))
  incremental === whole

testCrc32BinaryIncremental : TestT IO ()
testCrc32BinaryIncremental = do
  let left  : List Bits8
      left  = [0x00, 0xff, 0x80, 0x01]
      right : List Bits8
      right = [0xfe, 0x7f, 0x00, 0xaa]
      whole = left ++ right
  expected <- liftIOTest (crc32 (bufferL whole))
  first    <- liftIOTest (crc32Update 0 (bufferL left))
  actual   <- liftIOTest (crc32Update first (bufferL right))
  actual === expected

--------------------------------------------------------------------------------
--          Streaming Tests
--------------------------------------------------------------------------------

testOneByteStreamingCompressOneShotDecompress : TestT IO ()
testOneByteStreamingCompressOneShotDecompress = do
  compressed <-
    evalZlib $
      streamCompress
        ZlibWrapper
        (chunksOf 1 smallStreamingPayload)

  decoded <-
    evalZlib $
      decompress compressed.ibuf

  unpack decoded === smallStreamingPayload


testOneShotCompressOneByteStreamingDecompress : TestT IO ()
testOneShotCompressOneByteStreamingDecompress = do
  compressed <-
    evalZlib $
      compress (bufferL smallStreamingPayload)

  (decoded, finished) <-
    evalZlib $
      streamDecompress
        ZlibWrapper
        (chunksOf 1 (unpack compressed))

  finished === True
  unpack decoded === smallStreamingPayload

testStreamingOneByteChunks : TestT IO ()
testStreamingOneByteChunks = do
  compressed          <-
    evalZlib (streamCompress ZlibWrapper (chunksOf 1 smallStreamingPayload))
  (decoded, finished) <-
    evalZlib (streamDecompress ZlibWrapper (chunksOf 1 (unpack compressed)))
  finished       === True
  unpack decoded === smallStreamingPayload

testDebugStreamingCompress : TestT IO ()
testDebugStreamingCompress = do
  let bytes : List Bits8
      bytes =
        [ 0x00
        , 0x01
        , 0x02
        , 0x03
        ]

  result <-
    evalZlib $
      withStreamingCompressor
        DefaultCompression
        ZlibWrapper
        $ \stream =>
            go stream (chunksOf 1 bytes) []
  success
  where
    go :
         StreamingCompressor
      -> List (List Bits8)
      -> List AnyBuffer
      -> IO (Either (List ZlibError) ())
    go stream [] acc = do
      Right result <- finishCompressor stream
        | Left errs =>
            pure (Left errs)

      putStrLn $
        "finish: consumed="
          ++ show result.consumed
          ++ " finished="
          ++ show result.finished
          ++ " chunks="
          ++ show (map (\b => unpack b) result.output)

      let allOutput =
            Data.Buffer.Builder.fastConcat
              (acc ++ result.output)

      putStrLn $
        "final compressed = "
          ++ show (unpack allOutput)

      pure (Right ())

    go stream (chunk :: rest) acc = do
      Right result <-
        compressChunk stream (bufferL chunk)
        | Left errs =>
            pure (Left errs)

      putStrLn $
        "compress "
          ++ show chunk
          ++ ": consumed="
          ++ show result.consumed
          ++ " finished="
          ++ show result.finished
          ++ " output="
          ++ show (map (\b => unpack b) result.output)

      go
        stream
        rest
        (acc ++ result.output)

testDebugStreamingDecompress : TestT IO ()
testDebugStreamingDecompress = do
  compressed <-
    evalZlib $
      compress $
        bufferL
          [ 0x00
          , 0x01
          , 0x02
          , 0x03
          ]

  let compressedBytes =
        unpack compressed

  _ <-
    evalZlib $
      withStreamingDecompressor
        ZlibWrapper
        $ \stream =>
            go stream (chunksOf 1 compressedBytes)

  success
  where
    go :
         StreamingDecompressor
      -> List (List Bits8)
      -> IO (Either (List ZlibError) ())
    go stream [] =
      pure (Right ())

    go stream (chunk :: rest) = do
      Right result <-
        decompressChunk stream (bufferL chunk)
        | Left errs =>
            pure (Left errs)

      putStrLn $
        "inflate "
          ++ show chunk
          ++ ": consumed="
          ++ show result.consumed
          ++ " finished="
          ++ show result.finished
          ++ " output="
          ++ show (map (\b => unpack b) result.output)

      go stream rest        

--------------------------------------------------------------------------------
--          Main
--------------------------------------------------------------------------------

main : IO ()
main = do
  putStrLn "━━━ zlib tests ━━━"
  ok <-
    runTests
      [ ("zlib round-trip", testZlibRoundTrip)
      , ("gzip round-trip", testGzipRoundTrip)
      , ("raw DEFLATE round-trip", testRawRoundTrip)
      , ("empty zlib", testEmptyZlib)
      , ("empty gzip", testEmptyGzip)
      , ("empty raw DEFLATE", testEmptyRaw)
      , ("multi-chunk zlib", testLargeZlib)
      , ("multi-chunk gzip", testLargeGzip)
      , ("multi-chunk raw DEFLATE", testLargeRaw)
      , ("gzip rejected by zlib", testGzipRejectedByZlib)
      , ("zlib rejected by gzip", testZlibRejectedByGzip)
      , ("raw DEFLATE rejected by zlib", testRawRejectedByZlib)
      , ("invalid zlib rejected", testInvalidZlibRejected)
      , ("invalid gzip rejected", testInvalidGzipRejected)
      , ("repetitive input compresses", testRepetitiveInputCompresses)
      , ("CRC-32 known vector", testCrc32KnownVector)
      , ("Adler-32 known vector", testAdler32KnownVector)
      , ("CRC-32 empty input", testCrc32Empty)
      , ("Adler-32 empty input", testAdler32Empty)
      , ("CRC-32 incremental", testCrc32Incremental)
      , ("Adler-32 incremental", testAdler32Incremental)
      , ("CRC-32 binary incremental", testCrc32BinaryIncremental)
      , ("streaming compress (debug)", testDebugStreamingCompress)
      , ("streaming decompress (debug)", testDebugStreamingDecompress)
      --, ("streaming 1-byte chunks", testOneByteStreamingCompressOneShotDecompress)
      --, ("streaming 1-byte chunks", testOneShotCompressOneByteStreamingDecompress)
      --, ("streaming 1-byte chunks", testStreamingOneByteChunks)
      ]
  case ok of
    True =>
      putStrLn "All zlib tests passed."
    False =>
      exitFailure
