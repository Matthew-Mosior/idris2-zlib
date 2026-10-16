module Main

import Codec.Compression.Zlib.Compress
import Codec.Compression.Zlib.Decompress
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
bufferBytes : AnyBuffer -> List Bits8
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
      ]
  case ok of
    True =>
      putStrLn "All zlib tests passed."
    False =>
      exitFailure
