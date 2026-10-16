module System.Zlib.Raw

import Data.Buffer

%default total

--------------------------------------------------------------------------------
--          Stream Allocation
--------------------------------------------------------------------------------

||| Allocates a new opaque zlib stream handle.
|||
||| The returned pointer refers to storage owned by the C shim. The stream must
||| eventually be released with `prim__freeStream`.
|||
||| This function only allocates the stream structure; it does not initialize
||| the stream for compression or decompression.
|||
export %foreign "C:zlib_idris_stream_new,zlib-idris"
prim__newStream : PrimIO AnyPtr

||| Releases an opaque zlib stream handle previously allocated with
||| `prim__newStream`.
|||
||| Any initialized compression or decompression state must first be finalized
||| with `prim__deflateEnd` or `prim__inflateEnd`, respectively.
|||
export %foreign "C:zlib_idris_stream_free,zlib-idris"
prim__freeStream : AnyPtr -> PrimIO ()

--------------------------------------------------------------------------------
--          Library Information
--------------------------------------------------------------------------------

||| Returns the version string of the zlib implementation linked by the
||| `zlib-idris` C shim.
|||
export %foreign "C:zlib_idris_version,zlib-idris"
prim__zlibVersion : PrimIO String

--------------------------------------------------------------------------------
--          Compression
--------------------------------------------------------------------------------

||| Initializes an opaque stream for compression using zlib's default method,
||| window size, memory level, and compression strategy.
|||
||| The supplied integer is a zlib compression level, such as
||| `Z_DEFAULT_COMPRESSION`.
|||
||| Unlike zlib's `deflateInit_`, the zlib version and `z_stream` structure
||| size are supplied by the C shim, preventing ABI-sensitive details from
||| crossing the Idris FFI boundary.
|||
export %foreign "C:zlib_idris_deflate_init,zlib-idris"
prim__deflateInit : AnyPtr -> Int -> PrimIO Int

||| Initializes an opaque stream for compression with explicit zlib
||| configuration.
|||
||| The arguments following the stream are, in order:
|||
||| - compression level
||| - compression method
||| - window bits
||| - memory level
||| - compression strategy
|||
||| The C shim supplies `ZLIB_VERSION` and `sizeof(z_stream)` directly when
||| invoking `deflateInit2`, keeping those ABI details out of Idris.
|||
export %foreign "C:zlib_idris_deflate_init2,zlib-idris"
prim__deflateInit2 : AnyPtr -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

||| Compresses bytes from an Idris `Buffer` into another Idris `Buffer`.
|||
export %foreign "C:zlib_idris_deflate_buffer,zlib-idris"
prim__deflateBuffer : AnyPtr -> Buffer -> Bits32 -> Bits32 -> Buffer -> Bits32 -> Bits32 -> Int -> PrimIO Int

||| Releases all zlib resources associated with an initialized compression
||| stream.
|||
||| This does not free the opaque stream handle itself. Call
||| `prim__freeStream` afterwards to release that storage.
|||
export %foreign "C:zlib_idris_deflate_end,zlib-idris"
prim__deflateEnd : AnyPtr -> PrimIO Int

||| Resets an initialized compression stream while preserving its allocated
||| zlib state so that it can be reused for another compression operation.
|||
export %foreign "C:zlib_idris_deflate_reset,zlib-idris"
prim__deflateReset : AnyPtr -> PrimIO Int

||| Return an upper bound on the output capacity required to compress the
||| supplied number of input bytes using an initialized deflate stream.
|||
||| `UINT64_MAX` is returned by the C shim when the input size or resulting
||| bound cannot be represented safely by the one-shot zlib buffer interface.
|||
export %foreign "C:zlib_idris_deflate_bound,zlib-idris"
prim__deflateBound : AnyPtr -> Bits64 -> PrimIO Bits64

--------------------------------------------------------------------------------
--          Decompression
--------------------------------------------------------------------------------

||| Initializes an opaque stream for decompression using the standard zlib
||| wrapper format.
|||
||| `ZLIB_VERSION` and `sizeof(z_stream)` are supplied by the C shim rather
||| than crossing the Idris FFI boundary.
|||
export %foreign "C:zlib_idris_inflate_init,zlib-idris"
prim__inflateInit : AnyPtr -> PrimIO Int

||| Initializes an opaque stream for decompression with explicit `windowBits`.
|||
||| This is used to select standard zlib, gzip, or raw DEFLATE streams.
||| `ZLIB_VERSION` and `sizeof(z_stream)` are supplied by the C shim.
|||
export %foreign "C:zlib_idris_inflate_init2,zlib-idris"
prim__inflateInit2 : AnyPtr -> Int -> PrimIO Int

||| Decompresses bytes from an Idris `Buffer` into another Idris `Buffer`.
|||
export %foreign "C:zlib_idris_inflate_buffer,zlib-idris"
prim__inflateBuffer : AnyPtr -> Buffer -> Bits32 -> Bits32 -> Buffer -> Bits32 -> Bits32 -> Int -> PrimIO Int

||| Releases all zlib resources associated with an initialized decompression
||| stream.
|||
||| This does not free the opaque stream handle itself. Call
||| `prim__freeStream` afterwards to release that storage.
|||
export %foreign "C:zlib_idris_inflate_end,zlib-idris"
prim__inflateEnd : AnyPtr -> PrimIO Int

||| Resets an initialized decompression stream while retaining its allocated
||| zlib state so that it can be reused.
|||
export
%foreign "C:zlib_idris_inflate_reset,zlib-idris"
prim__inflateReset : AnyPtr -> PrimIO Int

--------------------------------------------------------------------------------
--          Buffer Progress
--------------------------------------------------------------------------------

||| Returns the number of input bytes consumed by the most recent
||| `prim__deflateBuffer` or `prim__inflateBuffer` call.
|||
export %foreign "C:zlib_idris_consumed,zlib-idris"
prim__consumed : AnyPtr -> PrimIO Bits32

||| Returns the number of output bytes produced by the most recent
||| `prim__deflateBuffer` or `prim__inflateBuffer` call.
|||
export %foreign "C:zlib_idris_produced,zlib-idris"
prim__produced : AnyPtr -> PrimIO Bits32

--------------------------------------------------------------------------------
--          Checksums
--------------------------------------------------------------------------------

||| Updates a CRC-32 checksum using the contents of an Idris `Buffer`.
|||
export %foreign "C:zlib_idris_crc32,zlib-idris"
prim__crc32 : Bits32 -> Buffer -> Bits32 -> PrimIO Bits32

||| Updates an Adler-32 checksum using the contents of an Idris `Buffer`.
|||
export %foreign "C:zlib_idris_adler32,zlib-idris"
prim__adler32 : Bits32 -> Buffer -> Bits32 -> PrimIO Bits32
