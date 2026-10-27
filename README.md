# Idris2 bindings to zlib

This library provides a type-safe Idris2 binding to [zlib](https://github.com/madler/zlib) that supports zlib, gzip, and raw DEFLATE compression/decompression, including both one-shot and streaming APIs. It uses direct C FFI with explicit buffer bounds, deterministic native resource management, checksum support (CRC-32/Adler-32), and proof-aware handling of consumed/produced buffer regions.

## Features

-   zlib, gzip, and raw DEFLATE support for compression and decompression.
-   One-shot APIs for simple whole-buffer compression and decompression.
-   Streaming compression and decompression APIs with explicit consumed/produced byte tracking.
-   Configurable compression levels including no compression, best speed, best compression, and custom levels.
-   Configurable zlib strategies such as default, filtered, Huffman-only, RLE, and fixed.
-   Flush-mode support including NoFlush, SyncFlush, FullFlush, Finish, and others.
-   Direct C FFI integration without Scheme-level foreign-procedure wrappers.
-   Type-safe buffer handling using Idris2 indexed buffers and explicit bounds proofs.
-   Proof-aware input/output regions to ensure FFI calls stay within buffer bounds.
-   Safe native resource management for z_stream lifetimes using bracketed cleanup.
-   Incremental CRC-32 and Adler-32 checksums, plus convenient one-shot checksum functions.
-   Portable ABI normalization around zlib types such as uInt and uLong.
-   Accurate streaming progress reporting through consumed, produced, status, and finished-state information.
-   Preservation of trailing input bytes after Z_STREAM_END, useful for framed protocols and concatenated data.
-   Efficient fixed-size output chunking for decompression and streaming operations.
-   No repeated grow-and-copy strategy for large decompression workloads.
-   Safe ownership of streaming output buffers by copying produced prefixes before retaining immutable chunks.
-   Support for very small chunk sizes, including validated 1-byte streaming input.
-   Comprehensive interoperability tests
    -  Streaming
    -  Streaming to one-shot
    -  One-shot to streaming
    -  Empty input
    -  Truncated streams
    -  Wrapper mismatches
    -  Invalid compressed data
    -  Large payloads
    -  Trailing bytes
 
 ## Architecture

### Overall architecture

flowchart TB
    User["User Code"]

    subgraph Public["Public Idris2 API"]
        Zlib["Codec.Compression.Zlib"]
        Compress["Compress"]
        Decompress["Decompress"]
        Stream["Stream"]
        Checksum["Checksum"]
    end

    subgraph Typed["Typed / Internal Layer"]
        Types["Codec.Compression.Zlib.Types"]
        Internal["Codec.Compression.Zlib.Internal"]
        Proofs["Codec.Compression.Zlib.Proofs"]
        Utilities["Codec.Compression.Zlib.Utilities"]
    end

    subgraph System["Low-Level Idris2 Layer"]
        Raw["System.Zlib.Raw"]
        Constants["System.Zlib.Constants"]
        Errors["System.Zlib.Error"]
        SysTypes["System.Zlib.Types"]
    end

    subgraph C["C Shim"]
        StreamWrap["zlib_idris_stream"]
        DeflateBridge["zlib_idris_deflate_buffer"]
        InflateBridge["zlib_idris_inflate_buffer"]
        BoundBridge["zlib_idris_deflate_bound"]
        ChecksumBridge["crc32 / adler32 wrappers"]
    end

    subgraph Native["Native zlib"]
        Deflate["deflate / deflateInit2 / deflateEnd"]
        Inflate["inflate / inflateInit2 / inflateEnd"]
        Bound["deflateBound"]
        Checksums["crc32 / adler32"]
    end

    User --> Zlib

    Zlib --> Compress
    Zlib --> Decompress
    Zlib --> Stream
    Zlib --> Checksum

    Compress --> Internal
    Decompress --> Internal
    Stream --> Internal
    Stream --> Utilities
    Stream --> Proofs
    Checksum --> Raw

    Internal --> Types
    Internal --> Raw
    Internal --> Errors
    Internal --> Constants
    Internal --> SysTypes

    Utilities --> Proofs

    Raw --> StreamWrap
    Raw --> DeflateBridge
    Raw --> InflateBridge
    Raw --> BoundBridge
    Raw --> ChecksumBridge

    DeflateBridge --> Deflate
    InflateBridge --> Inflate
    BoundBridge --> Bound
    ChecksumBridge --> Checksums

### Streaming compression/decompression model

flowchart LR
    subgraph Compression["Streaming Compression"]
        CInput["IBuffer input chunk"]
        COffset["Track input offset"]
        CCall["deflateBuffer"]
        CStatus{"zlib status"}
        COutput["IOBuffer 32 KiB"]
        CFreeze["Copy produced prefix\ninto owned AnyBuffer"]
        CChunks["List AnyBuffer"]
        CMore{"More input or\noutput pending?"}
        CFinish["finishCompressor\nZ_FINISH"]
        CEnd["Z_STREAM_END"]

        CInput --> COffset
        COffset --> CCall
        COutput --> CCall
        CCall --> CStatus
        CCall --> CFreeze
        CFreeze --> CChunks
        CStatus --> CMore
        CMore -->|yes| COffset
        CMore -->|no, more input may follow| CInput
        CMore -->|caller finishes| CFinish
        CFinish --> CCall
        CStatus -->|Z_STREAM_END| CEnd
    end

    subgraph Decompression["Streaming Decompression"]
        DInput["IBuffer compressed chunk"]
        DOffset["Track input offset"]
        DCall["inflateBuffer"]
        DStatus{"zlib status"}
        DOutput["IOBuffer 32 KiB"]
        DFreeze["Copy produced prefix\ninto owned AnyBuffer"]
        DChunks["List AnyBuffer"]
        DMore{"More input/output\nprocessing needed?"}
        DWait["finished = False\nwait for more input"]
        DEnd["Z_STREAM_END\nfinished = True"]
        DTrailing["Unconsumed trailing bytes\nremain with caller"]

        DInput --> DOffset
        DOffset --> DCall
        DOutput --> DCall
        DCall --> DStatus
        DCall --> DFreeze
        DFreeze --> DChunks

        DStatus -->|Z_OK| DMore
        DStatus -->|Z_BUF_ERROR / no progress| DWait
        DStatus -->|Z_STREAM_END| DEnd

        DMore -->|input remains| DOffset
        DMore -->|output buffer full| DOffset
        DMore -->|input exhausted| DWait

        DEnd --> DTrailing
    end

### Buffer ownership model

flowchart TB
    Input["Immutable IBuffer n"]
    Mutable["Mutable IOBuffer capacity"]

    InputRaw["Underlying Idris Buffer"]
    MutableRaw["Underlying Idris Buffer"]

    CInput["const uint8_t *"]
    COutput["uint8_t *"]

    Zlib["zlib call"]

    Progress["BufferResult\nstatus / consumed / produced"]

    BoundsCheck["Runtime bound check\nproduced <= capacity"]

    Proof["Erased LTE proof"]

    Copy["copy produced prefix"]

    Owned["Independent AnyBuffer"]

    Clear["Clear next_in / next_out\nbefore returning from C"]

    Input --> InputRaw
    Mutable --> MutableRaw

    InputRaw --> CInput
    MutableRaw --> COutput

    CInput --> Zlib
    COutput --> Zlib

    Zlib --> Progress
    Zlib --> Clear

    Progress --> BoundsCheck
    BoundsCheck -->|valid| Proof
    Proof --> Copy

    Mutable --> Copy
    Copy --> Owned

    Clear --> Input
    Clear --> Mutable

### One-shot versus Streaming API

flowchart TB
    Input["Input Buffer"]

    subgraph OneShot["One-Shot API"]
        OCompress["compress / gzipCompress / rawCompress"]
        ODecompress["decompress / gzipDecompress / rawDecompress"]

        Bound["deflateBound"]
        SingleDeflate["deflate(..., Z_FINISH)"]
        InflateLoop["inflate loop\n32 KiB chunks"]
        FinalConcat["fastConcat"]
    end

    subgraph Streaming["Streaming API"]
        SCompress["compressChunk"]
        SFinish["finishCompressor"]
        SDecompress["decompressChunk"]

        DeflateCalls["Repeated deflate calls"]
        InflateCalls["Repeated inflate calls"]
        ChunkOutputs["Return List AnyBuffer"]
    end

    Input --> OCompress
    Input --> ODecompress
    Input --> SCompress
    Input --> SDecompress

    OCompress --> Bound
    Bound --> SingleDeflate

    ODecompress --> InflateLoop
    InflateLoop --> FinalConcat

    SCompress --> DeflateCalls
    SFinish --> DeflateCalls
    DeflateCalls --> ChunkOutputs

    SDecompress --> InflateCalls
    InflateCalls --> ChunkOutputs

## API

-   `Codec.Compression.Zlib`: Top-level re-exporter.  This is the simplest import for most users who want compression, decompression, streaming, and checksum functionality in one place.
-   `Codec.Compression.Zlib.Compress`: One-shot compression API.  It provides whole-buffer compression using zlib, gzip, or raw DEFLATE wrappers, with support for configurable compression levels and related options.
-   `Codec.Compression.Zlib.Decompress`: One-shot decompression API.  It accepts complete compressed buffers and expands them using zlib, gzip, or raw DEFLATE semantics, returning an error for malformed, truncated, or wrapper-mismatched input.
-   `Codec.Compression.Zlib.Stream`:  Incremental compression and decompression. It exposes scoped `StreamingCompressor` and `StreamingDecompressor` handles, chunk-based operations such as `compressChunk` / `decompressChunk`, explicit `consumed` counts, produced output chunks, `finished` state, and `finishCompressor` for completing a compression stream.
-  `Codec.Compression.Zlib.Checksum`:  CRC-32 and Adler-32 support. It provides both one-shot checksum functions and incremental update functions for processing data piece-by-piece.
