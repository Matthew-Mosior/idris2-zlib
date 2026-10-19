module System.Zlib.Constant

||| Constants that zlib defines.
|||
public export
data ZlibConstant
  = Z_OK                -- Indicates that a zlib operation completed successfully.
  | Z_STREAM_END        -- Indicates that a compression or decompression stream has reached the end of the compressed data stream.
  | Z_FINISH            -- Indicates that the current chunk of input data is the final block of the stream.
  | Z_NEED_DICT         -- Indicates that a preset dictionary is required to continue decompression.
  | Z_ERRNO             -- Indicates that zlib encountered an error in the underlying input/output operation.
  | Z_STREAM_ERROR      -- Indicates that the zlib stream state is inconsistent or that an invalid parameter was supplied to a zlib operation.
  | Z_DATA_ERROR        -- Indicates that the compressed data is corrupted or otherwise invalid for the selected decompression operation.
  | Z_MEM_ERROR         -- Indicates that zlib was unable to allocate the memory required to complete the requested operation.
  | Z_BUF_ERROR         -- Indicates that the supplied output buffer was too small to complete the requested operation.
  | Z_VERSION_ERROR     -- Indicates that the zlib version supplied to an initialization function is incompatible with the linked zlib implementation.
  | Z_DEFLATED          -- The DEFLATE compression method used by zlib.
  | Z_DEFAULT_STRATEGY  -- The default compression strategy.
  | Z_DEFAULT_MEM_LEVEL -- The default memory level used by `deflateInit2`.

export
zlibconstantToInt :  ZlibConstant
                  -> Int
zlibconstantToInt Z_OK                = 0
zlibconstantToInt Z_STREAM_END        = 1
zlibconstantToInt Z_FINISH            = 4
zlibconstantToInt Z_NEED_DICT         = 2
zlibconstantToInt Z_ERRNO             = -1
zlibconstantToInt Z_STREAM_ERROR      = -2
zlibconstantToInt Z_DATA_ERROR        = -3
zlibconstantToInt Z_MEM_ERROR         = -4
zlibconstantToInt Z_BUF_ERROR         = -5
zlibconstantToInt Z_VERSION_ERROR     = -6
zlibconstantToInt Z_DEFLATED          = 8
zlibconstantToInt Z_DEFAULT_STRATEGY  = 0
zlibconstantToInt Z_DEFAULT_MEM_LEVEL = 8
