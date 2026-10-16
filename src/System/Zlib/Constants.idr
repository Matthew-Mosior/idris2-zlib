module System.Zlib.Constants

||| Indicates that a zlib operation completed successfully.
|||
export
Z_OK : Int
Z_OK = 0

||| Indicates that a compression or decompression stream has reached the end of the compressed data stream.
|||
export
Z_STREAM_END : Int
Z_STREAM_END = 1

||| Indicates that the current chunk of input data is the final block of the stream.
|||
export
Z_FINISH : Int
Z_FINISH = 4

||| Indicates that a preset dictionary is required to continue decompression.
|||
export
Z_NEED_DICT : Int
Z_NEED_DICT = 2

||| Indicates that zlib encountered an error in the underlying input/output operation.
|||
export
Z_ERRNO : Int
Z_ERRNO = (-1)

||| Indicates that the zlib stream state is inconsistent or that an invalid parameter was supplied to a zlib operation.
|||
export
Z_STREAM_ERROR : Int
Z_STREAM_ERROR = (-2)

||| Indicates that the compressed data is corrupted or otherwise invalid for the selected decompression operation.
|||
export
Z_DATA_ERROR : Int
Z_DATA_ERROR = (-3)

||| Indicates that zlib was unable to allocate the memory required to complete the requested operation.
|||
export
Z_MEM_ERROR : Int
Z_MEM_ERROR = (-4)

||| Indicates that the supplied output buffer was too small to complete the requested operation.
|||
export
Z_BUF_ERROR : Int
Z_BUF_ERROR = (-5)

||| Indicates that the zlib version supplied to an initialization function is incompatible with the linked zlib implementation.
|||
export
Z_VERSION_ERROR : Int
Z_VERSION_ERROR = (-6)

||| The DEFLATE compression method used by zlib.
|||
export
Z_DEFLATED : Int
Z_DEFLATED = 8

||| The default compression strategy.
|||
export
Z_DEFAULT_STRATEGY : Int
Z_DEFAULT_STRATEGY = 0

||| The default memory level used by `deflateInit2`.
|||
export
Z_DEFAULT_MEM_LEVEL : Int
Z_DEFAULT_MEM_LEVEL = 8
