#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

typedef struct {
    z_stream stream;
    uint32_t consumed;
    uint32_t produced;
} zlib_idris_stream;

zlib_idris_stream *
zlib_idris_stream_new(void)
{
    zlib_idris_stream *s =
        calloc(1, sizeof(zlib_idris_stream));

    return s;
}

void
zlib_idris_stream_free(zlib_idris_stream *s)
{
    free(s);
}

const char *
zlib_idris_version(void)
{
    return zlibVersion();
}

int
zlib_idris_deflate_init(
    zlib_idris_stream *s,
    int level
)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    s->consumed = 0;
    s->produced = 0;

    return deflateInit(&s->stream, level);
}

int
zlib_idris_deflate_init2(
    zlib_idris_stream *s,
    int level,
    int method,
    int window_bits,
    int mem_level,
    int strategy
)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    s->consumed = 0;
    s->produced = 0;

    return deflateInit2(
        &s->stream,
        level,
        method,
        window_bits,
        mem_level,
        strategy
    );
}

int
zlib_idris_deflate_buffer(
    zlib_idris_stream *s,
    uint8_t *input,
    uint32_t input_len,
    uint8_t *output,
    uint32_t output_len,
    int flush
)
{
    int status;

    if (s == NULL)
        return Z_STREAM_ERROR;

    s->stream.next_in =
        (Bytef *)input;

    s->stream.avail_in =
        (uInt)input_len;

    s->stream.next_out =
        (Bytef *)output;

    s->stream.avail_out =
        (uInt)output_len;

    status = deflate(&s->stream, flush);

    s->consumed =
        input_len - (uint32_t)s->stream.avail_in;

    s->produced =
        output_len - (uint32_t)s->stream.avail_out;

    /*
     * These pointers refer to Chez-managed bytevectors.
     *
     * They are valid only during this foreign call and therefore must
     * never remain reachable from the z_stream after we return.
     */
    s->stream.next_in = Z_NULL;
    s->stream.avail_in = 0;

    s->stream.next_out = Z_NULL;
    s->stream.avail_out = 0;

    return status;
}

int
zlib_idris_inflate_buffer(
    zlib_idris_stream *s,
    uint8_t *input,
    uint32_t input_len,
    uint8_t *output,
    uint32_t output_len,
    int flush
)
{
    int status;

    if (s == NULL)
        return Z_STREAM_ERROR;

    s->stream.next_in =
        (Bytef *)input;

    s->stream.avail_in =
        (uInt)input_len;

    s->stream.next_out =
        (Bytef *)output;

    s->stream.avail_out =
        (uInt)output_len;

    status = inflate(&s->stream, flush);

    s->consumed =
        input_len - (uint32_t)s->stream.avail_in;

    s->produced =
        output_len - (uint32_t)s->stream.avail_out;

    s->stream.next_in = Z_NULL;
    s->stream.avail_in = 0;

    s->stream.next_out = Z_NULL;
    s->stream.avail_out = 0;

    return status;
}

uint32_t
zlib_idris_consumed(
    const zlib_idris_stream *s
)
{
    return s == NULL ? 0 : s->consumed;
}

uint32_t
zlib_idris_produced(
    const zlib_idris_stream *s
)
{
    return s == NULL ? 0 : s->produced;
}

int
zlib_idris_deflate_end(zlib_idris_stream *s)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    return deflateEnd(&s->stream);
}

int
zlib_idris_deflate_reset(zlib_idris_stream *s)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    s->consumed = 0;
    s->produced = 0;

    return deflateReset(&s->stream);
}

#define ZLIB_IDRIS_BOUND_ERROR UINT64_MAX

uint64_t
zlib_idris_deflate_bound(
    zlib_idris_stream *s,
    uint64_t source_len
)
{
    uLong bound;

    if (s == NULL)
        return ZLIB_IDRIS_BOUND_ERROR;

    /*
     * Our deflate-buffer bridge eventually stores this length in
     * z_stream.avail_in, whose type is uInt.
     *
     * Do not silently truncate on platforms where the caller's integer
     * representation is wider than zlib's.
     */
    if (source_len > (uint64_t)UINT_MAX)
        return ZLIB_IDRIS_BOUND_ERROR;

    /*
     * deflateBound() takes uLong. This matters in particular on Windows,
     * where unsigned long remains 32 bits even on 64-bit systems.
     *
     * A value accepted by uInt should fit in uLong on the platforms zlib
     * supports, but keep the check explicit rather than depending on that
     * relationship at the FFI boundary.
     */
    if (source_len > (uint64_t)ULONG_MAX)
        return ZLIB_IDRIS_BOUND_ERROR;

    bound = deflateBound(
        &s->stream,
        (uLong)source_len
    );

    /*
     * Current zlib uses (uLong)-1 when the bound cannot be represented.
     * Treat it as an error instead of exposing an ABI-sized sentinel to
     * Idris.
     */
    if (bound == (uLong)-1)
        return ZLIB_IDRIS_BOUND_ERROR;

    /*
     * The resulting capacity is passed back to zlib as avail_out, which is
     * also a uInt. A larger bound cannot be used by our one-shot primitive
     * even if the host can allocate such a buffer.
     */
    if (bound > (uLong)UINT_MAX)
        return ZLIB_IDRIS_BOUND_ERROR;

    /*
     * Defensive overflow check for older zlib implementations.
     */
    if ((uint64_t)bound < source_len)
        return ZLIB_IDRIS_BOUND_ERROR;

    return (uint64_t)bound;
}

int
zlib_idris_inflate_end(zlib_idris_stream *s)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    return inflateEnd(&s->stream);
}

int
zlib_idris_inflate_reset(zlib_idris_stream *s)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    s->consumed = 0;
    s->produced = 0;

    return inflateReset(&s->stream);
}

int
zlib_idris_inflate_init(
    zlib_idris_stream *s
)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    s->consumed = 0;
    s->produced = 0;

    return inflateInit(&s->stream);
}

int
zlib_idris_inflate_init2(
    zlib_idris_stream *s,
    int window_bits
)
{
    if (s == NULL)
        return Z_STREAM_ERROR;

    s->consumed = 0;
    s->produced = 0;

    return inflateInit2(
        &s->stream,
        window_bits
    );
}

uint32_t
zlib_idris_crc32(
    uint32_t crc,
    const uint8_t *input,
    uint32_t input_len
)
{
    return (uint32_t)crc32(
        (uLong)crc,
        (const Bytef *)input,
        (uInt)input_len
    );
}

uint32_t
zlib_idris_adler32(
    uint32_t adler,
    const uint8_t *input,
    uint32_t input_len
)
{
    return (uint32_t)adler32(
        (uLong)adler,
        (const Bytef *)input,
        (uInt)input_len
    );
}
