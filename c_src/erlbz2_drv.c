/*
 * Copyright Smarkets Limited 2011.
 *
 * The Original Code was produced by Ericsson AB under the license below:
 *
 * Copyright Ericsson AB 2003-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 */
#include <stdio.h>
#include <bzlib.h>
#include <errno.h>
#include <string.h>

#include "erl_driver.h"

#define BZ_NO_STDIO

#define COMPRESS_INIT   1
#define COMPRESS        2
#define COMPRESS_END    3
#define DECOMPRESS_INIT 4
#define DECOMPRESS      5
#define DECOMPRESS_END  6
#define SET_BUFSZ       7
#define GET_BUFSZ       8
#define GET_QSIZE       9

#define DEFAULT_BUFSZ   4000

#define BZ_ERRNO        (-9999)

static ErlDrvData bz_start(ErlDrvPort port, char *buf);
static void bz_stop(ErlDrvData e);
static ErlDrvSSizeT bz_ctl(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);
static void bz_outputv(ErlDrvData drv_data, ErlIOVec *ev);

ErlDrvEntry erlbz2_driver_entry = {
    NULL,
    bz_start,
    bz_stop,
    NULL,                           /* output */
    NULL,                           /* ready_input */
    NULL,                           /* ready_output */
    "erlbz2_drv",
    NULL,                           /* finish */
    NULL,                           /* handle */
    bz_ctl,
    NULL,                           /* timeout */
    bz_outputv,
    NULL,                           /* read_async */
    NULL,                           /* flush */
    NULL,                           /* call */
    NULL,                           /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,                           /* handle2 */
    NULL,                           /* process_exit */
};

DRIVER_INIT(erlbz2_drv)
{
    return &erlbz2_driver_entry;
}

typedef enum {
    ST_NONE       = 0,
    ST_COMPRESS   = 1,
    ST_DECOMPRESS = 2
} BZState;

typedef struct {
    bz_stream s;
    BZState state;
    ErlDrvBinary* bin;
    int binsz;
    int binsz_need;
    int stream_end_seen;
    ErlDrvPort port;
} BZData;

static int bz_decompress(BZData* d);
static int bz_compress(BZData* d);

static inline int i32(char* buf)
{
    return (int) (
        (((int)((unsigned char*)buf)[0]) << 24) |
        (((int)((unsigned char*)buf)[1]) << 16) |
        (((int)((unsigned char*)buf)[2]) << 8) |
        (((int)((unsigned char*)buf)[3]) << 0));
}

static char* bz_reason(int code, int* err)
{
    switch(code) {
    case BZ_OK:
        *err = 0;
        return "ok";
    case BZ_RUN_OK:
        *err = 0;
        return "ok";
    case BZ_FLUSH_OK:
        *err = 0;
        return "ok";
    case BZ_FINISH_OK:
        *err = 0;
        return "ok";
    case BZ_STREAM_END:
        *err = 0;
        return "stream_end";
    case BZ_CONFIG_ERROR:
        *err = 1;
        return "config_error";
    case BZ_SEQUENCE_ERROR:
        *err = 1;
        return "sequence_error";
    case BZ_PARAM_ERROR:
        *err = 1;
        return "param_error";
    case BZ_MEM_ERROR:
        *err = 1;
        return "mem_error";
    case BZ_DATA_ERROR:
        *err = 1;
        return "data_error";
    case BZ_DATA_ERROR_MAGIC:
        *err = 1;
        return "data_error_magic";
    case BZ_IO_ERROR:
        *err = 1;
        return erl_errno_id(errno);
    case BZ_UNEXPECTED_EOF:
        *err = 1;
        return "unexpected_eof";
    case BZ_OUTBUFF_FULL:
        *err = 1;
        return "outbuff_full";
    case BZ_ERRNO:
        *err = 1;
        return erl_errno_id(errno);
    default:
        *err = 1;
        return "unknown_error";
    }
}

static int bz_return(int code, char **rbuf, int rlen)
{
    int msg_code = 0; /* 0=ok, 1=error */
    char *dst = *rbuf;
    char *src;
    int len = 0;

    src = bz_reason(code, &msg_code);
    *dst++ = msg_code;
    rlen--;
    len = 1;

    while ((rlen > 0) && *src) {
        *dst++ = *src++;
        rlen--;
        len++;
    }
    return len;
}

static int bz_value2(int msg_code, int value, char** rbuf, int rlen)
{
    char* dst = *rbuf;

    if (rlen  < 5) {
        return -1;
    }
    *dst++ = msg_code;
    *dst++ = (value >> 24) & 0xff;
    *dst++ = (value >> 16) & 0xff;
    *dst++ = (value >> 8) & 0xff;
    *dst++ = value & 0xff;
    return 5;
}

static int bz_value(int value, char** rbuf, int rlen)
{
    return bz_value2(2, value, rbuf, rlen);
}

static int bz_output_init(BZData* d)
{
    if (d->bin != NULL)
        driver_free_binary(d->bin);
    if ((d->bin = driver_alloc_binary(d->binsz_need)) == NULL)
        return -1;
    d->binsz = d->binsz_need;
    d->s.next_out = (char*)d->bin->orig_bytes;
    d->s.avail_out = d->binsz;
    return 0;
}

/*
 * Send compressed or uncompressed data
 * and restart output procesing
 */
static int bz_output(BZData* d)
{
    if (d->bin != NULL) {
        int len = d->binsz - d->s.avail_out;
        if (len > 0) {
            if (driver_output_binary(d->port, NULL, 0, d->bin, 0, len) < 0)
                return -1;
        }
        driver_free_binary(d->bin);
        d->bin = NULL;
        d->binsz = 0;
    }
    return bz_output_init(d);
}

static int bz_decompress(BZData* d)
{
    int res = BZ_OK;

    if ((d->bin == NULL) && (bz_output_init(d) < 0)) {
        errno = ENOMEM;
        return BZ_ERRNO;
    }

    while ((driver_sizeq(d->port) > 0) && (res != BZ_STREAM_END)) {
        int vlen;
        SysIOVec* iov = driver_peekq(d->port, &vlen);
        int len;
        int possibly_more_output = 0;

        d->s.next_in = iov[0].iov_base;
        d->s.avail_in = iov[0].iov_len;
        while ((possibly_more_output || (d->s.avail_in > 0)) && (res != BZ_STREAM_END)) {
            res = BZ2_bzDecompress(&d->s);
            if (res < 0) {
                return res;
            }
            if (d->s.avail_out != 0) {
                possibly_more_output = 0;
            } else {
                bz_output(d);
                possibly_more_output = 1;
            }
        }
        len = iov[0].iov_len - d->s.avail_in;
        driver_deq(d->port, len);
    }
    bz_output(d);
    if (res == BZ_STREAM_END) {
        d->stream_end_seen = 1;
    }
    return res;
}

static int bz_compress(BZData* d)
{
    int res = BZ_OK;

    if ((d->bin == NULL) && (bz_output_init(d) < 0)) {
        errno = ENOMEM;
        return BZ_ERRNO;
    }

    while ((driver_sizeq(d->port) > 0) && (res != BZ_STREAM_END)) {
        int vlen;
        SysIOVec* iov = driver_peekq(d->port, &vlen);
        int len;

        d->s.next_in = iov[0].iov_base;
        d->s.avail_in = iov[0].iov_len;

        while((d->s.avail_in > 0) && (res != BZ_STREAM_END)) {
            if ((res = BZ2_bzCompress(&d->s, BZ_RUN)) < 0) {
                return res;
            }
            if (d->s.avail_out == 0) {
                bz_output(d);
            }
        }
        len = iov[0].iov_len - d->s.avail_in;
        driver_deq(d->port, len);
    }

    if ((res = BZ2_bzCompress(&d->s, BZ_FINISH)) < 0) {
        return res;
    }
    while (d->s.avail_out < d->binsz) {
        bz_output(d);
        if (res == BZ_STREAM_END) {
            break;
        }
        if ((res = BZ2_bzCompress(&d->s, BZ_FINISH)) < 0) {
            return res;
        }
    }
    return res;
}

static void *bz_alloc(void *data, int items, int size)
{
    return (void*)driver_alloc(items * size);
}

static void bz_free(void *data, void *addr)
{
    driver_free(addr);
}

static ErlDrvData bz_start(ErlDrvPort port, char* buf)
{
    BZData* d;

    if ((d = (BZData*) driver_alloc(sizeof(BZData))) == NULL)
        return ERL_DRV_ERROR_GENERAL;

    memset(&d->s, 0, sizeof(bz_stream));

    d->s.bzalloc = bz_alloc;
    d->s.bzfree  = bz_free;
    d->s.opaque = d;

    d->port      = port;
    d->state     = ST_NONE;
    d->bin       = NULL;
    d->binsz     = 0;
    d->binsz_need = DEFAULT_BUFSZ;
    d->stream_end_seen = 0;
    return (ErlDrvData)d;
}

static void bz_stop(ErlDrvData e)
{
    BZData* d = (BZData*)e;

    if (d->state == ST_COMPRESS) {
        BZ2_bzCompressEnd(&d->s);
    } else if (d->state == ST_DECOMPRESS) {
        BZ2_bzDecompressEnd(&d->s);
    }

    if (d->bin != NULL) {
        driver_free_binary(d->bin);
    }

    driver_free(d);
}

static ErlDrvSSizeT bz_ctl(ErlDrvData drv_data, unsigned int command, char *buf,
                           ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    BZData* d = (BZData*)drv_data;
    int res;
    switch(command) {
    case COMPRESS_INIT:
        if (d->state != ST_NONE) goto badarg;
        if (len != 8) goto badarg;
        res = BZ2_bzCompressInit(&d->s, i32(buf), 0, i32(buf + 4));
        if (res == BZ_OK) {
            d->state = ST_COMPRESS;
        }
        return bz_return(res, rbuf, rlen);

    case COMPRESS:
        if (len != 0) goto badarg;
        if (d->state != ST_COMPRESS) goto badarg;
        res = bz_compress(d);
        return bz_return(res, rbuf, rlen);

    case COMPRESS_END:
        if (len != 0) goto badarg;
        if (d->state != ST_COMPRESS) goto badarg;
        driver_deq(d->port, driver_sizeq(d->port));
        res = BZ2_bzCompressEnd(&d->s);
        d->state = ST_NONE;
        return bz_return(res, rbuf, rlen);

    case DECOMPRESS_INIT:
        if (len != 4) goto badarg;
        if (d->state != ST_NONE) goto badarg;
        res = BZ2_bzDecompressInit(&d->s, 0, i32(buf));
        if (res == BZ_OK) {
            d->state = ST_DECOMPRESS;
            d->stream_end_seen = 0;
        }
        return bz_return(res, rbuf, rlen);

    case DECOMPRESS:
        if (len != 0) goto badarg;
        if (d->state != ST_DECOMPRESS) goto badarg;
        res = bz_decompress(d);
        return bz_return(res, rbuf, rlen);

    case DECOMPRESS_END:
        if (len != 0) goto badarg;
        if (d->state != ST_DECOMPRESS) goto badarg;
        res = BZ2_bzDecompressEnd(&d->s);
        d->state = ST_NONE;
        return bz_return(res, rbuf, rlen);

    case GET_QSIZE:
        return bz_value(driver_sizeq(d->port), rbuf, rlen);

    case GET_BUFSZ:
        return bz_value(d->binsz_need, rbuf, rlen);

    case SET_BUFSZ: {
        int need;
        if (len != 4) goto badarg;
        need = i32(buf);
        if ((need < 16) || (need > 0x00ffffff))
            goto badarg;
        if (d->binsz_need != need) {
            d->binsz_need = need;
            if (d->bin != NULL) {
                if (d->s.avail_out == d->binsz) {
                    driver_free_binary(d->bin);
                    d->bin = NULL;
                    d->binsz = 0;
                }
                else
                    bz_output(d);
            }
        }
        return bz_return(BZ_OK, rbuf, rlen);
    }
    }

  badarg:
    errno = EINVAL;
    return bz_return(BZ_ERRNO, rbuf, rlen);
}

static void bz_outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
    BZData *d = (BZData*)drv_data;
    driver_enqv(d->port, ev, 0);
}
