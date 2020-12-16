/* Modifed minutely for CamlPDF*/

/***********************************************************************/
/*                                                                     */
/*                      The CamlZip library                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id: zlibstubs.c,v 1.3 2006/04/04 08:29:07 xleroy Exp $ */

/* Stub code to interface with Zlib */

#include "miniz.c" //Modified to use miniz.c rather than zlib 

#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#define ZStream_val(v) ((z_stream *) (v))

static const value * camlzip_error_exn = NULL;

static void camlzip_error(char * fn, value vzs)
{
  char * msg;
  value s1 = Val_unit, s2 = Val_unit, bucket = Val_unit;

  msg = ZStream_val(vzs)->msg;
  if (msg == NULL) msg = "";
  if (camlzip_error_exn == NULL) {
    camlzip_error_exn = caml_named_value("Zlib.Error");
    if (camlzip_error_exn == NULL)
      invalid_argument("Exception Zlib.Error not initialized");
  }
  Begin_roots3(s1, s2, bucket);
    s1 = copy_string(fn);
    s2 = copy_string(msg);
    bucket = alloc_small(3, 0);
    Field(bucket, 0) = *camlzip_error_exn;
    Field(bucket, 1) = s1;
    Field(bucket, 2) = s2;
  End_roots();
  mlraise(bucket);
}

static value camlzip_new_stream(void)
{
  value res = alloc((sizeof(z_stream) + sizeof(value) - 1) / sizeof(value),
                    Abstract_tag);
  ZStream_val(res)->zalloc = NULL;
  ZStream_val(res)->zfree = NULL;
  ZStream_val(res)->opaque = NULL;
  ZStream_val(res)->next_in = NULL;
  ZStream_val(res)->next_out = NULL;
  return res;
}

value camlzip_deflateInit(value vlevel, value expect_header)
{
  value vzs = camlzip_new_stream();
  if (deflateInit2(ZStream_val(vzs),
                   Int_val(vlevel),
                   Z_DEFLATED,
                   Bool_val(expect_header) ? MAX_WBITS : -MAX_WBITS,
                   8,
                   Z_DEFAULT_STRATEGY) != Z_OK)
    camlzip_error("Zlib.deflateInit", vzs);
  return vzs;
}

static int camlzip_flush_table[] = 
{ Z_NO_FLUSH, Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH };

value camlzip_deflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  z_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = deflate(zs, camlzip_flush_table[Int_val(vflush)]);
  if (retcode < 0) camlzip_error("Zlib.deflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;         /* not required, but cleaner */
  zs->next_out = NULL;        /* (avoid dangling pointers into Caml heap) */
  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlzip_deflate_bytecode(value * arg, int nargs)
{
  return camlzip_deflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_deflateEnd(value vzs)
{
  if (deflateEnd(ZStream_val(vzs)) != Z_OK)
    camlzip_error("Zlib.deflateEnd", vzs);
  return Val_unit;
}

/* CamlZIP now treats Z_BUF_ERROR as non-fatal. However, this can lead to lack
 * of progress on some malformed streams (at least with miniz.c -- maybe not
 * zlib. So we have this hack. */
int buf_error_count;

value camlzip_inflateInit(value expect_header)
{
  buf_error_count = 0;
  value vzs = camlzip_new_stream();
  if (inflateInit2(ZStream_val(vzs),
                   Bool_val(expect_header) ? MAX_WBITS : -MAX_WBITS) != Z_OK)
    camlzip_error("Zlib.inflateInit", vzs);
  return vzs;
}

value camlzip_inflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  z_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = inflate(zs, camlzip_flush_table[Int_val(vflush)]);
  if (retcode == Z_BUF_ERROR) buf_error_count += 1; else buf_error_count = 0;
  if (retcode < 0 && retcode != Z_BUF_ERROR ||
      retcode == Z_BUF_ERROR && buf_error_count > 1 ||
      retcode == Z_NEED_DICT)
    camlzip_error("Zlib.inflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;           /* not required, but cleaner */
  zs->next_out = NULL;          /* (avoid dangling pointers into Caml heap) */
  res = caml_alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlzip_inflate_bytecode(value * arg, int nargs)
{
  return camlzip_inflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_inflateEnd(value vzs)
{
  if (inflateEnd(ZStream_val(vzs)) != Z_OK)
    camlzip_error("Zlib.inflateEnd", vzs);
  return Val_unit;
}

value camlzip_update_crc32(value crc, value buf, value pos, value len)
{
  return copy_int32(crc32((uint32_t) Int32_val(crc), 
                          &Byte_u(buf, Long_val(pos)),
                          Long_val(len)));
}
