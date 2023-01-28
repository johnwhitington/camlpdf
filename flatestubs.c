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

#define ZStream_val(v) ((mz_stream *) (v))

// So that the code links ok when using js_of_ocaml
char* camlpdf_caml_zlib_decompress(char *s) { return s; }
char* camlpdf_caml_zlib_compress(char *s) { return s; }

static const value * camlpdf_camlzip_error_exn = NULL;

static void camlpdf_camlzip_error(char * fn, value vzs)
{
  char * msg;
  value s1 = Val_unit, s2 = Val_unit, bucket = Val_unit;

  msg = ZStream_val(vzs)->msg;
  if (msg == NULL) msg = "";
  if (camlpdf_camlzip_error_exn == NULL) {
    camlpdf_camlzip_error_exn = caml_named_value("Pdfflate.Error");
    if (camlpdf_camlzip_error_exn == NULL)
      caml_invalid_argument("Exception Pdfflate.Error not initialized");
  }
  Begin_roots3(s1, s2, bucket);
    s1 = caml_copy_string(fn);
    s2 = caml_copy_string(msg);
    bucket = caml_alloc_small(3, 0);
    Field(bucket, 0) = *camlpdf_camlzip_error_exn;
    Field(bucket, 1) = s1;
    Field(bucket, 2) = s2;
  End_roots();
  caml_raise(bucket);
}

static value camlpdf_camlzip_new_stream(void)
{
  value res = caml_alloc((sizeof(mz_stream) + sizeof(value) - 1) / sizeof(value),
                    Abstract_tag);
  ZStream_val(res)->zalloc = NULL;
  ZStream_val(res)->zfree = NULL;
  ZStream_val(res)->opaque = NULL;
  ZStream_val(res)->next_in = NULL;
  ZStream_val(res)->next_out = NULL;
  return res;
}

value camlpdf_camlzip_deflateInit(value vlevel, value expect_header)
{
  value vzs = camlpdf_camlzip_new_stream();
  if (mz_deflateInit2(ZStream_val(vzs),
                   Int_val(vlevel),
                   MZ_DEFLATED,
                   Bool_val(expect_header) ? 15 : -15,
                   8,
                   MZ_DEFAULT_STRATEGY) != MZ_OK)
    camlpdf_camlzip_error("Zlib.deflateInit", vzs);
  return vzs;
}

static int camlpdf_camlzip_flush_table[] = 
{ MZ_NO_FLUSH, MZ_SYNC_FLUSH, MZ_FULL_FLUSH, MZ_FINISH };

value camlpdf_camlzip_deflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  mz_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = mz_deflate(zs, camlpdf_camlzip_flush_table[Int_val(vflush)]);
  if (retcode < 0) camlpdf_camlzip_error("Zlib.deflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;         /* not required, but cleaner */
  zs->next_out = NULL;        /* (avoid dangling pointers into Caml heap) */
  res = caml_alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == MZ_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlpdf_camlzip_deflate_bytecode(value * arg, int nargs)
{
  return camlpdf_camlzip_deflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlpdf_camlzip_deflateEnd(value vzs)
{
  if (mz_deflateEnd(ZStream_val(vzs)) != MZ_OK)
    camlpdf_camlzip_error("Zlib.deflateEnd", vzs);
  return Val_unit;
}

/* CamlZIP now treats Z_BUF_ERROR as non-fatal. However, this can lead to lack
 * of progress on some malformed streams (at least with miniz.c -- maybe not
 * zlib. So we have this hack. */
int camlpdf_buf_error_count;

value camlpdf_camlzip_inflateInit(value expect_header)
{
  camlpdf_buf_error_count = 0;
  value vzs = camlpdf_camlzip_new_stream();
  if (mz_inflateInit2(ZStream_val(vzs),
                   Bool_val(expect_header) ? 15 : -15) != MZ_OK)
    camlpdf_camlzip_error("Zlib.inflateInit", vzs);
  return vzs;
}

value camlpdf_camlzip_inflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  mz_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = mz_inflate(zs, camlpdf_camlzip_flush_table[Int_val(vflush)]);
  if (retcode == MZ_BUF_ERROR) camlpdf_buf_error_count += 1; else camlpdf_buf_error_count = 0;
  if (retcode < 0 && retcode != MZ_BUF_ERROR ||
      retcode == MZ_BUF_ERROR && camlpdf_buf_error_count > 1 ||
      retcode == MZ_NEED_DICT)
    camlpdf_camlzip_error("Zlib.inflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;           /* not required, but cleaner */
  zs->next_out = NULL;          /* (avoid dangling pointers into Caml heap) */
  res = caml_alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == MZ_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlpdf_camlzip_inflate_bytecode(value * arg, int nargs)
{
  return camlpdf_camlzip_inflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlpdf_camlzip_inflateEnd(value vzs)
{
  if (mz_inflateEnd(ZStream_val(vzs)) != MZ_OK)
    camlpdf_camlzip_error("Zlib.inflateEnd", vzs);
  return Val_unit;
}

value camlpdf_camlzip_update_crc32(value crc, value buf, value pos, value len)
{
  return caml_copy_int32(mz_crc32((uint32_t) Int32_val(crc), 
                          &Byte_u(buf, Long_val(pos)),
                          Long_val(len)));
}
