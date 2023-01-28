/***********************************************************************/
/*                                                                     */
/*                      The Cryptokit library                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id: stubs-aes.c 53 2010-08-30 10:53:00Z gildor-admin $ */

/* Stub code for AES */

#include "rijndael-alg-fst.h"
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define Cooked_key_NR_offset ((4 * (MAXNR + 1)) * sizeof(u32))
#define Cooked_key_size (Cooked_key_NR_offset + 1)

CAMLprim value camlpdf_caml_aes_cook_encrypt_key(value key)
{
  CAMLparam1(key);
  value ckey = caml_alloc_string(Cooked_key_size);
  int nr =
    camlpdf_rijndaelKeySetupEnc((u32 *) String_val(ckey),
                        (const u8 *) String_val(key),
                        8 * caml_string_length(key));
  Byte(ckey, Cooked_key_NR_offset) = nr;
  CAMLreturn(ckey);
}

CAMLprim value camlpdf_caml_aes_cook_decrypt_key(value key)
{
  CAMLparam1(key);
  value ckey = caml_alloc_string(Cooked_key_size);
  int nr =
    camlpdf_rijndaelKeySetupDec((u32 *) String_val(ckey),
                        (const u8 *) String_val(key),
                        8 * caml_string_length(key));
  Byte(ckey, Cooked_key_NR_offset) = nr;
  CAMLreturn(ckey);
}

CAMLprim value camlpdf_caml_aes_encrypt(value ckey, value src, value src_ofs,
                                value dst, value dst_ofs)
{
  camlpdf_rijndaelEncrypt((const u32 *) String_val(ckey),
                  Byte(ckey, Cooked_key_NR_offset),
                  (const u8 *) &Byte(src, Long_val(src_ofs)),
                  (u8 *) &Byte(dst, Long_val(dst_ofs)));
  return Val_unit;
}

CAMLprim value camlpdf_caml_aes_decrypt(value ckey, value src, value src_ofs,
                                value dst, value dst_ofs)
{
  camlpdf_rijndaelDecrypt((const u32 *) String_val(ckey),
                  Byte(ckey, Cooked_key_NR_offset),
                  (const u8 *) &Byte(src, Long_val(src_ofs)),
                  (u8 *) &Byte(dst, Long_val(dst_ofs)));
  return Val_unit;
}

