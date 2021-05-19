#include "sha2.h"
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value camlpdf_caml_sha256(value message)
{
  CAMLparam1(message);
  value digest = alloc_string(32);
  camlpdf_sha256((const unsigned char *) (String_val(message)),
         caml_string_length(message),
         ((unsigned char *) String_val(digest)));
  CAMLreturn(digest);
}

CAMLprim value camlpdf_caml_sha384(value message)
{
  CAMLparam1(message);
  value digest = alloc_string(48);
  camlpdf_sha384((const unsigned char *) (String_val(message)),
         caml_string_length(message),
         ((unsigned char *) String_val(digest)));
  CAMLreturn(digest);
}

CAMLprim value camlpdf_caml_sha512(value message)
{
  CAMLparam1(message);
  value digest = alloc_string(64);
  camlpdf_sha512((const unsigned char *) (String_val(message)),
         caml_string_length(message),
         ((unsigned char *) String_val(digest)));
  CAMLreturn(digest);
}
