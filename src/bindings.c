/* Used these online resources to create the bindings file and configure dune:
https://v2.ocaml.org/manual/intfc.html
https://jbuilder.readthedocs.io/en/latest/quick-start.html#defining-a-library-with-c-stubs
*/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include "gsacak.h" 

CAMLprim value sacak_ocaml(value str_ocaml) {
    CAMLparam1(str_ocaml);

    const char *str_c = String_val(str_ocaml);
    uint_t n = caml_string_length(str_ocaml);

    int_t *SA = malloc((n + 1) * sizeof(int_t));
    int result = sacak((unsigned char *)str_c, (uint_t*)SA, n + 1);

    if (result >= 0) {
        CAMLlocal2(sa_ocaml, elem);
        sa_ocaml = caml_alloc(n + 1, 0);

        for (size_t i = 0; i < n + 1; i++) {
            elem = Val_int(SA[i]);
            Store_field(sa_ocaml, i, elem);
        }

        // Freeing the allocated memory for SA
        free(SA);

        CAMLreturn(caml_alloc_some(sa_ocaml));
    }
    else {
        free(SA);
        CAMLreturn(Val_none);
    }
}

CAMLprim value sacak_int_ocaml(value arr_ocaml, value sigma_ocaml) {
    CAMLparam2(arr_ocaml, sigma_ocaml);

    uint_t n = caml_array_length(arr_ocaml);
    int_t *arr = malloc(n * sizeof(int_t));
    for (size_t i = 0; i < n; i++) {
        arr[i] = Int_val(Field(arr_ocaml, i));
    }
    int sigma = Int_val(sigma_ocaml);

    int_t *SA = malloc((n + 1) * sizeof(int_t));
    int result = sacak_int((int_text*)arr, (uint_t*)SA, n + 1, sigma + 1);

    if (result >= 0) {
        CAMLlocal2(sa_ocaml, elem);
        sa_ocaml = caml_alloc(n + 1, 0);

        for (size_t i = 0; i < n + 1; i++) {
            elem = Val_int(SA[i]);
            Store_field(sa_ocaml, i, elem);
        }

        // Freeing the allocated memory for SA
        free(SA);

        CAMLreturn(caml_alloc_some(sa_ocaml));
    }
    else {
        free(SA);
        CAMLreturn(Val_none);
    }
}
