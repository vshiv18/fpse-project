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
    uint_t sa_len = n + 1;

    // printf("String Len = %d\n", n);
    // printf("String Contents = %s\n", str_c);

    // char *terminated_str = malloc(sa_len * sizeof(char));
    // strcpy(terminated_str, str_c);
    // terminated_str[n] = '0';
    // terminated_str[n + 1] = '\0';

    // printf("New Contents = %s\n", terminated_str);
    // printf("New Length = %d\n", strlen(terminated_str));

    int_t *SA = malloc(sa_len * sizeof(int_t));
    int result = sacak((unsigned char *)str_c, (uint_t*)SA, sa_len);

    if (result >= 0) {
        CAMLlocal2(sa_ocaml, elem);
        sa_ocaml = caml_alloc(sa_len, 0);

        for (size_t i = 0; i < sa_len; i++) {
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
    uint_t sa_len = n + 1;
    int_t *arr = malloc(sa_len * sizeof(int_t));
    for (size_t i = 0; i < n; i++) {
        arr[i] = Int_val(Field(arr_ocaml, i));
    }
    arr[n] = 0;
    uint_t sigma = Int_val(sigma_ocaml) + 2;

    // printf("\nSigma Size = %d\n", sigma);
    // printf("Array Len = %d", n);

    int_t *SA = malloc(sa_len * sizeof(int_t));
    int result = sacak_int((int_text*)arr, (uint_t*)SA, sa_len, sigma + 2);

    if (result >= 0) {
        CAMLlocal2(sa_ocaml, elem);
        sa_ocaml = caml_alloc(sa_len, 0);

        for (size_t i = 0; i < sa_len; i++) {
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
