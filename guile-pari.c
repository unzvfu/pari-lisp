/* -*- compile-command: "gcc `pkg-config --cflags guile-2.0` -shared -o libguile-pari.so -fPIC guile-pari.c" -*- */
/*
 * Taken from Section 2.4 of the Guile manual.
 */

#include <math.h>
#include <libguile.h>

SCM
abs_wrapper(SCM x)
{
    return scm_from_double(abs(scm_to_double(x)));
}


void
initialise(void)
{
    scm_c_define_gsubr("abs", 1, 0, 0, abs_wrapper);
}
