/*
 * -*- compile-command: "gcc -o pari-scm pari-scm.c `pkg-config --cflags --libs guile-2.0` -lpari -lgmp" -*-
 *
 * -*- compile-command: "gcc -L ~/src/pari-git/GP.dbg/lib -I ~/src/pari-git/GP.dbg/include -o pari-scm pari-scm.c `pkg-config --cflags --libs guile-2.0` -lpari -lgmp" -*-
 *
 */

/*
 * Based on example from Section 2.3 of the Guile manual.
 *
 * Smob stuff is from Section 5.5.
 */

#include <pari/pari.h>
#include <libguile.h>

static scm_t_bits gen_tag;

typedef struct {
    GEN data_as_gen;
    SCM data_as_scm;
    int syncronised; // FIXME: Use SCM_SMOB_FLAGS and SCM_SET_SMOB_FLAGS instead.
} SCM_GEN;


static SCM
make_gen(SCM number)
{
    GEN data = stoi(scm_to_long(number));
    SCM smob;

    SCM_NEWSMOB(smob, gen_tag, data);

    return smob;
}


static SCM
make_gp(SCM s)
{
    if ( ! scm_is_string(s)) {
        // FIXME: Raise an error.
    }

    const char *str = scm_to_locale_string(s);
    GEN res = gp_read_str(str);
    SCM smob;
    SCM_NEWSMOB(smob, gen_tag, res);
    return smob;
}


static int
print_gen(SCM gen_smob, SCM port, scm_print_state *pstate)
{
    (void) pstate;

    GEN data = (GEN) SCM_SMOB_DATA(gen_smob);
    char *data_str = pari_sprintf("#<GEN %Ps >", data); //GENtostr(data);
    scm_puts(data_str, port);
    free(data_str);
    return 1;
}


static SCM
equalp_gen(SCM lhs_smob, SCM rhs_smob)
{
    GEN lhs = (GEN) SCM_SMOB_DATA(lhs_smob);
    GEN rhs = (GEN) SCM_SMOB_DATA(rhs_smob);
    return gequal(lhs, rhs) ? SCM_BOOL_T : SCM_BOOL_F;
}


static SCM
gen_factor(SCM x)
{
    if ( ! SCM_SMOB_PREDICATE(gen_tag, x))
        x = make_gen(x);
    GEN n = (GEN) SCM_SMOB_DATA(x);
    GEN fact = absi_factor(n);
    SCM res;
    SCM_NEWSMOB(res, gen_tag, fact);
    return res;
}


static SCM
gen_length(SCM x)
{
    if (SCM_SMOB_PREDICATE(gen_tag, x)) {
        GEN data = (GEN) SCM_SMOB_DATA(x);
        return scm_from_signed_integer(glength(data));
    }
    return scm_length(x);
}


static SCM
gen_integerp(SCM x)
{
    if (SCM_SMOB_PREDICATE(gen_tag, x)) {
        GEN gen_x = (GEN) SCM_SMOB_DATA(x);
        return typ(gen_x) == t_INT ? SCM_BOOL_T : SCM_BOOL_F;
    }
    return scm_integer_p(x);
}


static SCM
gen_oddp(SCM x)
{
    if (SCM_SMOB_PREDICATE(gen_tag, x)) {
        GEN gen_x = (GEN) SCM_SMOB_DATA(x);
        return mpodd(gen_x) ? SCM_BOOL_T : SCM_BOOL_F;
    }
    return scm_odd_p(x);
}


static SCM
gen_evenp(SCM x)
{
    return gen_oddp(x) == SCM_BOOL_T ? SCM_BOOL_F : SCM_BOOL_T;
}


static SCM
gen_quotient(SCM n, SCM d)
{
    //if ( ! SCM_SMOB_PREDICATE(gen_tag, n) &&  ! SCM_SMOB_PREDICATE(gen_tag, d))
    //    return scm_quotient(n, d);
    if ( ! SCM_SMOB_PREDICATE(gen_tag, n))
        n = make_gen(n);
    if ( ! SCM_SMOB_PREDICATE(gen_tag, d))
        d = make_gen(d);
    GEN gen_n = (GEN) SCM_SMOB_DATA(n);
    GEN gen_d = (GEN) SCM_SMOB_DATA(d);
    GEN gen_res = mpdiv(gen_n, gen_d);
    SCM res;
    SCM_NEWSMOB(res, gen_tag, gen_res);
    return res;
}


#define get_gen(gen, scm)                               \
    do {                                                \
        if ( ! SCM_SMOB_PREDICATE(gen_tag, scm))        \
            scm = make_gen(scm);                        \
        gen = (GEN) SCM_SMOB_DATA(scm);                 \
    } while (0)                                         \


static SCM
gen_sum(SCM x, SCM y)
{
    GEN gen_x, gen_y;
    get_gen(gen_x, x);
    get_gen(gen_y, y);

    SCM res;
    SCM_NEWSMOB(res, gen_tag, gadd(gen_x, gen_y));
    return res;
}


static SCM
gen_sum_i(SCM x, SCM y, SCM rest)
{
    printf("In %s().\n", __func__);
    if (SCM_UNBNDP(x))
        return scm_from_int(0);
    if (SCM_UNBNDP(y))
        return x;

    while ( ! scm_is_null(rest)) {
        x = gen_sum(x, y);
        y = scm_car(rest);
        rest = scm_cdr(rest);
    }

    return gen_sum(x, y);
}


void
init_gen_type(void)
{
    gen_tag = scm_make_smob_type("GEN", sizeof(GEN));
    scm_set_smob_print(gen_tag, print_gen);
    scm_set_smob_equalp(gen_tag, equalp_gen);

    scm_c_define_gsubr("make-gen", 1, 0, 0, make_gen);
    scm_c_define_gsubr("make-gp", 1, 0, 0, make_gp);
    scm_c_define_gsubr("factor", 1, 0, 0, gen_factor);
    scm_c_define_gsubr("length", 1, 0, 0, gen_length);
    scm_c_define_gsubr("integer?", 1, 0, 0, gen_integerp);
    scm_c_define_gsubr("odd?", 1, 0, 0, gen_oddp);
    scm_c_define_gsubr("even?", 1, 0, 0, gen_evenp);
    scm_c_define_gsubr("quotient", 2, 0, 0, gen_quotient);
    scm_c_define_gsubr("+", 0, 2, 1, gen_sum_i);
}


#define STACK_SIZE (1 << 24) // 16MB

static void
inner_main(void *data, int argc, char **argv)
{
    (void) data;

    pari_init(STACK_SIZE, 0);
    init_gen_type();
    scm_shell(argc, argv);
    // NB: scm_shell() doesn't return, so pari_close is never called.
    pari_close();
}

int
main(int argc, char **argv)
{
    scm_boot_guile(argc, argv, inner_main, 0);
    return 0; /* never reached */
}
