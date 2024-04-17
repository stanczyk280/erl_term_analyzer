#include <erl_nif.h>
#include <cstdint>
#include "term_tags.h"

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM noop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM analyze_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* Analyzer methods declarations */
void print_binary(uint64_t term, uint64_t mask);
void analyze_immed1(uint64_t term_64);
void analyze_list(uint64_t term_64);
void analyze_tag(uint64_t term_64);
void analyze_immed2(uint64_t term_64);
void analyze_boxed(uint64_t term_64);
void analyze_header(uint64_t term_64);

static ErlNifFunc nif_funcs[] = {
    {"echo", 1, echo},
    {"analyze_term", 1, analyze_term}
};

ERL_NIF_INIT(erl_term_analyzer, nif_funcs, load, NULL, upgrade, unload)

static ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    *priv_data = NULL;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}


static ERL_NIF_TERM echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return argv[0];
}

static ERL_NIF_TERM analyze_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t term_64;
    term_64 = uint64_t(argv[0]);
    analyze_tag(term_64);
    return atom_ok;
}

void analyze_tag(uint64_t term_64)
{
    switch(term_64 & _TAG_PRIMARY_MASK)
    {
        case TAG_PRIMARY_HEADER:
            printf("TAG: HEADER\n");
            printf("TAG bits: ");
            print_binary(term_64, _TAG_PRIMARY_MASK);
            analyze_header(term_64);
            break;
        case TAG_PRIMARY_LIST:
            printf("TAG: LIST\n");
            printf("TAG bits: ");
            print_binary(term_64, _TAG_PRIMARY_MASK);
            analyze_list(term_64);
            break;
        case TAG_PRIMARY_BOXED:
            printf("TAG: BOXED\n");
            printf("TAG bits: ");
            print_binary(term_64, _TAG_PRIMARY_MASK);
            analyze_boxed(term_64);
            break;
        case TAG_PRIMARY_IMMED1:
            printf("TAG: IMMED1\n");
            printf("TAG bits: ");
            print_binary(term_64, _TAG_PRIMARY_MASK);
            analyze_immed1(term_64);
            break;
        default:
            printf("TAG: UNKNOWN\n");
            break;
    }
}

void analyze_list(uint64_t term_64)
{
    uint64_t list_ptr_value;
    list_ptr_value = term_64 & ~ _TAG_PRIMARY_MASK;
    printf("LIST POINTER: %lu\n", list_ptr_value);
    printf("LIST POINTER bits: ");
    print_binary(list_ptr_value, _VALUE_MASK);
    printf("NEXT TERM: \n");
    uint64_t* list_ptr;
    list_ptr = (uint64_t *)list_ptr_value;
    analyze_tag(*list_ptr);
}

void analyze_boxed(uint64_t term_64) {
    uint64_t boxed_ptr_value;
    boxed_ptr_value = term_64 & ~_TAG_PRIMARY_MASK;
    printf("BOXED POINTER: %d\n", boxed_ptr_value);
    printf("BOXED POINTER bits: ");
    print_binary(boxed_ptr_value, _VALUE_MASK);
    printf("NEXT TERM: \n");
    uint64_t* boxed_ptr;
    boxed_ptr = (uint64_t *)boxed_ptr_value;
    analyze_tag(*boxed_ptr);
}

void analyze_header(uint64_t term_64)
{
    uint64_t header = term_64 & _TAG_HEADER_MASK;
    switch (header)
    {
    case _TAG_HEADER_ARITYVAL:
        printf("SUBTAG: ARITYVAL\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        printf("ARITY VALUE: %lu\n", term_64 >> _HEADER_ARITY_OFFS);
        printf("ARITY VALUE bits after bsl of 6: ");
        print_binary(term_64 >> _HEADER_ARITY_OFFS, _VALUE_MASK);
        break;
    case _TAG_HEADER_FUN_REF:
        printf("SUBTAG: FUN_REF\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_FUN:
        printf("SUBTAG: FUN\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_POS_BIG:
        printf("SUBTAG: POS_BIG\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_NEG_BIG:
        printf("SUBTAG: NEG_BIG\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_FLOAT:
        printf("SUBTAG: FLOAT\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_REF:
        printf("SUBTAG: REF\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        printf("ARITY VALUE: %lu\n", term_64 >> _HEADER_ARITY_OFFS);
        printf("ARITY VALUE bits after bsl of 6: ");
        print_binary(term_64 >> _HEADER_ARITY_OFFS, _VALUE_MASK);
        break;
    case _TAG_HEADER_BIN_REF:
        printf("SUBTAG: BIN_REF\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_HEAP_BITS:
        printf("SUBTAG: HEAP_BITS\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_SUB_BITS:
        printf("SUBTAG: SUB_BITS\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_EXTERNAL_PID:
        printf("SUBTAG: EXTERNAL_PID\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_EXTERNAL_PORT: 
        printf("SUBTAG: EXTERNAL_PORT\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_EXTERNAL_REF:
        printf("SUBTAG: EXTERNAL_REF\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    case _TAG_HEADER_MAP:
        printf("SUBTAG: MAP\n");
        printf("SUBTAG bits: ");
        print_binary(term_64, _HEADER_SUBTAG_MASK);
        break;
    }
}

void analyze_immed1(uint64_t term_64)
{
    uint64_t immed1_term = term_64 & _TAG_IMMED1_MASK;
    uint64_t pid, port, small_int;
    switch(immed1_term)
    {
        case _TAG_IMMED1_PID:
            printf("TYPE: PID\n");
            pid = term_64 >> _TAG_IMMED1_SIZE;
            printf("TYPE bits: ");
            print_binary(term_64, _TYPE_IMMED1_MASK);
            printf("VALUE: %lu\n", pid);
            break;
        case _TAG_IMMED1_PORT:
            printf("TYPE: PORT\n");
            port = term_64 >> _TAG_IMMED1_SIZE;
            printf("TYPE bits: ");
            print_binary(term_64, _TYPE_IMMED1_MASK);
            printf("VALUE: %lu\n", port);
            break;
        case _TAG_IMMED1_IMMED2:
            printf("SUBTAG: IMMED2\n");
            printf("SUBTAG bits: ");
            print_binary(term_64, _TYPE_IMMED1_MASK);
            analyze_immed2(term_64);
            break;
        case _TAG_IMMED1_SMALL:
            printf("TYPE: SMALL INT\n");
            small_int = term_64 >> _TAG_IMMED1_SIZE;
            printf("TYPE bits: ");
            print_binary(term_64, _TYPE_IMMED1_MASK);
            printf("VALUE: %lu\n", small_int);
            printf("VALUE bits after bsl of 4: ");
            print_binary(small_int, _VALUE_MASK);
            break;
        default:
            printf("IMMED1: UNKNOWN\n");
            break;
    
    }
}

void analyze_immed2(uint64_t term_64)
{
    uint64_t immed2_term;
    immed2_term = term_64 & _TAG_IMMED2_MASK;
    switch (immed2_term)
    {
        case _TAG_IMMED2_ATOM:
            printf("TYPE: ATOM\n");
            printf("TYPE bits: ");
            print_binary(term_64, _TYPE_IMMED2_MASK);
            break;
        case _TAG_IMMED2_CATCH:
            printf("TYPE: CATCH\n");
            printf("TYPE bits: ");
            print_binary(term_64, _TYPE_IMMED2_MASK);
            break;
        case _TAG_IMMED2_NIL:
            printf("TYPE: NIL\n");
            printf("TYPE bits: ");
            print_binary(term_64, _TYPE_IMMED2_MASK);
            break;
        default:
            printf("TYPE: UNKNOWN\n");
            break;
    }
}

void print_binary(uint64_t num, uint64_t mask) {
    bool is_bracket_open = false;
    for (int i = 63; i >= 0; i--) {
        if ((mask >> i) & 1) {
            if (!is_bracket_open) {
                printf("[");
                is_bracket_open = true;
            }
            putchar((num & (1ULL << i)) ? '1' : '0');
        } else {
            if (is_bracket_open) {
                printf("]");
                is_bracket_open = false;
            }
            putchar((num & (1ULL << i)) ? '1' : '0');
        }
    }
    if (is_bracket_open) {
        printf("]");
    }
    printf("\n");
}