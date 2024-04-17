#include <erl_nif.h>
#include <cstdint>
#include "term_tags.h"

typedef struct TermInfo
{
    char* binary;
    char* tag;
    char* type;
    uint64_t value;
    TermInfo* next_term;

}TermInfo;


/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM analyze_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* Analyzer methods declarations */
void print_binary(uint64_t term, uint64_t mask);
void print_term_info(TermInfo* info);
TermInfo* analyze_immed1(uint64_t term_64);
TermInfo* analyze_list(uint64_t term_64);
TermInfo* analyze_tag(uint64_t term_64);
TermInfo* analyze_immed2(uint64_t term_64);
TermInfo* analyze_boxed(uint64_t term_64);
TermInfo* analyze_header(uint64_t term_64);

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

static ERL_NIF_TERM term_info_to_map(ErlNifEnv* env, TermInfo* term_info)
{
    ERL_NIF_TERM map = enif_make_new_map(env);

    ERL_NIF_TERM key, value;

    key = enif_make_atom(env, "binary");
    value = enif_make_string(env, term_info->binary, ERL_NIF_LATIN1);
    enif_make_map_put(env, map, key, value, &map);

    key = enif_make_atom(env, "tag");
    value = enif_make_string(env, term_info->tag, ERL_NIF_LATIN1);
    enif_make_map_put(env, map, key, value, &map);

    key = enif_make_atom(env, "type");
    value = enif_make_string(env, term_info->type ? term_info->type : "(null)", ERL_NIF_LATIN1);
    enif_make_map_put(env, map, key, value, &map);

    key = enif_make_atom(env, "value");
    value = enif_make_uint64(env, term_info->value);
    enif_make_map_put(env, map, key, value, &map);

    if (term_info->next_term != NULL) {
        key = enif_make_atom(env, "next_term");
        value = term_info_to_map(env, term_info->next_term);
        enif_make_map_put(env, map, key, value, &map);
    }

    return map;
}

static ERL_NIF_TERM analyze_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t term_64;
    term_64 = uint64_t(argv[0]);
    TermInfo* term_info = analyze_tag(term_64);

    return term_info_to_map(env, term_info);
}

char* get_binary_representation(uint64_t term_64)
{
    char* binary = (char*) malloc(65 * sizeof(char));

    for(int i = 63; i >= 0; i--) {
        binary[63 - i] = ((term_64 >> i) & 1) ? '1' : '0';
    }
    binary[64] = '\0';

    return binary;
}

TermInfo* analyze_tag(uint64_t term_64)
{
    TermInfo* term_info;

    char* binary = get_binary_representation(term_64);

    switch(term_64 & _TAG_PRIMARY_MASK)
    {
        case TAG_PRIMARY_HEADER:
            term_info = analyze_header(term_64);
            break;
        case TAG_PRIMARY_LIST:
            term_info = analyze_list(term_64);
            break;
        case TAG_PRIMARY_BOXED:
            term_info = analyze_boxed(term_64);
            break;
        case TAG_PRIMARY_IMMED1:
            term_info = analyze_immed1(term_64);
            break;
        default:
            printf("TAG: UNKNOWN\n");
            term_info = NULL;
            break;
    }
    term_info -> binary = binary;
    return term_info;
}

TermInfo* analyze_list(uint64_t term_64)
{
    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));

    char* tag;
    tag = "LIST";

    char* binary = get_binary_representation(term_64);
    uint64_t list_ptr_value;
    list_ptr_value = term_64 & ~ _TAG_PRIMARY_MASK;
    uint64_t* list_ptr;
    list_ptr = (uint64_t *)list_ptr_value;
    term_info -> binary = binary;
    term_info -> tag = tag;
    term_info -> type = NULL;
    term_info -> value = list_ptr_value;
    term_info -> next_term = analyze_tag(*list_ptr);

    return term_info;
}

TermInfo* analyze_boxed(uint64_t term_64) 
{
    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));

    char* tag;
    tag = "BOXED";

    char* binary = get_binary_representation(term_64);
    uint64_t boxed_ptr_value;
    boxed_ptr_value = term_64 & ~_TAG_PRIMARY_MASK;
    uint64_t* boxed_ptr;
    boxed_ptr = (uint64_t *)boxed_ptr_value;

    term_info -> binary = binary;
    term_info -> tag = tag;
    term_info -> type = NULL;
    term_info -> value = boxed_ptr_value;
    term_info -> next_term = analyze_tag(*boxed_ptr);

    return term_info;
}

TermInfo* analyze_header(uint64_t term_64)
{
    uint64_t header = term_64 & _TAG_HEADER_MASK;
    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));

    char* tag;
    tag = "HEADER";
    char* binary = get_binary_representation(term_64);

    switch (header)
    {
    case _TAG_HEADER_ARITYVAL:
        term_info -> tag = tag;
        term_info -> type = "ARITYVAL";
        term_info -> value = term_64 >> _HEADER_ARITY_OFFS;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_FUN_REF:
        term_info -> tag = tag;
        term_info -> type = "FUN_REF";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_FUN:
        term_info -> tag = tag;
        term_info -> type = "FUN";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_POS_BIG:
        term_info -> tag = tag;
        term_info -> type = "POS_BIG";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_NEG_BIG:
        term_info -> tag = tag;
        term_info -> type = "NEG_BIG";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_FLOAT:
        term_info -> tag = tag;
        term_info -> type = "FLOAT";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_REF:
        term_info -> tag = tag;
        term_info -> type = "REF";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_BIN_REF:
        term_info -> tag = tag;
        term_info -> type = "BIN_REF";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_HEAP_BITS:
        term_info -> tag = tag;
        term_info -> type = "HEAP_BITS";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_SUB_BITS:
        term_info -> tag = tag;
        term_info -> type = "SUB_BITS";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_EXTERNAL_PID:
        term_info -> tag = tag;
        term_info -> type = "EXTERNAL_PID";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_EXTERNAL_PORT: 
        term_info -> tag = tag;
        term_info -> type = "EXTERNAL_PORT";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_EXTERNAL_REF:
        term_info -> tag = tag;
        term_info -> type = "EXTERNAL_REF";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    case _TAG_HEADER_MAP:
        term_info -> tag = tag;
        term_info -> type = "MAP";
        term_info -> value = NULL;
        term_info -> next_term = NULL;
        break;
    }

    term_info -> binary = binary;
    return term_info;
}

TermInfo* analyze_immed1(uint64_t term_64)
{
    uint64_t immed1_term = term_64 & _TAG_IMMED1_MASK;
    uint64_t pid, port, small_int;

    char* tag;

    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));

    char* binary = get_binary_representation(term_64);
    tag = "IMMED1";

    switch(immed1_term)
    {
        case _TAG_IMMED1_PID:
            pid = term_64 >> _TAG_IMMED1_SIZE;
            term_info -> tag = tag;
            term_info -> type = "PID";
            term_info -> value = pid;
            term_info -> next_term = NULL;
            break;
        case _TAG_IMMED1_PORT:
            port = term_64 >> _TAG_IMMED1_SIZE;
            term_info -> tag = tag;
            term_info -> type = "PORT";
            term_info -> value = port;
            term_info -> next_term = NULL;
            break;
        case _TAG_IMMED1_IMMED2:
            term_info -> tag = tag;
            term_info -> type = "IMMED2";
            term_info -> value = NULL;
            term_info -> next_term = analyze_immed2(term_64);
            break;
        case _TAG_IMMED1_SMALL:
            small_int = term_64 >> _TAG_IMMED1_SIZE;
            term_info -> tag = tag;
            term_info -> type = "SMALL INT";
            term_info -> value = small_int;
            term_info -> next_term = NULL;
            break;
        default:
            printf("IMMED1: UNKNOWN\n");
            break;
    
    }

    term_info -> binary = binary;

    return term_info;
}

TermInfo* analyze_immed2(uint64_t term_64)
{
    uint64_t immed2_term;
    immed2_term = term_64 & _TAG_IMMED2_MASK;

    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));
    char* tag;
    tag = "IMMED2";
    char* binary = get_binary_representation(term_64);

    switch (immed2_term)
    {
        case _TAG_IMMED2_ATOM:
            term_info -> tag = tag;
            term_info -> type = "ATOM";
            term_info -> value = NULL;
            term_info -> next_term = NULL;
            break;
        case _TAG_IMMED2_CATCH:
            term_info -> tag = tag;
            term_info -> type = "CATCH";
            term_info -> value = NULL;
            term_info -> next_term = NULL;
            break;
        case _TAG_IMMED2_NIL:
            term_info -> tag = tag;
            term_info -> type = "NILL";
            term_info -> value = NULL;
            term_info -> next_term = NULL;
            break;
        default:
            printf("TYPE: UNKNOWN\n");
            break;
    }

    term_info -> binary = binary;
    
    return term_info;
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

void print_term_info(TermInfo* info) {
    printf("BINARY: %s\n", info->binary);
    printf("TAG: %s\n", info->tag);
    printf("TYPE: %s\n", info->type);
    printf("VALUE: %lu\n", info->value);
    if (info->next_term != NULL) {
        print_term_info(info->next_term);
    }
}