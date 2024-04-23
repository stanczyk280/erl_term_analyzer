#include <erl_nif.h>
#include <cstdint>
#include "term_tags.h"
#include <string.h>

typedef struct TermInfo
{
    char* binary;
    char* tag;
    char* type;
    long int value;
    TermInfo* head;  // The first element of the list
    TermInfo* tail;  // The rest of the list
}TermInfo;



/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM analyze_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* Analyzer methods declarations */
TermInfo* analyze_immed1(uint64_t term_64);
TermInfo* analyze_immed2(uint64_t term_64);
TermInfo* analyze_tag(uint64_t term_64);
void analyze_list(uint64_t term_64);

void print_binary(uint64_t term_64, uint64_t mask);
void print_term_info(TermInfo* info);
char* get_binary_representation(uint64_t term_64);

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

    if (term_info->head != NULL) {
        key = enif_make_atom(env, "head");
        value = term_info_to_map(env, term_info->head);
        enif_make_map_put(env, map, key, value, &map);
    }

    return map;
}

static ERL_NIF_TERM analyze_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t term_64;
    term_64 = uint64_t(argv[0]);
    TermInfo* term_info;
    analyze_tag(term_64);

    return atom_ok;
}

TermInfo* analyze_tag(uint64_t term_64)
{
    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));
    char* binary = get_binary_representation(term_64);
    uint64_t tag = term_64 & _TAG_PRIMARY_MASK;

    switch(tag)
    {
        case TAG_PRIMARY_HEADER:
            term_info -> binary = binary;
            term_info -> tag = (char*)"HEADER";
            term_info -> type = (char*)"HEADER";
            term_info -> value = term_64;
            term_info -> head = NULL;
            term_info -> tail = NULL;
            print_term_info(term_info);
            break;
        case TAG_PRIMARY_LIST:
            analyze_list(term_64);
            break;
        case TAG_PRIMARY_BOXED:
            term_info -> binary = binary;
            term_info -> tag = (char*)"BOXED";
            term_info -> type = (char*)"BOXED";
            term_info -> value = term_64;
            term_info -> head = NULL;
            term_info -> tail = NULL;
            print_term_info(term_info);
            break;
        case TAG_PRIMARY_IMMED1:
            term_info = analyze_immed1(term_64);
            break;
    }

    return term_info;
}

TermInfo* analyze_immed1(uint64_t term_64)
{
    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));
    char* binary = get_binary_representation(term_64);
    uint64_t immed1 = term_64 & _TAG_IMMED1_MASK;
    long int value = term_64 >> _TAG_IMMED1_SIZE;

    term_info -> binary = binary;
    term_info -> tag = (char*)"IMMED1";
    term_info -> tail = NULL;
    switch(immed1)
    {
        case _TAG_IMMED1_PID:
            term_info -> type = (char*)"PID";
            term_info -> value = value;
            term_info -> head = NULL;
            print_term_info(term_info);
            break;
        case _TAG_IMMED1_PORT:
            term_info -> type = (char*)"PORT";
            term_info -> value = value;
            term_info -> head = NULL;
            print_term_info(term_info);
            break;
        case _TAG_IMMED1_IMMED2:
            term_info -> type = (char*)"IMMED2";
            term_info -> value = 0;
            term_info -> head = analyze_immed2(term_64);
            print_term_info(term_info);
            break;
        case _TAG_IMMED1_SMALL:
            term_info -> type = (char*)"SMALL";
            term_info -> value = value;
            term_info -> head = NULL;
            print_term_info(term_info);
            break; 

    }

    return term_info;
}

TermInfo* analyze_immed2(uint64_t term_64)
{
    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));
    char* binary = get_binary_representation(term_64);
    uint64_t immed2 = term_64 & _TAG_IMMED2_MASK;
    long int value = term_64 >> _TAG_IMMED2_SIZE;

    term_info -> binary = binary;
    term_info -> tag = (char*)"IMMED2";
    term_info -> tail = NULL;
    switch(immed2)
    {
        case _TAG_IMMED2_ATOM:
            term_info -> type = (char*)"ATOM";
            term_info -> value = value;
            term_info -> head = NULL;
            break;
        case _TAG_IMMED2_CATCH:
            term_info -> type = (char*)"CATCH";
            term_info -> value = 0;
            term_info -> head = NULL;
            break;
        case _TAG_IMMED2_NIL:
            term_info -> type = (char*)"NIL";
            term_info -> value = 0;
            term_info -> head = NULL;
            break;
    }

    return term_info;
}

TermInfo* analyze_tail(uint64_t term_64)
{
    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));
    char* binary = get_binary_representation(term_64);
    char* tag = (char*)"LIST";
    char* type = (char*)"NONE";
    long int value = term_64 & ~ _TAG_PRIMARY_MASK;
    
    term_info -> binary = binary;
    term_info -> tag = tag;
    term_info -> type = type;
    term_info -> value = value;
    term_info -> head = NULL;
    term_info -> tail = NULL;

    return term_info;
}

void analyze_list(uint64_t term_64)
{
    char* tag = (char*)"LIST";
    char* type = (char*)"NONE";
    long int value = term_64 & ~ _TAG_PRIMARY_MASK;
    uint64_t* list_ptr = (uint64_t *)value;
    uint64_t* head_ptr = list_ptr;
    uint64_t* tail_ptr = list_ptr + 1;

    TermInfo* term_info = (TermInfo*)malloc(sizeof(TermInfo));
    term_info->binary = get_binary_representation(term_64);
    term_info->tag = tag;
    term_info->type = type;
    term_info->value = value;

    while(true)
    {
        if (!head_ptr || !tail_ptr)
        {
            printf("NULL POINTER\r\n");
            break;
        }
        
        term_info->head = analyze_tag(*head_ptr);
        term_info->tail = analyze_tail(*tail_ptr);

        printf("\r\n====================\r\n");
        print_term_info(term_info);
        head_ptr = (uint64_t *)term_info->tail->value;
        tail_ptr = head_ptr + 1;
        if ((*tail_ptr & _TAG_IMMED2_MASK) == _TAG_IMMED2_NIL) 
        {
            printf("\r\n====================\r\n");
            term_info->head = analyze_tag(*head_ptr);
            print_term_info(term_info);

            printf("\r\n====================\r\n");
            print_term_info(analyze_tag(*tail_ptr));
            printf("END OF LIST\r\n");
            break;
        }
        
    }
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

void print_binary(uint64_t term_64, uint64_t mask) {
    bool is_bracket_open = false;
    for (int i = 63; i >= 0; i--) {
        if ((mask >> i) & 1) {
            if (!is_bracket_open) {
                printf("[");
                is_bracket_open = true;
            }
            putchar((term_64 & (1ULL << i)) ? '1' : '0');
        } else {
            if (is_bracket_open) {
                printf("]");
                is_bracket_open = false;
            }
            putchar((term_64 & (1ULL << i)) ? '1' : '0');
        }
    }
    if (is_bracket_open) {
        printf("]");
    }
    printf("\r\n");
}

void print_term_info(TermInfo* info) {
    printf("BINARY: %s\r\n", info->binary);
    printf("TAG: %s\r\n", info->tag);
    printf("TYPE: %s\r\n", info->type);
    printf("VALUE: %lu\r\n", info->value);
    if (info->head != NULL) {
        print_term_info(info->head);
    }
}
