/*
 * EPIA - Easily Parallel Instruction Architecture
 * (C) Dmitry 'sciloaf' Solovyev aka MatrixS_Master, 2025
 * All rights reserved.
 * */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdbool.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <strings.h>
#include <locale.h>
#include <assert.h>
#include <math.h>
#include <time.h>
#include <fcntl.h>
#include <sys/stat.h>

#define VERSION "0.2.2"

//#define DEBUG
#define LONG64B
#define USE_MMAP
#define USE_OPENMP
#define DEFAULT_MEMORY (1U*1024U*1024U)

#define LOCTYPE uint64_t

#define MAXLEN_MESSAGE 1024

#define MACHINE_ARGFIELD 8
#define MACHINE_PPUCLOBS 32
#define MACHINE_OP_ARITH (1 << 7)
#define MACHINE_OP_SIGND (1 << 6)
#define MACHINE_FLG_ZERO (1 << 7)

#define ASSEMBLY_MAXLEN 1024
#define ASSEMBLY_MAXVARNAME 64
#define ASSEMBLY_MNEMONICLEN 3
#define ASSEMBLY_MAXARGS 3
#define ASSEMBLY_DEFWIDTH (4-1)
#define ASSEMBLY_FLOATWIDTH (3-1)
#define ASSEMBLY_WHITESPACE " \t,"
#define ASSEMBLY_TOKENS "+-*/|&~^%<>()"
#define ASSEMBLY_ARITHOPS "+-*/|&^<>"
#define ASSEMBLY_UNARYOPS "-~&%"
#define ASSEMBLY_EXPIGNORE " \t\n\r!"
#define ASSEMBLY_OFFSETMODS "+?"
#define ASSEMBLY_STOPMARK "STOP"
//#define ASSEMBLY_LABELS_CASE_INSENS

#ifdef LONG64B
    #define FMT_IP "0x%016lx"
    #define FMT_LOC "%lu"
    #define FMT_SGN "%li"
    #define STRTOU64 strtoul
    #define STRTOL64 (LOCTYPE)strtol
#else
    #define FMT_IP "0x%016llx"
    #define FMT_LOC "%llu"
    #define FMT_SGN "%lli"
    #define STRTOU64 strtoull
    #define STRTOL64 (LOCTYPE)strtoll
#endif

#ifdef USE_MMAP
    #include <sys/mman.h>
#endif

#ifdef USE_OPENMP
    #define PARALLEL _Pragma("omp parallel for")
#else
    #define PARALLEL
#endif

#define NUMITEMS(ARR) (sizeof(ARR)/sizeof((ARR)[0]))

#define ERR(S,...) fprintf(stderr,S "\n", __VA_ARGS__)

#ifdef DEBUG
    #define DBG(S,...) fprintf(stderr,"[DEBUG] " S "\n",__VA_ARGS__)
#else
    #define DBG(...)
#endif

enum {
    ASM_STATE_START = 0,
    ASM_STATE_NAME,
    ASM_STATE_ARG,
    ASM_STATE_MACRO_NAME,
    ASM_STATE_MACRO_BODY,
    ASM_STATE_CONCAT_ALL
};

enum {
    ASM_OK = 0,
    ASM_ERR_UNKNOWN,
    ASM_ERR_UNK_TOKEN,
    ASM_ERR_VALUE,
    ASM_ERR_NOLABEL,
    ASM_ERR_LINESTOP,
    ASM_ERR_SYNTAX,
    ASM_ERR_EMIT,
    ASM_NUMERRORS
};

static const char* asm_errornames[ASM_NUMERRORS] = {
    "",
    "unknown error",
    "unknown token",
    "value error",
    "label not found",
    "abrupt line end",
    "syntax error",
    "code emission error",
};

enum {
    GERR_OK,
    GERR_NOINPUT,
    GERR_INPERR,
    GERR_OUTERR,
    GERR_CPUEXCPT,
    GERR_MMAP,
};

static const struct {
    char op;
    int p;

} op_prio_tab[] = {
    { '|', 3 },
    { '^', 4 },
    { '&', 5 },
    { '>', 10 },
    { '<', 10 },
    { '+', 15 },
    { '-', 15 },
    { '*', 20 },
    { '/', 20 },
    { 0, 0 }
};

static const char* data_labels[] = { "DB", "DH", "DW", "DD", 0 };
static const char* data_modifiers[] = { "b", "h", "w", "d", 0 };

typedef struct __attribute__((packed)) {
    uint8_t reserved;
    uint8_t reserve_field : 4;
    uint8_t first_arg : 1;
    uint8_t second_arg : 1;
    uint8_t third_arg : 1;
    uint8_t dst_ptr : 1;
    uint8_t dst_imm : 1;
    uint8_t src1_ptr : 1;
    uint8_t src1_imm : 1;
    uint8_t src2_ptr : 1;
    uint8_t src2_imm : 1;
    uint8_t fpu_op : 1;
    uint8_t width : 2;
    uint8_t opcode;
} bytecode;

static const struct {
    const char* name;
    uint8_t opcode;
    int n_args;
    bool is_fpu;

} asm_instructions[] = {
    { "nop", 0b00000000, 0, false },
    { "hlt", 0b01111111, 0, false },

    { "mov", 0b00000001, 2, false },

    { "cmp", 0b00000110, 2, false },
    { "cms", 0b01000110, 2, false },
    { "add", 0b10000001, 3, false },
    { "sub", 0b11000010, 3, false },
    { "suu", 0b10000010, 3, false },
    { "mul", 0b10000011, 3, false },
    { "div", 0b11000100, 3, false },
    { "diu", 0b10000100, 3, false },
    { "mod", 0b10000101, 3, false },
    { "neg", 0b11000110, 2, false },
    { "sex", 0b11000111, 3, false },
    { "and", 0b10001000, 3, false },
    { "ior", 0b10001001, 3, false },
    { "xor", 0b10001010, 3, false },
    { "not", 0b10001100, 2, false },
    { "shl", 0b10001101, 3, false },
    { "shr", 0b10001110, 3, false },

    { "cmf", 0b00000101, 2, true  },
    { "adf", 0b10000001, 3, true  },
    { "suf", 0b10000010, 3, true  },
    { "muf", 0b10000011, 3, true  },
    { "dif", 0b10000100, 3, true  },
    { "ngf", 0b10000110, 2, true  },
    { "ivf", 0b10001100, 2, true  },
    { "abf", 0b10100000, 2, true  },
    { "rnf", 0b10100001, 2, true  },
    { "sqr", 0b10100010, 2, true  },
    { "pow", 0b10100011, 3, true  },
    { "exp", 0b10100100, 2, true  },
    { "sin", 0b10100101, 2, true  },
    { "cos", 0b10100110, 2, true  },

    { "itf", 0b11110000, 2, true  },
    { "fti", 0b10110001, 2, false },

    { "sip", 0b00010000, 1, false },
    { "sfl", 0b00010001, 1, false },
    { "jif", 0b00010010, 2, false },
    { "jmp", 0b00010011, 1, false },

    { "ppu", 0b00111000, 3, false },
    { "clo", 0b00111001, 1, false },

    { "dpu", 0b10101000, 1, true  },
    { "dpr", 0b10101001, 2, false },
    { "dpw", 0b00101010, 2, false },

    { "out", 0b00111101, 2, false },
    { "inp", 0b10111110, 2, false },
};

enum instructions_e {
    INST_nop,
    INST_hlt,

    INST_mov,

    INST_cmp,
    INST_cms,
    INST_add,
    INST_sub,
    INST_suu,
    INST_mul,
    INST_div,
    INST_diu,
    INST_mod,
    INST_neg,
    INST_sex,
    INST_and,
    INST_ior,
    INST_xor,
    INST_not,
    INST_shl,
    INST_shr,

    INST_cmf,
    INST_adf,
    INST_suf,
    INST_muf,
    INST_dif,
    INST_ngf,
    INST_ivf,
    INST_abf,
    INST_rnf,
    INST_sqr,
    INST_pow,
    INST_exp,
    INST_sin,
    INST_cos,

    INST_itf,
    INST_fti,

    INST_sip,
    INST_sfl,
    INST_jif,
    INST_jmp,

    INST_ppu,
    INST_clo,

    INST_dpu,
    INST_dpr,
    INST_dpw,

    INST_out,
    INST_inp,

    INST_INVALID
};

static const char* asm_mnemonics[NUMITEMS(asm_instructions)+1] = {0};

enum {
    EXCPT_NO,
    EXCPT_DIV_BY_ZERO,
    EXCPT_INST_INVALID,
    EXCPT_MEM_BOUND,
    EXCPT_NO_CLOBBER,
    EXCPT_NUM_EXCEPTIONS
};

static const char* cpu_exceptions[EXCPT_NUM_EXCEPTIONS] = {
    "",
    "division by zero",
    "invalid instruction",
    "memory bounds error",
    "no room for clobber",
};

enum {
    MACHINE_IOCHAR,
    MACHINE_IOUINT,
    MACHINE_IOINT,
    MACHINE_IOFLOAT,
    MACHINE_IOSTRING,
    MACHINE_IOPOINTER,
};

enum {
    //MACHINE_DPU_STATUS,
    MACHINE_DPU_AVSTART,
    MACHINE_DPU_AVEND,
    MACHINE_DPU_ASSTART,
    MACHINE_DPU_BVSTART,
    MACHINE_DPU_BSSTART,
    //MACHINE_DPU_BEND,
    MACHINE_DPU_VSEQLEN,
    MACHINE_DPU_VPAD,
    MACHINE_DPU_SSEQLEN,
    MACHINE_DPU_SPAD,
    //MACHINE_DPU_ELEMSIZE,
    MACHINE_DPU_NREGS
};

typedef struct {
    char* label;
    char* prefix;
    LOCTYPE addr;
    int width;
    int refs;
    int line;
    bool ismacro;
    bool isfloat;
} asm_label;

typedef struct {
    union {
        LOCTYPE u;
        float f;
    } v;
    int t;
} asm_term;

typedef union {
    LOCTYPE u;
    int64_t s;
    float f;
} uvalue;

struct {
    const char* fname;
    const char* outname;
    FILE* file;
    LOCTYPE ip;
    size_t line;
    char* linecopy;
    char* prefix;
    const char* pos;
    char next_tok;
    LOCTYPE next_val;
    float next_flt;
    asm_label* labels;
    int n_labels;
    asm_label* pending;
    int n_pending;
    uint8_t* outmem;
    LOCTYPE outsize;
    bool error;
    bool dumptxt;
    bool listmacro;
    bool listunused;
    bool cutatip;
} state;

typedef struct {
    bool exec;
    uint8_t* mem;
    uint8_t* ext;
    LOCTYPE ip, start, size, barrier;
    LOCTYPE extstart, extsize;
    LOCTYPE clob_addr[MACHINE_PPUCLOBS];
    uint8_t clob_cont[MACHINE_PPUCLOBS * sizeof(LOCTYPE)];
    LOCTYPE dpu_regs[MACHINE_DPU_NREGS];
    int n_clobs;
    int exception;
    int ext_file;
    bool ext_rw;
    bool timing;
    LOCTYPE start_us; // instrumentation only
} execstate;

execstate main_exe;

// prototypes
static asm_term expression(const int prio_level);
static asm_term unary_op();
void run(execstate* S);

void error(char* fmtstr, ...)
{
    va_list vl;
    char buf[MAXLEN_MESSAGE] = {0};
    va_start(vl,fmtstr);
    vsnprintf(buf,sizeof(buf),fmtstr,vl);
    va_end(vl);
    fprintf(stderr,"ERROR in '%s' on line %zu: %s\n",state.fname,state.line,buf);
    state.error = true;
#ifdef DEBUG
    abort();
#endif
}

int dumpmem(const char* fn, uint8_t* data, size_t len)
{
    FILE* f = fopen(fn,"wb");
    if (!f) return GERR_OUTERR;
    fwrite(data,len,1,f);
    fclose(f);
    return 0;
}

void strtrim_left(char* str)
{
    if (!isspace(str[0])) return;

    // count spaces
    int spaces = 0;
    while (str[spaces] && isspace(str[spaces])) spaces++;

    // shift string
    for (int i = 0; str[i]; i++) {
        str[i] = str[i+spaces];
        if (!str[i]) break;
    }
}

void strtrim_right(char* str)
{
    int n = strlen(str);
    while (str[0] && n && isspace(str[n-1]))
        str[--n] = 0;
}

char numconvert(const char* pos, char** endptr, LOCTYPE* ival, float* fval)
{
    char ret = 0;
    char* end = NULL;
    errno = 0;

    if (*pos == '-') // is it signed int?
        *ival = STRTOL64(pos,&end,0);
    else
        *ival = STRTOU64(pos,&end,0);

    if (errno) return ret;
    if (end > pos) ret = '1'; // signals a numeric token

    if (*end == '.' || toupper(*end) == 'E') { // is it float?
        *fval = strtod(pos,&end);
        ret = '0';
    }

    if (endptr) *endptr = end;
    return ret;
}

LOCTYPE get_us()
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC,&ts);
    return ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}

int find_in(const char** arr, const char* word)
{
    for (int i = 0; arr[i]; i++) {
        if (!strcasecmp(word,arr[i])) return i;
    }
    return -1;
}

static inline bool safestrequ(const char* a, const char* b)
{
    if (a == b) return true;
    if (!a || !b) return false;
#ifdef ASSEMBLY_LABELS_CASE_INSENS
    return !strcasecmp(a,b);
#else
    return !strcmp(a,b);
#endif
}

static LOCTYPE* resolve_label(const char* label, bool global, bool* isfloat)
{
    const char* pfx = global? NULL : state.prefix;
    DBG("Looking for '%s' in %s space %s",label,global? "global":"local",pfx);

    // try to find in local (first) or global name space
    for (int i = 0; i < state.n_labels; i++) {
        asm_label* p = state.labels + i;
        if (safestrequ(p->label,label) && safestrequ(p->prefix,pfx)) {
            if (isfloat) *isfloat = p->isfloat;
            p->refs++;
            return &(p->addr);
        }
    }

    // if we're actually in local space, try again to look in global space
    LOCTYPE* r = NULL;
    if (!global && state.prefix) r = resolve_label(label,true,isfloat);
    return r;
}

static char get_token()
{
    char cur = state.next_tok;

    // skip all spaces and unwanted characters
    while (*state.pos && strchr(ASSEMBLY_EXPIGNORE,*state.pos)) state.pos++;

    // detect end of string
    if (*state.pos == 0) {
        state.next_val = 0;
        state.next_tok = 0;
        return cur;
    }

    // is it just a number?
    if (isdigit(*state.pos) || (*state.pos == '-' && isdigit(*(state.pos+1)))) {
        // get a whole numeric value
        char* end = NULL;
        state.next_tok = numconvert(state.pos,&end,&state.next_val,&state.next_flt);
        assert(end);
        state.pos = end;
        return cur;

    } else if (*state.pos == '\'') {
        // get an ASCII character value
        state.next_val = *(++state.pos);
        if (*(++state.pos) != '\'') {
            error("Expected ' at the end of a character literal '%c', got '%c'",state.next_val,*state.pos);
            return 0;
        }
        ++state.pos;
        state.next_tok = '1';
        return cur;
    }

    // is it a regular token?
    if (strchr(ASSEMBLY_TOKENS,*state.pos)) {
        state.next_val = 0;
        state.next_tok = *state.pos++;
        return cur;
    }

    // is it a label or a macro?
    char name[ASSEMBLY_MAXVARNAME] = {0};
    for (int i = 0; i < ASSEMBLY_MAXVARNAME-1; i++) {
        if (isspace(*state.pos) || strchr(ASSEMBLY_TOKENS,*state.pos)) break;
        name[i] = *state.pos++;
    }

    bool flt = false;
    LOCTYPE* arg = resolve_label(name,false,&flt);
    if (!arg) {
        error("Invalid token in expression: %s",name);
        return 0;
    }

    if (!flt) {
        state.next_val = *arg;
        state.next_tok = '1';
    } else {
        memcpy(&state.next_flt,arg,sizeof(float));
        state.next_tok = '0';
    }
    return cur;
}

static void init_parser(const char* input)
{
    state.pos = input;
    state.next_tok = 0;
    get_token();
}

static void add_label(const LOCTYPE addr, const char* label, const int width, bool macro, bool pending, bool isfloat)
{
    if (!pending && resolve_label(label,false,NULL)) {
        error("Duplicate label/macro name: %s",label);
        return;
    }

    int n = pending? state.n_pending++ : state.n_labels++;
    asm_label* arr = pending? state.pending : state.labels;

    arr = (asm_label*)realloc(arr,(n+1)*sizeof(asm_label));
    arr[n].addr = addr;
    arr[n].label = strdup(label);
    arr[n].ismacro = macro;
    arr[n].isfloat = isfloat;
    arr[n].width = width;
    arr[n].prefix = state.prefix? strdup(state.prefix) : NULL;
    arr[n].refs = 0;
    arr[n].line = state.line;

    if (pending) {
        state.pending = arr;
        DBG("Name '%s' added to pending pool in context '%s'",label,state.prefix);
    } else {
        state.labels = arr;
        DBG("Label '%s' in context '%s' set at " FMT_IP,label,state.prefix,addr);
    }
}

static void append_pending(const char* text)
{
    char buf[ASSEMBLY_MAXLEN] = {0};
    char* old = state.pending[state.n_pending-1].label;
    snprintf(buf,sizeof(buf),"%s %s",old,text);
    free(old);
    state.pending[state.n_pending-1].label = strdup(buf);
}

static int get_prio(const char op)
{
    for (int i = 0; op_prio_tab[i].op; i++) {
        if (op_prio_tab[i].op == op) return op_prio_tab[i].p;
    }
    error("Unknown operator '%c'",op);
    return 0;
}

static asm_term term()
{
    asm_term ret = {0};

    if (state.next_tok == '(') {
        get_token();
        ret = expression(0);
        if (get_token() != ')') {
            error("No closing parenthesis at the end of expression");
            ret.t = 0;
            return ret;
        }

    } else if (state.next_tok == '1') {
        ret.v.u = state.next_val;
        ret.t = 1;
        get_token();

    } else if (state.next_tok == '0') {
        ret.v.f = state.next_flt;
        ret.t = 2;
        get_token();

    } else if (strchr(ASSEMBLY_UNARYOPS,state.next_tok))
        ret = unary_op();

    return ret;
}

static asm_term expression(const int prio_level)
{
    asm_term a = term();
    while (state.next_tok && strchr(ASSEMBLY_ARITHOPS,state.next_tok)) {
        // precedence can only climb up
        if (get_prio(state.next_tok) <= prio_level) break;

        char op = get_token();
        asm_term b = expression(get_prio(op)); // don't get lower than current priority level

        if (a.t == 2 && b.t == 1) {
            b.v.f = (float)b.v.u;
            b.t = 2;
        } else if (a.t == 1 && b.t == 2) {
            b.v.u = (LOCTYPE)((int64_t)b.v.f);
            b.t = 1;
        }

        if (a.t == 1) {
            switch (op) {
            case '+': a.v.u += b.v.u; break;
            case '-': a.v.u -= b.v.u; break;
            case '*': a.v.u *= b.v.u; break;
            case '/': a.v.u /= b.v.u; break;
            case '%': a.v.u = a.v.u % b.v.u; break;
            case '|': a.v.u |= b.v.u; break;
            case '&': a.v.u &= b.v.u; break;
            case '^': a.v.u ^= b.v.u; break;
            case '>': a.v.u >>= b.v.u; break;
            case '<': a.v.u <<= b.v.u; break;
            default: error("Unimplemented integer operator: '%c'",op);
            }
        } else if (a.t == 2) {
            switch (op) {
            case '+': a.v.f += b.v.f; break;
            case '-': a.v.f -= b.v.f; break;
            case '*': a.v.f *= b.v.f; break;
            case '/': a.v.f /= b.v.f; break;
            case '^': a.v.f = powf(a.v.f,b.v.f); break;
            default: error("Unimplemented float operator: '%c'",op);
            }
        } else
            error("Wrong LH literal type %d",a.t);
    }
    return a;
}

static asm_term unary_op()
{
    char op = get_token();
    asm_term val = term();

    if (val.t == 1) {
        switch (op) {
        case '-': val.v.u = (LOCTYPE)(-(int64_t)val.v.u); return val;
        case '~': val.v.u = ~val.v.u; return val;
        case '&': val.v.f = (int64_t)val.v.u; val.t = 2; return val;
        }
    } else if (val.t == 2) {
        switch (op) {
        case '-': val.v.f = -val.v.f; return val;
        case '%': val.v.f = sqrtf(val.v.f); return val;
        }
    }

    // This shouldn't happen
    error("Unary operator selection failed!");
    val.v.u = 0;
    val.t = 0;
    return val;
}

static inline int asm_numeric_token(char* tok, LOCTYPE* arg, bool* isfloat)
{
    float vf = 0;
    char x = numconvert(tok,NULL,arg,&vf);
    if (!x) {
        error("Error converting argument '%s' to a number",tok);
        return ASM_ERR_VALUE;
    }

    if (x == '0') { // it's a float
        if (isfloat) *isfloat = true;
        *arg = 0;
        memcpy(arg,&vf,sizeof(float));
        DBG("Float literal found: %f converted to " FMT_LOC,vf,*arg);
    }

    return ASM_OK;
}

static void emit_memcopy(void* buf, LOCTYPE len)
{
    if (state.ip + len > state.outsize) {
        error("Memory overflow: attempt to write " FMT_LOC " bytes @" FMT_IP " with memory limit set to " FMT_IP,len,state.ip,state.outsize);
        state.error = true;
    } else
        memcpy(state.outmem+state.ip,buf,len);
    state.ip += len;
}

static void emit_oper(int op, bytecode bc)
{
    bc.reserved = 0;
    bc.opcode = asm_instructions[op].opcode;
    bc.fpu_op = asm_instructions[op].is_fpu;
    bc.first_arg = asm_instructions[op].n_args > 0;
    bc.second_arg = asm_instructions[op].n_args > 1;
    bc.third_arg = asm_instructions[op].n_args > 2;
    emit_memcopy(&bc,sizeof(bc));
}

static void emit_arg(LOCTYPE val, int width)
{
    DBG("Emitting argument @" FMT_LOC ": " FMT_LOC " with width %d",state.ip,val,width);
    if (width > MACHINE_ARGFIELD) {
        error("Weird width for an argument of value " FMT_LOC,val);
    }
    emit_memcopy(&val,width);
}

static void emit_byte(uint8_t b)
{
    DBG("Emitting byte @" FMT_LOC ": 0x%02X",state.ip,b);
    emit_memcopy(&b,1);
}

static bool emit_string(int offset)
{
    if (!state.linecopy) {
        error("Unable to emit string: line copy buffer is empty");
        return false;
    }

    bool escape = false;
    char* ptr = state.linecopy + offset;

    while (*ptr) {
        if (*ptr == '\\' && !escape) {
            escape = true;
            ptr++;
            continue;
        }

        if (escape) {
            escape = false;
            switch (*ptr) {
            case '\\':
            case '"': emit_byte(*ptr); break;
            case 'n': emit_byte('\n'); break;
            case '0': emit_byte(0); break;
            }
        } else if (*ptr == '"') break;
        else emit_byte(*ptr);

        ptr++;
    }

    return (*ptr == '"'); // should end with double quotes
}

static int process_line(char* instring)
{
    int fsm = ASM_STATE_START;
    int op = -1; // initialize to invalid value
    int dat = -1;
    int nargs = 0;
    int curarg = 0;
    int width = 0;
    char* macro_name = NULL;
    LOCTYPE* arg_label[ASSEMBLY_MAXARGS] = {0};
    LOCTYPE arg_value[ASSEMBLY_MAXARGS] = {0};
    bool repeat = false;
    bool nocode = false;
    bool done = false;
    bool stringarg = false;
    bool widthset = false;
    bytecode cur;
    memset(&cur,0,sizeof(cur));

    DBG("Processing line " FMT_LOC ": '%s'",state.line,instring);

    // tokenize string
    char* tok = strtok(instring,ASSEMBLY_WHITESPACE);
    while (tok && (!done)) {
        if (tok[0] == ';') break; // comment started

        switch (fsm) {
        case ASM_STATE_START:
            nargs = 0;
            curarg = 0;

            if (*tok == '.') { // is it a program offset?
                // try to resolve/scan it in advance
                errno = 0;
                int imod = strchr(ASSEMBLY_OFFSETMODS,tok[1])? 2:1;
                LOCTYPE* iptr = resolve_label(tok+imod,false,NULL);
                LOCTYPE ival = STRTOU64(tok+imod,NULL,0);
                if (!iptr && (errno || !isdigit(tok[imod]))) {
                    error("Unable to resolve program offset '%s'",tok);
                    return ASM_ERR_NOLABEL;
                }

                if (tok[1] == '+') { // is it an incremental offset?
                    LOCTYPE off = iptr? *iptr : ival;
                    state.ip += off;
                    DBG("IP advanced by " FMT_IP " to " FMT_IP,off,state.ip);

                } else if (tok[1] == '?') { // is it an alignment offset?
                    LOCTYPE b = iptr? *iptr : ival;
                    state.ip += b - (state.ip % b);
                    assert(state.ip % b == 0);
                    DBG("IP aligned to " FMT_LOC " -> " FMT_IP,b,state.ip);

                } else { // it has to be either a direct value, or an already known symbolic name
                    state.ip = iptr? *iptr : ival;
                    DBG("IP set to " FMT_IP,state.ip);
                }

                done = true;
                nocode = true;
                break;

            } else if (*tok == ':') { // is it a label?
                add_label(state.ip,tok+1,0,false,false,false);
                done = true;
                nocode = true;
                break;

            } else if (*tok == '#') { // is it a macro?
                // save the body and repeat token processing
                tok++;
                fsm = ASM_STATE_MACRO_NAME;
                repeat = true;
                break;

            } else if (*tok == '>' || *tok == '<') { // is it a prefixed block marker?
                if (*tok == '<') DBG("End of context block with prefix '%s'",state.prefix);
                if (state.prefix) free(state.prefix);
                state.prefix = (*tok == '>')? strdup(++tok) : NULL;
                if (*tok == '>') DBG("Begin of context block with prefix '%s'",state.prefix);
                done = true;
                nocode = true;
                break;
            }

            // is it a data marker?
            dat = find_in(data_labels,tok);
            if (dat >= 0) {
                width = 1 << dat; // data widths are powers of 2
                fsm = ASM_STATE_NAME;
                break;
            }

            // is it an operation? first let's check its width suffix
            widthset = false;
            if (strlen(tok) > ASSEMBLY_MNEMONICLEN) {
                dat = find_in(data_modifiers,tok+ASSEMBLY_MNEMONICLEN);
                if (dat < 0) {
                    error("Unknown modifier in token '%s'",tok);
                    return ASM_ERR_UNK_TOKEN;
                }
                cur.width = dat;
                tok[ASSEMBLY_MNEMONICLEN] = 0; // remove the suffix
                widthset = true;
                dat = -1; // do not confuse it with real data entries
            } else
                cur.width = ASSEMBLY_DEFWIDTH;

            // and now the operation itself
            op = find_in(asm_mnemonics,tok);
            if (op >= 0) {
                fsm = ASM_STATE_ARG;
                nargs = asm_instructions[op].n_args;
                width = sizeof(bytecode) + nargs * MACHINE_ARGFIELD;
                if (!widthset && asm_instructions[op].is_fpu) cur.width = ASSEMBLY_FLOATWIDTH;
            }

            // nothing helped, that shouldn't happen
            if (fsm == ASM_STATE_START) {
                error("Unknown token '%s'",tok);
                return ASM_ERR_UNK_TOKEN;
            }
            break;

        case ASM_STATE_NAME:
            nargs = 1;
            fsm = ASM_STATE_ARG;
            // data markers may have an optional name
            if (!isdigit(*tok) && *tok != '-' && *tok != '!') {
                add_label(state.ip,tok,0,false,false,false);
                break;
            }
            // otherwise, there's
            // no break

        case ASM_STATE_ARG:
            // is it a no-argument operation?
            if (nargs == curarg) {
                done = true;
                break;
            }

            // try to understand what the argument is
            switch (tok[0]) {
            case '%': // immediate value
                switch (curarg) {
                case 0: cur.dst_imm = true; break;
                case 1: cur.src1_imm = true; break;
                case 2: cur.src2_imm = true; break;
                }
                tok++; // business as usual
                break;

            case '@': // pointer argument
                switch (curarg) {
                case 0: cur.dst_ptr = true; break;
                case 1: cur.src1_ptr = true; break;
                case 2: cur.src2_ptr = true; break;
                }
                tok++;
                break;

            case '"': // string (only for data fields)
                if (op >= 0) {
                    error("String literal can only be used in data fields");
                    return ASM_ERR_SYNTAX;
                }
                tok++;
                stringarg = true;
                break;
            }
            if (stringarg) break;

            // if current token is a label
            arg_label[curarg] = resolve_label(tok,false,NULL);
            if (arg_label[curarg]) {
                curarg++;
                break;
            }

            // should be either a number, or a name now
            if (isdigit(tok[0]) || tok[0] == '-') {
                // looks like a number
                int r = asm_numeric_token(tok,arg_value+curarg,NULL);
                if (r) return r;

            } else if (tok[0] == '\'') {
                // it's a character literal
                arg_value[curarg] = tok[1];

            } else {
                // it's a name, add it to the 'pending' pool
                arg_value[curarg] = 0;
                if (dat >= 0) add_label(state.ip,tok,width,false,true,false);
                else add_label(state.ip + sizeof(bytecode) + curarg * MACHINE_ARGFIELD,tok,MACHINE_ARGFIELD,false,true,false);
            }

            curarg++;
            break;

        case ASM_STATE_MACRO_NAME:
            macro_name = tok;
            nocode = true;
            fsm = ASM_STATE_MACRO_BODY;
            break;

        case ASM_STATE_MACRO_BODY:
            // for now, we can just treat macros as labels but with IP field set to arbitrary value
            // (i.e., macros == named constants). Another difference is macro arguments should be
            // calculated immediately, as they could not be postponed (they have no memory area)
            bool flt = false;
            if (*tok == '!') {
                // it's an expression
                init_parser(tok+1);
                asm_term res = expression(0);
                switch (res.t) {
                case 1: arg_value[curarg] = res.v.u; break;
                case 2:
                    arg_value[curarg] = 0;
                    memcpy(arg_value+curarg,&res.v.f,sizeof(float));
                    flt = true;
                    break;
                default:
                    error("Expression calculation error - type inference failed");
                    return ASM_ERR_SYNTAX;
                }

            } else if ((!isdigit(*tok)) && *tok != '-') {
                // it's another macro or label name
                arg_label[curarg] = resolve_label(tok,false,NULL);
                if (!arg_label[curarg]) {
                    error("Unknown label/macro name '%s' used in macro body",tok);
                    return ASM_ERR_NOLABEL;
                }
                arg_value[curarg] = *(arg_label[curarg]);

            } else {
                // it's just a number
                int r = asm_numeric_token(tok,arg_value+curarg,&flt);
                if (r) return r;
            }
            add_label(arg_value[curarg],macro_name,0,true,false,flt);
            done = true;
            break;

        case ASM_STATE_CONCAT_ALL:
            append_pending(tok);
            break;

        default:
            error("Internal assembler error");
            return ASM_ERR_UNKNOWN;
        }
        if (stringarg) break;

        // next token, please
        if (!repeat)
            tok = strtok(NULL,ASSEMBLY_WHITESPACE);
        repeat = false;
    }

    // if we have valid string argument, it means we're populating a data entry
    if (stringarg && dat >= 0) done = true;

    // if we're concatenating everything, done flag wouldn't be set
    if (fsm == ASM_STATE_CONCAT_ALL) done = true;

    // if we've added all args, we're also done (no more tokens beyond last arg)
    if (fsm == ASM_STATE_ARG && curarg == nargs) done = true;

    // a line could end prematurely in case of uninitialized data area
    if (!done && dat >= 0) {
        state.ip += width; // simply skip this area
        return ASM_OK;
    }

    // check for completeness
    if (!done && nargs) {
        error("Unfinished line");
        return ASM_ERR_LINESTOP;
    }

    // OK, so some stuff (like labels) would not require to emit anything at all
    if (nocode) return ASM_OK; // nothing to do :)

    // if it's data, prepare the data, or emit the contents of a string
    if (dat >= 0) {
        if (stringarg) {
            // string emitter will advance IP accordingly
            if (!emit_string(tok-instring)) // send where the string has started
                return ASM_ERR_EMIT;
            return ASM_OK;
        }
        nargs = 1; // there's only one arg in data field
    }

    // if it's a command, emit the opcode
    if (op >= 0) emit_oper(op,cur);

    // emit all arguments
    for (int i = 0; i < nargs; i++) {
        if (arg_label[i]) arg_value[i] = *(arg_label[i]);
        emit_arg(arg_value[i],(dat >= 0)? width : MACHINE_ARGFIELD);
    }

    // check for errors and done
    return state.error? ASM_ERR_EMIT : ASM_OK;
}

static int resolve_pending()
{
    LOCTYPE oldip = state.ip;
    char* oldpfx = state.prefix;

    for (int i = 0; i < state.n_pending; i++) {
        LOCTYPE arg = 0;
        if (state.pending[i].label[0] == '!') {
            // it's an expression
            init_parser(state.pending[i].label+1);
            asm_term res = expression(0);
            switch (res.t) {
            case 1: arg = res.v.u; break;
            case 2: arg = 0; memcpy(&arg,&res.v.f,sizeof(float)); break;
            default:
                error("Expression calculation error - type inference failed");
                return ASM_ERR_SYNTAX;
            }

        } else {
            // it's just a label
            state.prefix = state.pending[i].prefix;
            LOCTYPE* parg = resolve_label(state.pending[i].label,false,NULL);
            if (!parg) {
                error("Unable to resolve label '%s'",state.pending[i].label);
                return ASM_ERR_NOLABEL;
            }
            arg = *parg;
        }

        state.ip = state.pending[i].addr;
        emit_arg(arg,state.pending[i].width);
    }

    state.ip = oldip;
    state.prefix = oldpfx;
    return state.error? ASM_ERR_EMIT : ASM_OK;
}

int assemble()
{
    int ret;
    char instring[ASSEMBLY_MAXLEN] = {0};

    state.ip = 0;
    state.error = false;
    while (fgets(instring,sizeof(instring),state.file)) {
        state.line++;
        strtrim_right(instring);

        // skip empty strings
        if (!instring[0]) continue;
        strtrim_left(instring);

        // skip comment-only lines
        if (instring[0] == ';') continue;

        // check for the STOP marker
        if (!strcmp(instring,ASSEMBLY_STOPMARK)) break;

        // preserve a copy of the line for various uses
        if (state.linecopy) free(state.linecopy);
        state.linecopy = strdup(instring);

        // tokenize and process the line
        ret = process_line(instring);
        if (ret) return ret;
        if (state.error) return ASM_ERR_SYNTAX;
    }

    // free last line's copy
    if (state.linecopy) free(state.linecopy);
    state.linecopy = NULL;

    // resolve 'pending' pool
    state.error = false;
    ret = resolve_pending();
    if (ret) return ret;
    if (state.error) return ASM_ERR_SYNTAX;

    // dump textual representation if requested
    if (state.dumptxt) {
        for (size_t i = 0; i < state.outsize; i++) {
            if (i % 16 == 0) putchar('\n');
            printf("%02X ",state.outmem[i]);
            if (state.cutatip && i > state.ip) break;
        }
        putchar('\n');
    }

    // list identifiers
    for (int i = 0; i < state.n_labels; i++) {
        asm_label* p = state.labels + i;
        if (state.listmacro && p->ismacro) {
            if (p->isfloat) {
                float f;
                memcpy(&f,&p->addr,sizeof(float));
                printf("Macro %s.%s = %f\n",p->prefix,p->label,f);
            } else
                printf("Macro %s.%s = " FMT_LOC "\n",p->prefix,p->label,p->addr);

        } else if (state.listunused && !p->refs)
            printf("%s %s.%s (line %d) is unused\n",(p->ismacro? "Macro":"Ref/Label"),p->prefix,p->label,p->line);
    }

    // and we're done :)
    return ASM_OK;
}

static inline bool is_clobber(execstate* S, LOCTYPE loc, int* idx)
{
    for (int i = 0; i < S->n_clobs; i++) {
        if (S->clob_addr[i] == loc) {
            if (idx) *idx = i * sizeof(LOCTYPE);
            return true;
        }
    }
    return false;
}

static inline void add_clobber(execstate* S, LOCTYPE addr)
{
    if (S->n_clobs >= MACHINE_PPUCLOBS) {
        S->exception = EXCPT_NO_CLOBBER;
        return;
    }
    S->clob_addr[S->n_clobs++] = addr;
}

static inline uint8_t* mmu(execstate* S, LOCTYPE addr, bool wr)
{
    if (addr >= S->size) {
        if (S->ext && addr >= S->extstart && addr < (S->extstart + S->extsize)) {
            if (!wr || S->ext_rw) return S->ext + (addr - S->extstart);
            else
                ERR("MMU exception: attempt to write to read-only memory @ " FMT_IP,addr);
        } else
            ERR("MMU exception: attempt to access memory @ " FMT_IP,addr);

        S->exception = EXCPT_MEM_BOUND;
        return NULL;
    }

    int clobb;
    if (is_clobber(S,addr,&clobb)) return S->clob_cont + clobb;

    return S->mem + addr;
}

static inline LOCTYPE rdval(execstate* S, bool isptr, bool isimm, int width, LOCTYPE* ptrto)
{
    LOCTYPE out = 0;

    // extract the field itself
    if (ptrto) *ptrto = S->ip;
    uint8_t* ptr = mmu(S,S->ip,false);
    if (ptr) memcpy(&out,ptr,isimm? width : MACHINE_ARGFIELD);

    if (!isimm) { // not immediate value - dereference
        if (ptrto) *ptrto = out;
        ptr = mmu(S,out,false);
        out = 0; // reset out as it can be contaminated by previous memcpy (if width differs)
        if (ptr) memcpy(&out,ptr,isptr? MACHINE_ARGFIELD : width);
    }

    if (isptr) { // pointer - dereference again
        if (ptrto) *ptrto = out;
        ptr = mmu(S,out,false);
        out = 0;
        if (ptr) memcpy(&out,ptr,width);
    }

    S->ip += MACHINE_ARGFIELD; // step is always one full machine double-word
    return out;
}

static inline void sign_extend(LOCTYPE* v, int width)
{
    switch (width) {
    case 1: if (*v & 0x80) *v |= 0xFFFFFFFFFFFFFF00; break;
    case 2: if (*v & 0x8000) *v |= 0xFFFFFFFFFFFF0000; break;
    case 4: if (*v & 0x80000000) *v |= 0xFFFFFFFF00000000; break;
    }
}

#define COMPARER(T,N) static inline uint8_t N (T a, T b) {\
    if (a > b) return 2; \
    else if (a < b) return 4; \
    else return 1; }

COMPARER(LOCTYPE,compareu)
COMPARER(int64_t,compares)
COMPARER(float,comparef)

static void output(execstate* S, int stream, LOCTYPE data, int width)
{
    DBG("OUTPUT [%d]: " FMT_IP,stream,data);
    switch (stream) {
    case MACHINE_IOCHAR:
        {
            char x = (char)data;
            if (isprint(x) || x == '\n') putchar(x);
        }
        break;

    case MACHINE_IOUINT:
        printf(FMT_LOC,data);
        break;

    case MACHINE_IOINT:
        sign_extend(&data,width);
        printf(FMT_SGN,(int64_t)data);
        break;

    case MACHINE_IOFLOAT:
        {
            float f;
            memcpy(&f,&data,4);
            printf("%f",f);
        }
        break;

    case MACHINE_IOSTRING:
        {
            DBG("Attempting to print string @" FMT_LOC,data);
            char* s = (char*)mmu(S,data,false);
            if (s) printf("%s",s);
        }
        break;

    case MACHINE_IOPOINTER:
        printf(FMT_IP,data);
        break;

    default:
        ERR("\nUnknown/unimplemented stream %d\n",stream);
    }
    fflush(stdout);
}

static LOCTYPE input(int stream)
{
    DBG("INPUT [%d]",stream);
    LOCTYPE data = 0;

    switch (stream) {
    case MACHINE_IOCHAR:
        data = getchar(); //FIXME: getchar is blocking until RET, need read() maybe?
        break;

    case MACHINE_IOUINT:
        scanf(FMT_LOC,&data);
        break;

    case MACHINE_IOINT:
        scanf(FMT_SGN,&data);
        break;

    case MACHINE_IOFLOAT:
        {
            float f;
            scanf("%f",&f);
            memcpy(&data,&f,4);
        }
        break;

    default:
        ERR("\nUnknown/unimplemented stream %d\n",stream);
    }
    return data;
}

static void start_ppu(execstate* S, LOCTYPE addr, LOCTYPE size, LOCTYPE end)
{
    execstate org = *S;
    PARALLEL for (LOCTYPE i = 0; i < size; i++) {
        // spawn new VM
        execstate ppu = org;
        ppu.clob_addr[0] = addr; // ID field itself is the first clobber
        memcpy(ppu.clob_cont,&i,sizeof(LOCTYPE));
        ppu.n_clobs = 1;
        ppu.start = org.ip;
        ppu.barrier = end;
        run(&ppu);
    }
    S->ip = end;
}

static float sdpu(execstate* S)
{
    int32_t i,sum;
    uint32_t scnt = 0;
    float acc = 0;
    uint8_t* aptr = mmu(S,S->dpu_regs[MACHINE_DPU_AVSTART],false);
    uint8_t* bptr = mmu(S,S->dpu_regs[MACHINE_DPU_BVSTART],false);
    uint8_t* asptr = mmu(S,S->dpu_regs[MACHINE_DPU_ASSTART],false);
    uint8_t* bsptr = mmu(S,S->dpu_regs[MACHINE_DPU_BSSTART],false);
    uint8_t* end = mmu(S,S->dpu_regs[MACHINE_DPU_AVEND],false);

    while (aptr < end) {
        sum = 0;
        for (i = 0; i < S->dpu_regs[MACHINE_DPU_VSEQLEN]; i++,aptr++,bptr++)
            sum += (int32_t)(*(int8_t*)aptr) * (int32_t)(*(int8_t*)bptr);

        aptr += S->dpu_regs[MACHINE_DPU_VPAD];
        bptr += S->dpu_regs[MACHINE_DPU_VPAD];

        acc += (*(float*)asptr) * (*(float*)bsptr) * (float)sum;
        asptr += sizeof(float);
        bsptr += sizeof(float);
        if (++scnt >= S->dpu_regs[MACHINE_DPU_SSEQLEN]) {
            scnt = 0;
            asptr += S->dpu_regs[MACHINE_DPU_SPAD];
            bsptr += S->dpu_regs[MACHINE_DPU_SPAD];
        }
    }
    return acc;
}

void run(execstate* S)
{
    bool done = false;
    uint8_t flags = 0;
    if (S->timing) S->start_us = get_us();

    while (!done && S->ip < S->barrier && !S->exception) {
        LOCTYPE myip = S->ip;
        bytecode cmd;
        memcpy(&cmd,S->mem+S->ip,sizeof(cmd));
        S->ip += sizeof(cmd);

        enum instructions_e id = INST_INVALID;
        for (int i = 0; i < NUMITEMS(asm_instructions); i++) {
            if (asm_instructions[i].opcode == cmd.opcode && asm_instructions[i].is_fpu == cmd.fpu_op) {
                id = (enum instructions_e)i;
                break;
            }
        }
        if (id == INST_INVALID) {
            ERR("Unknown instruction at " FMT_LOC,myip);
            S->exception = EXCPT_INST_INVALID;
            break;
        }

        int width = 1 << cmd.width;
        LOCTYPE dest = 0;

        if (id == INST_fti) width = 4; // force input width to 4 as floats can't be of different size, but the output can

        uvalue arg1,arg2,arg3;
        arg1.u = cmd.first_arg? rdval(S,cmd.dst_ptr,cmd.dst_imm,width,&dest) : 0;
        arg2.u = cmd.second_arg? rdval(S,cmd.src1_ptr,cmd.src1_imm,width,NULL) : 0;
        arg3.u = cmd.third_arg? rdval(S,cmd.src2_ptr,cmd.src2_imm,width,NULL) : 0;

        if (cmd.opcode & MACHINE_OP_SIGND) {
            sign_extend(&arg1.u,width);
            sign_extend(&arg2.u,width);
            sign_extend(&arg3.u,width);
        }

        bool w = cmd.opcode & MACHINE_OP_ARITH;

        DBG("IP " FMT_LOC " : %s, " FMT_LOC " [" FMT_LOC "], " FMT_LOC ", " FMT_LOC " %s",myip,asm_instructions[id].name,dest,arg1.u,arg2.u,arg3.u,w?"writeback":"");

        switch (id) {
        case INST_nop: break;
        case INST_hlt: done = true; break;

        case INST_mov: arg1.u = arg2.u; w = true; break;

        case INST_cmp: flags = compareu(arg1.u,arg2.u); break;
        case INST_cms: flags = compares(arg1.s,arg2.s); break;
        case INST_cmf: flags = comparef(arg1.f,arg2.f); break;

        case INST_add: arg1.u = arg2.u + arg3.u; break;
        case INST_adf: arg1.f = arg2.f + arg3.f; break;

        case INST_sub: arg1.s = arg2.s - arg3.s; break;
        case INST_suu: arg1.u = arg2.u - arg3.u; break;
        case INST_suf: arg1.f = arg2.f - arg3.f; break;

        case INST_neg: arg1.s = -arg2.s; break;
        case INST_ngf: arg1.f = -arg2.f; break;

        case INST_sex: arg1.s = arg2.s; width = arg3.u; break;

        case INST_and: arg1.u = arg2.u & arg3.u; break;
        case INST_ior: arg1.u = arg2.u | arg3.u; break;
        case INST_xor: arg1.u = arg2.u ^ arg3.u; break;
        case INST_shl: arg1.u = arg2.u << arg3.u; break;
        case INST_shr: arg1.u = arg2.u >> arg3.u; break;
        case INST_not: arg1.u = ~arg2.u; break;

        case INST_mul: arg1.u = arg2.u * arg3.u; break;
        case INST_muf: arg1.f = arg2.f * arg3.f; break;

        case INST_div: if (arg3.s) arg1.s = arg2.s / arg3.s; else S->exception = EXCPT_DIV_BY_ZERO; break;
        case INST_diu: if (arg3.u) arg1.u = arg2.u / arg3.u; else S->exception = EXCPT_DIV_BY_ZERO; break;
        case INST_mod: if (arg3.u) arg1.u = arg2.u % arg3.u; else S->exception = EXCPT_DIV_BY_ZERO; break;
        case INST_dif: if (arg3.f != 0.f) arg1.f = arg2.f / arg3.f; else S->exception = EXCPT_DIV_BY_ZERO; break;

        case INST_ivf: if (arg2.f != 0.f) arg1.f = 1.f / arg2.f; else S->exception = EXCPT_DIV_BY_ZERO; break;

        case INST_abf: arg1.f = fabsf(arg2.f); break;
        case INST_sqr: arg1.f = sqrtf(arg2.f); break;
        case INST_exp: arg1.f = expf(arg2.f); break;
        case INST_sin: arg1.f = sinf(arg2.f); break;
        case INST_cos: arg1.f = cosf(arg2.f); break;
        case INST_rnf: arg1.f = roundf(arg2.f); break;

        case INST_pow: arg1.f = powf(arg2.f,arg3.f); break;

        case INST_itf: arg1.f = arg2.s; width = 4; break;
        case INST_fti: arg1.s = arg2.f; width = 1 << cmd.width; break;

        case INST_sip: arg1.u = S->ip; w = true; break;
        case INST_sfl: arg1.u = flags; w = true; break;
        case INST_jif: if (flags & (uint8_t)arg2.u) S->ip = dest; break;
        case INST_jmp: S->ip = dest; break;

        case INST_ppu: start_ppu(S,dest,arg2.u,arg3.u); break;
        case INST_clo: add_clobber(S,dest); break;

        case INST_dpu: arg1.f = sdpu(S); break;
        case INST_dpr: arg1.u = S->dpu_regs[arg2.u]; break;
        case INST_dpw: S->dpu_regs[arg1.u] = arg2.u; break;

        case INST_out: output(S,arg2.u,arg1.u,width); break;
        case INST_inp: arg1.u = input(arg2.u); break;

        case INST_INVALID: break;
        }

        if (cmd.opcode & MACHINE_OP_ARITH) {
            flags &= ~(MACHINE_FLG_ZERO);
            if (cmd.fpu_op? (arg1.f == 0.f) : (arg1.u == 0)) flags |= MACHINE_FLG_ZERO;
        }

        if (!w) continue;

        uint8_t* resptr = mmu(S,dest,true);
        if (!resptr) break;

        memcpy(resptr,&arg1,width);
    }

    if (S->timing) {
        S->start_us = get_us() - S->start_us;
        printf("[TIMING] Executed in " FMT_LOC " us\n",S->start_us);
    }
}

static void init_static()
{
    assert(sizeof(bytecode) == 4);
    assert(NUMITEMS(asm_instructions) == INST_INVALID);

    for (int i = 0; i < NUMITEMS(asm_instructions); i++) {
        // check opcode uniqueness
        for (int j = 0; j < NUMITEMS(asm_instructions); j++) {
            if (i == j) continue;
            if (asm_instructions[i].opcode == asm_instructions[j].opcode && asm_instructions[i].is_fpu == asm_instructions[j].is_fpu) {
                ERR("General assembler error: non-unique opcode 0x%02X in %s and %s",asm_instructions[j].opcode,asm_instructions[j].name,asm_instructions[i].name);
                abort();
            }
        }
        // put into mnemonics shortcut table
        asm_mnemonics[i] = asm_instructions[i].name;
    }
    DBG("%d instructions registered",(int)NUMITEMS(asm_instructions));

    memset(&state,0,sizeof(state));
    memset(&main_exe,0,sizeof(main_exe));
    main_exe.ext_file = -1;

#ifdef DEFAULT_MEMORY
    state.outsize = DEFAULT_MEMORY;
    state.outmem = (uint8_t*)malloc(state.outsize);
#endif
}

static bool init_mmap(const char* arg)
{
#ifdef USE_MMAP
    char* end;
    errno = 0;
    LOCTYPE addr = STRTOU64(arg,&end,0);
    if (!end || errno || end[0] != ':' || !end[1]) {
        ERR("Unable to parse memory map argument '%s'; should be <start_addr>:<filename>",arg);
        return false;
    }

    main_exe.ext_file = open(++end,O_RDONLY);
    if (main_exe.ext_file == -1) {
        ERR("Unable to open file %s",end);
        return false;
    }

    struct stat st;
    if (fstat(main_exe.ext_file,&st)) {
        ERR("Unable to stat() file %s",end);
        return false;
    }

    main_exe.extstart = addr;
    main_exe.extsize = st.st_size;

    int flags = PROT_READ;
    if (main_exe.ext_rw) flags |= PROT_WRITE;

    main_exe.ext = (uint8_t*)mmap(NULL,st.st_size,flags,MAP_SHARED,main_exe.ext_file,0);
    if (main_exe.ext == MAP_FAILED) {
        ERR("Unable to mmap file %s",end);
        return false;
    }
    return true;
#else
    return false;
#endif
}

static void usage(const char* prog)
{
    printf("Use: %s [OPTIONS]\n",prog);
    puts("\t-i <input_assembly.asm>");
    puts("\t-o <output_binary.bin>");
    puts("\t-m <memory_size_in_bytes>");
    puts("\t-e - execute binary after translation");
    puts("\t-d - dump binary as hex dump to stdout");
    puts("\t-M <addr>:<file> - memory-map a file starting at address <addr>");
    puts("\t-W - allow write access to memory-mapped file");
    puts("\t-T - enable execution timing");
    puts("\t-l - list macros and their computed values");
    puts("\t-u - list unused identifiers");
    puts("\t-c - cut output at last value of IP");
}

int parsearg(int argc, char* argv[])
{
    int opt;
    while ((opt = getopt(argc,argv,"i:o:m:edM:WTluc")) != -1) {
        switch (opt) {
        case 'i':
            state.fname = optarg;
            state.file = fopen(optarg,"r");
            break;

        case 'o':
            state.outname = optarg;
            break;

        case 'm':
            state.outsize = STRTOU64(optarg,NULL,0);
            if (state.outsize) state.outmem = (uint8_t*)malloc(state.outsize);
            break;

        case 'M':
            if (!init_mmap(optarg)) return GERR_MMAP;
            break;

        case 'e': main_exe.exec = true; break;
        case 'd': state.dumptxt = true; break;
        case 'W': main_exe.ext_rw = true; break;
        case 'T': main_exe.timing = true; break;
        case 'l': state.listmacro = true; break;
        case 'u': state.listunused = true; break;
        case 'c': state.cutatip = true; break;

        default:
            ERR("Unknown switch '%c'",opt);
            return 1;
        }
    }

    if (!state.file) return GERR_NOINPUT;
    return 0;
}

int main(int argc, char* argv[])
{
    puts("EPIA Assembler/Emulator ver. " VERSION);
    puts("(C) Dmitry 'sciloaf' Solovyev aka MatrixS_Master, 2025\n");

    setlocale(LC_ALL,"C");
    init_static();

    int r = parsearg(argc,argv);
    if (r) {
        usage(argv[0]);
        return r;
    }

    r = assemble();
    if (r) {
        if (r < ASM_NUMERRORS)
            ERR("Assembler error: %s",asm_errornames[r]);
        return r;
    }

    if (state.outname) {
        r = dumpmem(state.outname,state.outmem,state.cutatip? state.ip:state.outsize);
        if (r) return r;
    }

    if (main_exe.exec) {
        main_exe.mem = state.outmem;
        main_exe.ip = 0;
        main_exe.size = state.outsize;
        main_exe.barrier = state.outsize;
        run(&main_exe);

        if (main_exe.exception) {
            if (main_exe.exception < EXCPT_NUM_EXCEPTIONS)
                ERR("CPU exception caught: %s",cpu_exceptions[main_exe.exception]);
            return GERR_CPUEXCPT;
        }
    }

    puts("Done.");
    return GERR_OK;
}
