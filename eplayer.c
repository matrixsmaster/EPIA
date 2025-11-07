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

#define VERSION "0.1.1"

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
//#define MACHINE_OP_MASK  0xFF2
//#define MACHINE_OP_SHIFT 20

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
    GERR_OK,
    GERR_NOINPUT,
    GERR_INPERR,
    GERR_OUTERR,
    GERR_CPUEXCPT,
    GERR_MMAP,
    GERR_NOMEM,
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
    MACHINE_DPU_AVSTART,
    MACHINE_DPU_AVEND,
    MACHINE_DPU_ASSTART,
    MACHINE_DPU_BVSTART,
    MACHINE_DPU_BSSTART,
    MACHINE_DPU_VSEQLEN,
    MACHINE_DPU_VPAD,
    MACHINE_DPU_SSEQLEN,
    MACHINE_DPU_SPAD,
    MACHINE_DPU_NREGS
};

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

enum instructions_e {
    INST_nop = 0b000000000,
    INST_hlt = 0b011111110,

    INST_mov = 0b000000010,

    INST_cmp = 0b000001100,
    INST_cms = 0b010001100,
    INST_add = 0b100000010,
    INST_sub = 0b110000100,
    INST_suu = 0b100000100,
    INST_mul = 0b100000110,
    INST_div = 0b110001000,
    INST_diu = 0b100001000,
    INST_mod = 0b100001010,
    INST_neg = 0b110001100,
    INST_sex = 0b110001110,
    INST_and = 0b100010000,
    INST_ior = 0b100010010,
    INST_xor = 0b100010100,
    INST_not = 0b100011000,
    INST_shl = 0b100011010,
    INST_shr = 0b100011100,

    INST_cmf = 0b000001011,
    INST_adf = 0b100000011,
    INST_suf = 0b100000101,
    INST_muf = 0b100000111,
    INST_dif = 0b100001001,
    INST_ngf = 0b100001101,
    INST_ivf = 0b100011001,
    INST_abf = 0b101000001,
    INST_rnf = 0b101000011,
    INST_sqr = 0b101000101,
    INST_pow = 0b101000111,
    INST_exp = 0b101001001,
    INST_sin = 0b101001011,
    INST_cos = 0b101001101,

    INST_itf = 0b111100001,
    INST_fti = 0b101100010,

    INST_sip = 0b000100000,
    INST_sfl = 0b000100010,
    INST_jif = 0b000100100,
    INST_jmp = 0b000100110,

    INST_ppu = 0b001110000,
    INST_clo = 0b001110010,

    INST_dpu = 0b101010001,
    INST_dpr = 0b101010010,
    INST_dpw = 0b001010100,

    INST_out = 0b001111010,
    INST_inp = 0b101111100,
};

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

typedef union {
    LOCTYPE u;
    int64_t s;
    float f;
} uvalue;

typedef struct {
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

void run(execstate* S);

LOCTYPE get_us()
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC,&ts);
    return ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
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
        //LOCTYPE myip = S->ip;
        bytecode cmd;
        memcpy(&cmd,S->mem+S->ip,sizeof(cmd));
        S->ip += sizeof(cmd);

        uint32_t id = ((uint32_t)cmd.opcode << 1) | cmd.fpu_op;

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

        default: S->exception = EXCPT_INST_INVALID; return;
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

static bool open_stat(const char* fn, int* fd, LOCTYPE* size)
{
    int f = open(fn,O_RDONLY);
    if (f == -1) {
        ERR("Unable to open file %s",fn);
        return false;
    }

    struct stat st;
    if (fstat(f,&st)) {
        ERR("Unable to stat() file %s",fn);
        return false;
    }

    if (fd) *fd = f;
    if (size) *size = st.st_size;
    return true;
}

static bool init_mmap(const char* fn, LOCTYPE addr)
{
#ifdef USE_MMAP
    LOCTYPE size;
    if (!open_stat(fn,&main_exe.ext_file,&size)) return false;

    main_exe.extstart = addr;
    main_exe.extsize = size;

    int flags = PROT_READ;
    if (main_exe.ext_rw) flags |= PROT_WRITE;

    main_exe.ext = (uint8_t*)mmap(NULL,size,flags,MAP_SHARED,main_exe.ext_file,0);
    if (main_exe.ext == MAP_FAILED) {
        ERR("Unable to mmap file %s",fn);
        return false;
    }
    return true;
#else
    return false;
#endif
}

static bool load_exe(const char* fn)
{
    int fd;
    LOCTYPE size;
    if (!open_stat(fn,&fd,&size)) return false;

    if (size > main_exe.size) {
        ERR("Not enough RAM to hold the executable (RAM size is " FMT_LOC " bytes; Executable is " FMT_LOC " bytes)",main_exe.size,size);
        close(fd);
        return false;
    }

    LOCTYPE r = read(fd,main_exe.mem,size);
    close(fd);

    if (r != size) {
        ERR("Unable to read the whole file %s",fn);
        return false;
    }
    return true;
}

int main(int argc, char* argv[])
{
    puts("EPIA Player ver. " VERSION);
    puts("(C) Dmitry 'sciloaf' Solovyev aka MatrixS_Master, 2025\n");
    setlocale(LC_ALL,"C");

    if (argc < 2) {
        printf("Use: %s <executable> [RAMsize] [<mapping_address> <mapping_file>]\n",argv[0]);
        return GERR_NOINPUT;
    }

    memset(&main_exe,0,sizeof(main_exe));

    if (argc > 2) main_exe.size = STRTOU64(argv[2],NULL,0);
    else main_exe.size = DEFAULT_MEMORY;

    if (!main_exe.size) return GERR_NOMEM;
    main_exe.mem = (uint8_t*)malloc(main_exe.size);
    if (!main_exe.mem) return GERR_NOMEM;

    if (!load_exe(argv[1])) return GERR_NOINPUT;

    main_exe.ext_file = -1;
    if (argc == 5) {
        if (!init_mmap(argv[4],STRTOU64(argv[3],NULL,0))) return GERR_MMAP;
    }

    main_exe.barrier = main_exe.size;
    run(&main_exe);

    if (main_exe.ext_file != -1) close(main_exe.ext_file);

    if (main_exe.exception) {
        if (main_exe.exception < EXCPT_NUM_EXCEPTIONS)
            ERR("CPU exception caught: %s",cpu_exceptions[main_exe.exception]);
        return GERR_CPUEXCPT;
    }

    puts("Done.");
    return GERR_OK;
}
