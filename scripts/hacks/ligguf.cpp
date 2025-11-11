/* LiGGUF - a tiny, dependency-free LLaMA inference engine with direct GGUF support
 * (C) Dmitry 'sciloaf' Solovyev aka MatrixS_Master, 2025
 *
 * C++ version
 *
 * Quickly hacked to produce model conversions for EPIA NPU LLaMA-2 demo
 * */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <vector>
#include <string>
#include <map>

#define FORCE_CONTEXT 1024

#define ALIGNMENT 32
#define MAXNAMELEN 1024
#define TOKENS_KEY "tokenizer.ggml.tokens"
#define TOKENS_SCORE_KEY "tokenizer.ggml.scores"
#define TOKENS_EMBED_KEY "token_embd.weight"
#define TOKENS_BOS_KEY "tokenizer.ggml.bos_token_id"
#define TOKENS_EOS_KEY "tokenizer.ggml.eos_token_id"
#define VOCAB_SIZE_KEY "llama.vocab_size"
#define CONTEXT_LEN_KEY "llama.context_length"
#define EMBED_LEN_KEY "llama.embedding_length"
#define HEAD_COUNT_KEY "llama.attention.head_count"
#define HEAD_KV_COUNT_KEY "llama.attention.head_count_kv"
#define BLOCK_COUNT_KEY "llama.block_count"
#define RMS_EPSILON_KEY "llama.attention.layer_norm_rms_epsilon"
#define ROPE_BASE_KEY "llama.rope.freq_base"
#define ROPE_DIMS_KEY "llama.rope.dimension_count"
#define OUTPUT_KEY "output.weight"
#define OUTPUT_NORM_KEY "output_norm.weight"
#define LAYER_ATT_NORM_KEY "blk.%d.attn_norm.weight"
#define LAYER_ATT_Q_KEY "blk.%d.attn_q.weight"
#define LAYER_ATT_K_KEY "blk.%d.attn_k.weight"
#define LAYER_ATT_V_KEY "blk.%d.attn_v.weight"
#define LAYER_ATT_OUT_KEY "blk.%d.attn_output.weight"
#define FF_LEN_KEY "llama.feed_forward_length"
#define LAYER_FFN_NORM_KEY "blk.%d.ffn_norm.weight"
#define LAYER_FFN_UP_KEY   "blk.%d.ffn_up.weight"    // w1
#define LAYER_FFN_DOWN_KEY "blk.%d.ffn_down.weight"  // w2
#define LAYER_FFN_GATE_KEY "blk.%d.ffn_gate.weight"  // w3
#define QK8_0 32 // From llama.cpp

#define ERR(S,...) fprintf(stderr,S "\n", __VA_ARGS__)
#define DBG(S,...) fprintf(stderr,S "\n",__VA_ARGS__)

using namespace std;

enum gguf_type {
    F32, F16,
    Q4_0, Q4_1, Q4_2, Q4_3,
    Q5_0, Q5_1,
    Q8_0, Q8_1,
    Q2_K, Q3_K, Q4_K, Q5_K, Q6_K, Q8_K,
    IQ2_XXS, IQ2_XS, IQ3_XXS,
    IQ1_S, IQ4_NL, IQ3_S, IQ2_S, IQ4_XS,
    I8, I16, I32, I64,
    F64,
    IQ1_M,
    GGUF_TYPE_COUNT
};

enum gguf_val_type {
    GUINT8, GINT8, GUINT16, GINT16, GUINT32, GINT32,
    GFLOAT32, GBOOL, GSTRING, GARRAY, GUINT64, GINT64, GFLOAT64,
    GGUF_VAL_TYPE_COUNT
};

struct gguf_kv {
    uint64_t off;
    gguf_val_type tag;
};

struct gguf_tensor {
    uint64_t off;
    vector<uint64_t> dims;
    gguf_type type;
};

struct block_q8_0 { // From llama.cpp
    uint16_t d;
    int8_t qs[QK8_0];
};

struct block_q8_f {
    float d;
    int8_t qs[QK8_0];
};

typedef vector<block_q8_0> qtensor;
typedef vector<float> ftensor;

struct model_state {
    int file; // model file handle
    uint8_t* base; // base mmap address
    uint64_t fsize; // model file size
    uint8_t* tensors_off; // aligned offset of the start of tensors block

    int vocab_size; // size of the vocabulary
    int n_layers; // number of layers
    int n_heads; // number of Query heads
    int n_kv_heads; // number of Key/Value heads
    int n_embed; // input embedding size
    int n_context; // size of the context window
    int tok_bos, tok_eos; // BOS/EOS token IDs
    int head_dim; // head size (computed)
    float rms_epsilon; // epsilon for RMS norm
    float rope_base; // RoPE base freq
    int rope_dim; // RoPE dimension (normally should be equal to head_dim, but could be smaller)
    int n_ff; // Feed-Forward length

    vector<string> tokens; // vector of known tokens (pos == index)
    map<string,int> tokens_rev; // reverse map of tokens (by token string) for convenience
    ftensor tokscores; // token scores (const)
    map<string,gguf_kv> meta_kv; // key-value pairs with model's metadata
    map<string,gguf_tensor> tensors; // map of all tensors in model file
};

model_state g_m;

static inline float fp32_from_bits(uint32_t w)
{
    float r;
    memcpy(&r,&w,4);
    return r;
}

static inline uint32_t fp32_to_bits(float f)
{
    uint32_t r;
    memcpy(&r,&f,4);
    return r;
}

static inline float fp16_to_fp32(uint16_t h)
{
    const uint32_t w = ((uint32_t)h) << 16;
    const uint32_t sign = w & 0x80000000;
    const uint32_t two_w = w << 1;

    const uint32_t exp_offset = 0xE0U << 23;
    const float exp_scale = fp32_from_bits(0x7800000);

    const float normalized_value = fp32_from_bits((two_w >> 4) + exp_offset) * exp_scale;

    const uint32_t magic_mask = 126U << 23;
    const float magic_bias = 0.5f;
    const float denormalized_value = fp32_from_bits((two_w >> 17) | magic_mask) - magic_bias;

    const uint32_t denormalized_cutoff = 1U << 27;
    const uint32_t result = sign | (two_w < denormalized_cutoff ? fp32_to_bits(denormalized_value) : fp32_to_bits(normalized_value));
    return fp32_from_bits(result);
}

static inline uint16_t fp32_to_fp16(float f)
{
    const float scale_to_inf = fp32_from_bits(0x77800000);
    const float scale_to_zero = fp32_from_bits(0x08800000);

    float base = (fabsf(f) * scale_to_inf) * scale_to_zero;

    const uint32_t w = fp32_to_bits(f);
    const uint32_t shl1_w = w << 1;
    const uint32_t sign = w & 0x80000000;
    uint32_t bias = shl1_w & 0xFF000000;
    if (bias < 0x71000000) bias = 0x71000000;

    base += fp32_from_bits((bias >> 1) + 0x07800000);
    const uint32_t bits = fp32_to_bits(base);
    const uint32_t exp_bits = (bits >> 13) & 0x00007C00;
    const uint32_t mantissa_bits = bits & 0x00000FFF;
    const uint32_t nonsign = exp_bits + mantissa_bits;
    return (sign >> 16) | ((shl1_w > 0xFF000000) ? (uint16_t)0x7E00 : nonsign);
}

void open_mmap(const char* fn)
{
    g_m.fsize = 0;
    g_m.base = NULL;
    g_m.file = open(fn,O_RDONLY);
    assert(g_m.file != -1);

    struct stat st;
    assert(!fstat(g_m.file,&st));

    g_m.fsize = st.st_size;
    g_m.base = (uint8_t*)mmap(NULL,g_m.fsize,PROT_READ,MAP_SHARED,g_m.file,0);
    assert(g_m.base != MAP_FAILED);
}

// unified reading from memory
#define RDMEM(T,N) T inline N (uint8_t** pos)\
{\
    T r = 0;\
    memcpy(&r,*pos,sizeof(T));\
    *pos += sizeof(T);\
    return r;\
}

RDMEM(uint32_t,rd32)
RDMEM(uint64_t,rd64)
RDMEM(float,rdf32)

string rdstr(uint8_t** pos)
{
    uint64_t l = rd64(pos);
    string res((const char*)*pos,l);
    *pos += l;
    return res;
}

uint32_t inline kvrd32(string key)
{
    assert(g_m.meta_kv.count(key));
    uint8_t* p = g_m.base + g_m.meta_kv[key].off;
    return rd32(&p);
}

float inline kvrdf32(string key)
{
    assert(g_m.meta_kv.count(key));
    uint8_t* p = g_m.base + g_m.meta_kv[key].off;
    return rdf32(&p);
}

uint64_t skipper(gguf_val_type t, uint8_t* pos)
{
    switch(t){
        case GUINT8:
        case GINT8:
        case GBOOL:
            return 1;
        case GUINT16:
        case GINT16:
            return 2;
        case GUINT32:
        case GINT32:
        case GFLOAT32:
            return 4;
        case GUINT64:
        case GINT64:
        case GFLOAT64:
            return 8;
        case GSTRING: {
            uint64_t l = rd64(&pos);
            return l + 8;
        }
        case GARRAY: {
            uint8_t* org = pos;
            gguf_val_type nt = (gguf_val_type)rd32(&pos);
            uint64_t ne = rd64(&pos);
            for (uint64_t i = 0; i < ne; i++) pos += skipper(nt,pos);
            return pos - org;
        }
        default:
            assert(false);
    }
}

void read_gguf()
{
    uint8_t* p = g_m.base;
    uint32_t magic = rd32(&p);
    assert(!memcmp(&magic,"GGUF",4));
    uint32_t ver = rd32(&p);
    assert(ver == 3);
    uint64_t nten = rd64(&p);
    uint64_t meta = rd64(&p);

    gguf_kv kv;
    for (uint64_t i = 0; i < meta; i++) {
        string key = rdstr(&p);

        kv.tag = (gguf_val_type)rd32(&p);
        kv.off = p - g_m.base;
        g_m.meta_kv[key] = kv;

        p += skipper(kv.tag,p);
    }

    for (uint64_t i=0; i < nten; i++) {
        string key = rdstr(&p);
        uint32_t ndim = rd32(&p);

        gguf_tensor tz;
        tz.dims.resize(ndim);
        for (uint32_t j = 0; j < ndim; j++)
            tz.dims[j] = rd64(&p);

        tz.type = (gguf_type)rd32(&p);
        tz.off = rd64(&p);
        g_m.tensors[key] = tz;
    }

    uint64_t off = p - g_m.base;
    g_m.tensors_off = g_m.base + (off + (ALIGNMENT - (off % ALIGNMENT)) % ALIGNMENT);

    g_m.vocab_size = kvrd32(VOCAB_SIZE_KEY);
    g_m.n_layers = kvrd32(BLOCK_COUNT_KEY);
    g_m.n_embed = kvrd32(EMBED_LEN_KEY);
    g_m.n_heads = kvrd32(HEAD_COUNT_KEY);
    g_m.n_kv_heads = kvrd32(HEAD_KV_COUNT_KEY);
    g_m.n_context = kvrd32(CONTEXT_LEN_KEY);
    g_m.tok_bos = kvrd32(TOKENS_BOS_KEY);
    g_m.tok_eos = kvrd32(TOKENS_EOS_KEY);
    g_m.rms_epsilon = kvrdf32(RMS_EPSILON_KEY);
    g_m.rope_base = kvrdf32(ROPE_BASE_KEY);
    g_m.rope_dim = kvrd32(ROPE_DIMS_KEY);
    g_m.n_ff = kvrd32(FF_LEN_KEY);

    g_m.head_dim = g_m.n_embed / g_m.n_heads;
    assert(g_m.head_dim * g_m.n_heads == g_m.n_embed);

#if 1
    g_m.n_context = FORCE_CONTEXT;
    printf("#Nheads %d\n",g_m.n_heads);
    printf("#Nkv_heads %d\n",g_m.n_kv_heads);
    printf("#NLayers %d\n",g_m.n_layers);
    printf("#NContext %d\n",g_m.n_context);
    printf("#NEmbed %d\n",g_m.n_embed);
    printf("#NFeedForward %d\n",g_m.n_ff);
    printf("#Frope %f\n",g_m.rope_base);
    printf("#RopeDim %d\n",g_m.rope_dim);
    printf("#RMSEpsilon %f\n",g_m.rms_epsilon);
    printf("#VocabSize %d\n",g_m.vocab_size);
    printf("#BOS %d\n",g_m.tok_bos);
    printf("#EOS %d\n",g_m.tok_eos);
#endif
}

void puttensor_q(uint64_t off, uint64_t len, FILE* f)
{
    uint64_t nb = len / QK8_0;
    assert(len % QK8_0 == 0);

    block_q8_0* ptr = (block_q8_0*)(g_m.tensors_off+off);
    for (uint64_t i = 0; i < nb; i++) {
        block_q8_0 b;
        memcpy(&b,ptr,sizeof(b));
        block_q8_f bf;
        memcpy(&bf.qs,&b.qs,sizeof(b.qs));
        bf.d = fp16_to_fp32(b.d);

        fwrite(&bf,sizeof(block_q8_f),1,f);
        ptr++;
    }
}

void puttensor(string name, int n, FILE* f)
{
    char str[MAXNAMELEN] = {0};
    snprintf(str,sizeof(str),name.c_str(),n);
    assert(g_m.tensors.count(str));

    gguf_tensor tz = g_m.tensors[str];
    uint64_t sz = 1;
    for (auto &i : tz.dims) sz *= i;
    DBG("Tensor %s size %lu",str,sz);

    if (tz.type == Q8_0) puttensor_q(tz.off,sz,f);
    else if (tz.type == F32) fwrite(g_m.tensors_off+tz.off,sizeof(float),sz,f);
    else abort();
}

void read_tokenizer()
{
    assert(g_m.meta_kv.count(TOKENS_KEY));
    uint8_t* pos = g_m.base + g_m.meta_kv[TOKENS_KEY].off;

    gguf_val_type nt = (gguf_val_type)rd32(&pos);
    assert(nt == GSTRING);

    uint64_t ne = rd64(&pos);
    assert((int)ne == g_m.vocab_size);
    g_m.tokens.resize(ne);

    for (uint64_t i = 0; i < ne; i++) {
        g_m.tokens[i] = rdstr(&pos);
        g_m.tokens_rev[g_m.tokens[i]] = i;
    }

    assert(g_m.meta_kv.count(TOKENS_SCORE_KEY));
    pos = g_m.base + g_m.meta_kv[TOKENS_SCORE_KEY].off;
    nt = (gguf_val_type)rd32(&pos);
    assert(nt == GFLOAT32);

    ne = rd64(&pos);
    g_m.tokscores.resize(ne);
    for (uint64_t i = 0; i < ne; i++)
        g_m.tokscores[i] = rdf32(&pos);
}

void make_blob(const char* fn)
{
    FILE* fo = fopen(fn,"wb");
    //DBG("Vocab size = %d",g_m.vocab_size);
    uint64_t base = 0x80000000 + g_m.vocab_size * 8;
    //DBG("Base = %lu",base);
    //DBG("g_m.tokens.size() = %zu",g_m.tokens.size());
    for (auto &i : g_m.tokens) {
        fwrite(&base,8,1,fo);
        base += i.length() + 1;
    }
    for (auto &i : g_m.tokens)
        fwrite(i.c_str(),1,i.length()+1,fo);

    printf("#TokensTabBytes %zu\n",ftell(fo));

    for (auto &i : g_m.tokscores)
        fwrite(&i,4,1,fo);

    puttensor(TOKENS_EMBED_KEY,0,fo);
    DBG("Layers begin at %zu",ftell(fo));

    for (int l = 0; l < g_m.n_layers; l++) {
        puttensor(LAYER_ATT_NORM_KEY,l,fo);
        puttensor(LAYER_ATT_Q_KEY,l,fo);
        puttensor(LAYER_ATT_K_KEY,l,fo);
        puttensor(LAYER_ATT_V_KEY,l,fo);
        puttensor(LAYER_ATT_OUT_KEY,l,fo);

        puttensor(LAYER_FFN_NORM_KEY,l,fo);
        puttensor(LAYER_FFN_UP_KEY,l,fo);
        puttensor(LAYER_FFN_GATE_KEY,l,fo);
        puttensor(LAYER_FFN_DOWN_KEY,l,fo);

        DBG("Layer %d ends at %zu",l,ftell(fo));
    }

    puttensor(OUTPUT_NORM_KEY,0,fo);

    DBG("Output begins at %zu",ftell(fo));
    puttensor(OUTPUT_KEY,0,fo);

    DBG("Done making The Blob '%s': %zu bytes",fn,ftell(fo));
    fclose(fo);
}

int main(int argc, char* argv[])
{
    assert(argc > 1);
    open_mmap(argv[1]);
    read_gguf();
    read_tokenizer();

    if (argc > 2) make_blob(argv[2]);

    return 0;
}
