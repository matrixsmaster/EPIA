;llama2-compatible inference engine for Syntheva EPIA NPUs
;(C) Dmitry 'sciloaf' Solovyev aka MatrixS_Master, 2005
;License: MIT
; Enjoy at your own risk. You've been warned!

;***GENERATED TABLE for Mistral 0.3 7B
#Nheads 32
#Nkv_heads 8
#NLayers 32
#NContext 1024
#NEmbed 4096
#NFeedForward 14336
#Frope 1000000.000000
#RopeDim 128
#RMSEpsilon 0.000010
#VocabSize 32768
#BOS 1
#EOS 2
#TokensTabBytes 509470
#BlobStart 0x80000000
;***

#QK8 32
#QK8FULL !(QK8+4)
#QK8_FLT !(QK8*4)

#MaxTokens 256
#MaxInput 256
#MaxTokAccum 128
#StackSize !(256*8)

#TokensTab !(BlobStart)
#TokenArrSize !(MaxTokens*4)
#TokensScores !(TokensTab+TokensTabBytes)

#EmbedBegin !(BlobStart+TokensTabBytes+(VocabSize*4))
#LayersBegin !(EmbedBegin+((VocabSize*NEmbed)/QK8)*QK8FULL)

#KVdim !((NEmbed*Nkv_heads)/Nheads)
#HeadDim !(NEmbed/Nheads)
#KVCacheLayer !(NContext*KVdim)
#KVNrep !(Nheads/Nkv_heads)
#SqrHeadDim !(%(&HeadDim))

#AttNormLen !(NEmbed*4) ; float
#AttQLen !(((NEmbed*NEmbed)/QK8)*QK8FULL) ; quantized
#AttKVLen !(((NEmbed*KVdim)/QK8)*QK8FULL) ; quantized
#AttOutLen !(((NEmbed*NEmbed)/QK8)*QK8FULL) ; quantized

#FFNormLen !(NEmbed*4) ; float
#FFUpLen !(((NEmbed*NFeedForward)/QK8)*QK8FULL) ; quantized
#FFGateLen !(((NEmbed*NFeedForward)/QK8)*QK8FULL) ; quantized
#FFDownLen !(((NFeedForward*NEmbed)/QK8)*QK8FULL) ; quantized, transposed

#OutNormLen !(NEmbed*4) ; float

#TEmbedSize !(NEmbed*4)
#TQFFNSize !(NFeedForward*QK8FULL)
#TFFNSize !(NFeedForward*4)
#TKVSize !(KVdim*4)
#TKVCacheSize !(NLayers*KVCacheLayer*4)
#TAttSize !(Nheads*NContext*4)
#TLogitsSize !(VocabSize*4)

#RopeDimFlt !(&RopeDim)

; Symbolic names for SDPU registers
#DPU_AVSTART 0
#DPU_AVEND 1
#DPU_ASSTART 2
#DPU_BVSTART 3
#DPU_BSSTART 4
#DPU_VSEQLEN 5
#DPU_VPAD 6
#DPU_SSEQLEN 7
#DPU_SPAD 8

.0

jmp main

dd SP stack_begin

:memcpy
>memcpy
    mov ret @SP
    add SP SP %-8
    mov len @SP
    add SP SP %-8
    mov src @SP
    add SP SP %-8
    mov dst @SP

    :loop
        cmp len %0
        jif endloop %1

        movb @dst @src
        suu len len %1
        add src src %1
        add dst dst %1
        jmp loop
    :endloop

    jmp @ret

dd ret 0
dd src 0
dd dst 0
dd len 0
<

:memzero
>memzero
    mov ret @SP
    add SP SP %-8
    mov len @SP
    add SP SP %-8
    mov ptr @SP

    mov i %0
    :loop
        cmp i len
        jif endloop %3

        movb @ptr %0
        add i i %1
        add ptr ptr %1
        jmp loop
    :endloop
    jmp @ret

dd ret 0
dd ptr 0
dd len 0
dd i 0
<

:strcmp
>strcmp
    mov ret @SP
    add SP SP %-8
    mov b @SP
    add SP SP %-8
    mov a @SP

    :loop
        cmpb @a %0
        jif endloop %1
        cmpb @b %0
        jif endloop %1

        cmpb @a @b
        jif equ %1

        mov @SP %1
        jmp @ret

        :equ
        add a a %1
        add b b %1
        jmp loop
    :endloop

    mov @SP %0
    iorb @SP @a @b
    jmp @ret

dd ret 0
dd a 0
dd b 0
<

:strcat
>strcat
    mov ret @SP
    add SP SP %-8
    mov b @SP
    add SP SP %-8
    mov a @SP

    :find
        cmpb @a %0
        jif endfind %1
        add a a %1
        jmp find
    :endfind

    :loop
        cmpb @b %0
        jif endloop %1

        movb @a @b
        add a a %1
        add b b %1
        jmp loop
    :endloop

    movb @a %0
    jmp @ret

dd ret 0
dd a 0
dd b 0
<

;void quantize_q80(qtensor y, ftensor x, int xsize)
:quantize
>quantize
    mov ret @SP
    add SP SP %-4
    movw xsize @SP
    add SP SP %-8
    mov x @SP
    add SP SP %-8
    mov y @SP

    ;const int nb = xsize / QK8_0;
    divw nb xsize %QK8

    ;for (int i = 0; i < nb; i++) {
    ppuw i nb %endloop
        clo j
        clo amax
        clo xi
        clo yi
        clo d
        clo id
        clo v

        ;float amax = 0.0f; // absolute max
        movw amax %0.0

        ;for (int j = 0; j < QK8_0; j++) {
        mov j %0
        :loop2
            cmpb j %QK8
            jif endloop2 %3

            ;const float v = x[i*QK8_0 + j];
            mul xi i %QK8
            add xi xi j
            mul xi xi %4
            add xi x xi
            movw v @xi

            ;amax = MAX(amax,fabsf(v));
            abf v v
            cmf amax v
            jif nobig %3
            movw amax v
            :nobig

            add j j %1
            jmp loop2
        :endloop2

        ;const float d = amax / ((1 << 7) - 1);
        dif d amax %127.0
        ;const float id = d ? 1.0f/d : 0.0f;
        jif divzero %0x80
        ivf id d
        jmp divok
        :divzero
        movw id %0.0
        :divok

        ;y[i].d = d;
        mul yi i %QK8FULL
        add yi y yi
        movw @yi d
        add yi yi %4

        ;for (int j = 0; j < QK8_0; j++)
        mov j %0
        :loop3
            cmpb j %QK8
            jif endloop3 %3

            ;y[i].qs[j] = roundf(x[i * QK8_0 + j] * id);
            mul xi i %QK8
            add xi xi j
            mul xi xi %4
            add xi x xi

            muf v @xi id
            rnf v v
            ftib @yi v
            add yi yi %1

            add j j %1
            jmp loop3
        :endloop3

        ;add i i %1
        ;jmp loop
    :endloop

    jmp @ret

dd ret 0
dd y 0
dd x 0
dw xsize 0
dw nb 0
dd i 0
dd j 0
dw amax 0
dd xi 0
dd yi 0
dw d 0
dw id 0
dw v 0
<

;void dequant_q80(ftensor y, block_q8_0* ptr, int nrow, int len)
:dequant
>dequant
    mov ret @SP
    add SP SP %-4
    movw len @SP
    add SP SP %-4
    movw nrow @SP
    add SP SP %-8
    mov ptr @SP
    add SP SP %-8
    mov y @SP

    ;qtensor x = ptr + nrow * (len / QK8_0);
    mov x %0
    divw x len %QK8
    mulw x x nrow
    mul x x %QK8FULL
    add x x ptr

    ;const int nb = len / QK8_0;
    divw nb len %QK8

    ;for (int i = 0; i < nb; i++) {
    mov i %0
    :loop
        cmpw i nb
        jif endloop %3

        ;const float d = x[i].d;
        mul xi i %QK8FULL
        add xi x xi
        movw d @xi
        add xi xi %4

        mul yi i %QK8_FLT
        add yi y yi

        ;for (int j = 0; j < QK8_0; j++)
        mov j %0
        :loop2
            cmpb j %QK8
            jif endloop2 %3

            ;y[i*QK8_0 + j] = x[i].qs[j]*d;
            itfb xv @xi
            muf @yi xv d
            add yi yi %4
            add xi xi %1

            add j j %1
            jmp loop2
        :endloop2

        add i i %1
        jmp loop
    :endloop

    jmp @ret

dd ret 0
dd ptr 0
dw nrow 0
dd y 0
dd x 0
dw len 0
dw nb 0
dd i 0
dd j 0
dd xi 0
dd yi 0
dw d 0
dw xv 0
<

;void rmsnorm(ftensor out, ftensor x, ftensor w, int size)
:rmsnorm
>rms
    mov ret @SP
    add SP SP %-4
    movw size @SP
    add SP SP %-8
    mov w @SP
    add SP SP %-8
    mov x @SP
    add SP SP %-8
    mov out @SP

    movw ss %0.0
    mov i %0
    :loop1
        cmpw i size
        jif endloop1 %3

        mul ix i %4
        add ix x ix
        movw F @ix
        muf F F F
        adf ss ss F

        add i i %1
        jmp loop1
    :endloop1

    itfw F size
    dif F ss F
    adf F F %RMSEpsilon

    sqr F F
    ivf ss F

    mov i %0
    mov ix x
    mov iw w
    mov oi out
    :loop2
        cmpw i size
        jif endloop2 %3

        ;out[i] = x[i] * ss * w[i];
        ;muf F ss @iw
        ;muf @oi F @ix
        muf F @ix ss
        muf @oi F @iw

        add ix ix %4
        add iw iw %4
        add oi oi %4
        add i i %1
        jmp loop2
    :endloop2

    jmp @ret

dd ret 0
dw size 0
dd w 0
dd x 0
dd out 0
dw ss 0
dd i 0
dd ix 0
dd iw 0
dd oi 0
dw F 0
<

;void rope(ftensor x, int n_heads, int pos)
:rope
>rope
    mov ret @SP
    add SP SP %-4
    movw pos @SP
    add SP SP %-4
    movw nheads @SP
    add SP SP %-8
    mov x @SP

    ;for (int h = 0; h < n_heads; h++) { // for each head
    mov h %0
    :loop
        cmpw h nheads
        jif endloop %3

        ;float* v = x + h * RopeDim;
        mul v h %RopeDim
        mul v v %4
        add v x v

        ;for (int i = 0; i < rd; i += 2) {
        mov i %0
        :loop2
            cmpw i %RopeDim
            jif endloop2 %3

            ;const int m = i >> 1;
            itfw m i
            ;float ang = pos * powf(1000000.0, -2.0f * (float)m / (float)rd);
            ngf m m
            dif m m %RopeDimFlt
            pow m %Frope m
            itfw ang pos
            muf ang ang m

            ;float c = cosf(ang), s = sinf(ang);
            cos c ang
            sin s ang

            ;float x0 = v[i + 0];
            mul vi i %4
            add vi v vi
            movw x0 @vi

            ;float x1 = v[i + 1];
            add vi vi %4
            movw x1 @vi

            ;v[i + 0] = x0 * c - x1 * s;
            mul vi i %4
            add vi v vi
            muf v0 x0 c
            muf v1 x1 s
            suf @vi v0 v1

            ;v[i + 1] = x0 * s + x1 * c;
            add vi vi %4
            muf v0 x0 s
            muf v1 x1 c
            adf @vi v0 v1

            add i i %2
            jmp loop2
        :endloop2

        addw h h %1
        jmp loop
    :endloop

    jmp @ret

dd ret 0
dw pos 0
dw nheads 0
dd x 0
dd v 0
dd h 0
dd i 0
dw m 0
dw c 0
dw s 0
dw ang 0
dw x0 0
dw x1 0
dd vi 0
dw v0 0
dw v1 0
<

;void matmul(ftensor out, qtensor qx, qtensor qw, int n, int d)
:matmul
>matmul
    mov ret @SP
    add SP SP %-4
    movw d @SP
    add SP SP %-4
    movw n @SP
    add SP SP %-8
    mov qw @SP
    add SP SP %-8
    mov qx @SP
    add SP SP %-8
    mov out @SP

    ;const int nb = n / QK8_0;
    mov nb %0
    divw nb n %QK8

    ;for (int r = 0; r < d; r++) { // each row
    mov r %0

    ppuw r d %endloop1
        clo iw
        clo ex
        clo acc

        dpw %DPU_ASSTART qx
        add ex qx %4
        dpw %DPU_AVSTART ex

        mov iw %0
        movw iw nb
        mul iw iw r
        mul iw iw %QK8FULL
        add iw iw qw
        dpw %DPU_BSSTART iw
        add iw iw %4
        dpw %DPU_BVSTART iw

        mov ex %0
        divw ex n %QK8
        mul ex ex %QK8FULL
        add ex ex qx
        dpw %DPU_AVEND ex

        dpw %DPU_VSEQLEN %QK8
        dpw %DPU_SSEQLEN %1
        dpw %DPU_VPAD %4
        dpw %DPU_SPAD %QK8

        dpu acc

        mul iw r %4
        add iw iw out
        movw @iw acc
    :endloop1

    jmp @ret

dd ret 0
dd out 0
dd qx 0
dd qw 0
dw n 0
dw d 0
dw nb 0
dd r 0

dd iw 0
dd ex 0
dw acc 0
<

;tokenize(int* out, const char* str, int bos, int eos)
:tokenize
>tokenize
    mov ret @SP
    add SP SP %-4
    movw eos @SP
    add SP SP %-4
    movw bos @SP
    add SP SP %-8
    mov str @SP
    add SP SP %-8
    mov out @SP

    ;memset(out,0,olen);
    mov @SP out
    add SP SP %8
    mov @SP %TokenArrSize
    add SP SP %8
    mov @SP %ret1
    jmp memzero
    :ret1

    ;int* pout = out;
    mov pout out

    ;if (bos) *pout++ = g_m.tok_bos;
    cmpw bos %0
    jif skipbos %1
    movw @pout %BOS
    add pout pout %4
    :skipbos

    ;char s[8] = {0};
    mov @SP %s
    add SP SP %8
    mov @SP %8
    add SP SP %8
    mov @SP %ret2
    jmp memzero
    :ret2

    ;while (*str || *s) {
    :loop1
        iorb tmp @str s
        jif endloop1 %0x80

        ;if (!*s) *s = *str++;
        cmpb s %0
        jif skipnext %2
        movb s @str
        add str str %1
        :skipnext

        ;for (int i = 0; i < g_m.vocab_size; i++) {
        mov i %0
        :loop2
            cmp i %VocabSize
            jif endloop2 %3

            ;if (!strcmp(g_m.tokens[i],s)) {
            mul tmp i %8
            add tmp tmp %TokensTab
            mov tmp @tmp

            mov @SP tmp
            add SP SP %8
            mov @SP %s
            add SP SP %8
            mov @SP %ret3
            jmp strcmp
            :ret3

            cmpb @SP %0
            jif notok1 %2
                ;*pout++ = i;
                movw @pout i
                add pout pout %4

                ;memset(s,0,sizeof(s));
                mov @SP %s
                add SP SP %8
                mov @SP %8
                add SP SP %8
                mov @SP %ret4
                jmp memzero
                :ret4

                ;break;
                jmp endloop2
            :notok1
            add i i %1
            jmp loop2
        :endloop2

        ;if (*s) {
        cmp s %0
        jif tokok %1

            ;assert(strlen(s) == 1);
            ;snprintf(s,sizeof(s),"<0x%02X>",*s);
            movb b s
            mov tmp %s
            movb @tmp %'<'
            add tmp tmp %1
            movb @tmp %'0'
            add tmp tmp %1
            movb @tmp %'x'
            add tmp tmp %1

            mov sym %0
            shrb sym b %4
            add hex %hexcodes sym
            movb @tmp @hex
            add tmp tmp %1

            andb sym b %0x0F
            add hex %hexcodes sym
            movb @tmp @hex
            add tmp tmp %1

            movb @tmp %'>'
            add tmp tmp %1
            movb @tmp %0
        :tokok
        jmp loop1
    :endloop1

    ;while (1) {
    :loop3
        ;unsigned plen = pout - out + 1;
        suu plen pout out
        div plen plen %4

        ;float best_score = -1e10;
        movw best_score %-1e10
        ;unsigned best_id = -1;
        movw best_id %-1
        ;unsigned best_idx = plen + 1;
        add best_idx plen %1

        ;char acc[MAXNAMELEN] = {0};
        mov @SP %acc
        add SP SP %8
        mov @SP %MaxTokAccum
        add SP SP %8
        mov @SP %retB1
        jmp memzero
        :retB1

        ;for (unsigned i = 0; i < plen-1; i++) {
        mov i %0
        :loop4
            suu tmp plen %1
            cmp i tmp
            jif endloop4 %3

            ;sprintf(acc,"%s%s",g_m.tokens[out[i]],g_m.tokens[out[i+1]]);
            mul toki i %4
            add toki out toki
            movw toki @toki
            mul toki toki %8
            add toki toki %TokensTab
            mov toki @toki

            movb acc %0     ; reset accum
            mov @SP %acc    ; put the first word
            add SP SP %8
            mov @SP toki
            add SP SP %8
            mov @SP %retC1
            jmp strcat
            :retC1

            add toki i %1
            mul toki toki %4
            add toki out toki
            movw toki @toki
            mul toki toki %8
            add toki toki %TokensTab
            mov toki @toki

            mov @SP %acc    ; put the second word
            add SP SP %8
            mov @SP toki
            add SP SP %8
            mov @SP %retC2
            jmp strcat
            :retC2

            ;int fnd = -1;
            movw fnd %-1

            ;for (int j = 0; j < g_m.vocab_size; j++) {
            mov j %0
            :loop5
                cmp j %VocabSize
                jif endloop5 %3

                ;if (!strcmp(g_m.tokens[j],acc)) {
                mul tmp j %8
                add tmp tmp %TokensTab
                mov tmp @tmp

                mov @SP tmp
                add SP SP %8
                mov @SP %acc
                add SP SP %8
                mov @SP %retC3
                jmp strcmp
                :retC3

                cmpb @SP %0
                jif notok2 %2
                    ;fnd = j;
                    movw fnd j
                    ;break;
                    jmp endloop5
                :notok2

                add j j %1
                jmp loop5
            :endloop5

            ;if (fnd < 0) continue;
            cmsw fnd %0
            jif nofnd %4

            ;float sc = g_m.tokscores[fnd];
            mov toki %0
            movw toki fnd
            mul toki toki %4
            add toki toki %TokensScores
            movw sc @toki

            ;if (sc > best_score) {
            cmf sc best_score
            jif nofnd %5
                ;best_score = sc;
                movw best_score sc
                ;best_id = fnd;
                movw best_id fnd
                ;best_idx = i;
                mov best_idx i
            :nofnd

            add i i %1
            jmp loop4
        :endloop4

        ;if (best_idx > plen) break;
        cmp best_idx plen
        jif endloop3 %2

        ;out[best_idx] = best_id;
        mul toki best_idx %4
        add toki out toki
        movw @toki best_id

        ;for (unsigned j = best_idx+1; j < plen-1; j++) out[j] = out[j+1];
        add j best_idx %1
        :loop6
            suu tmp plen %1
            cmp j tmp
            jif endloop6 %3

            add toki j %1
            mul toki toki %4
            add toki out toki
            movw tmp @toki

            mul toki j %4
            add toki out toki
            movw @toki tmp

            add j j %1
            jmp loop6
        :endloop6

        ;*--pout = 0;
        suu pout pout %4
        movw @pout %0
        jmp loop3
    :endloop3

    ;if (eos) *pout = g_m.tok_eos;
    cmpw eos %0
    jif skipeos %1
    movw @pout %EOS
    ;add pout pout %4
    :skipeos

    jmp @ret

dd ret 0
dw eos 0
dw bos 0
dd str 0
dd out 0
dd pout 0
db s "1234567\0"
dd i 0
dd j 0
db b 0
dd tmp 0
dd sym 0
dd hex 0
dd plen 0
dw best_id 0
dd best_idx 0
dw best_score 0
dd toki 0
dw fnd 0
dw sc 0
db acc 0
.+MaxTokAccum
db 0
<

;void attention(ftensor out, ftensor q, ftensor k, ftensor v, int l, int pos)
:attention
>attention
    ; get arguments
    mov ret @SP
    add SP SP %-4
    movw pos @SP
    add SP SP %-4
    movw l @SP
    add SP SP %-8
    mov v @SP
    add SP SP %-8
    mov k @SP
    add SP SP %-8
    mov q @SP
    add SP SP %-8
    mov out @SP

    ;uint64_t loff = l * g_m.n_context * g_m.kv_dim; // kv cache layer offset
    mov loff %0
    movw loff l
    mul loff loff %KVCacheLayer
    ;float* kc_row = g_m.kc + loff + pos * g_m.kv_dim;
    mov lpos %0
    movw lpos pos
    mul tmp lpos %KVdim
    add tmp tmp loff
    mul tmp tmp %4
    add kc_row tmp %t_kc
    ;float* vc_row = g_m.vc + loff + pos * g_m.kv_dim;
    add vc_row tmp %t_vc

    ;memcpy(kc_row, k, g_m.kv_dim*sizeof(float));
    mov @SP kc_row
    add SP SP %8
    mov @SP k
    add SP SP %8
    mov @SP %TKVSize
    add SP SP %8
    mov @SP %retM1
    jmp memcpy
    :retM1

    ;memcpy(vc_row, v, g_m.kv_dim*sizeof(float));
    mov @SP vc_row
    add SP SP %8
    mov @SP v
    add SP SP %8
    mov @SP %TKVSize
    add SP SP %8
    mov @SP %retM2
    jmp memcpy
    :retM2

    ;const int nrep = g_m.n_heads / g_m.n_kv_heads; // GQA, heads per KV head
    ;%KVNrep

    ;for (int h = 0; h < g_m.n_heads; h++) {
    mov h %0
    :loop1
        cmpw h %Nheads
        jif endloop1 %3

        ;float* attv = g_m.att + (h * g_m.n_context); // attention scores for this head
        mul attv h %NContext
        mul attv attv %4
        add attv attv %t_att
        ;float* qv = q + (h * g_m.head_dim); // start of the Q vector
        mul qv h %HeadDim
        mul qv qv %4
        add qv qv q

        ;precalc (h / nrep) * g_m.head_dim; <- hoff
        div hoff h %KVNrep
        mul hoff hoff %HeadDim

        ;for (int t = 0; t <= pos; t++) {
        mov t %0
        :loop2
            cmpw t pos
            jif endloop2 %2

            ;kc_row = g_m.kc + loff + t * g_m.kv_dim + (h / nrep) * g_m.head_dim;
            mul kc_row t %KVdim
            add kc_row kc_row hoff
            add kc_row kc_row loff
            mul kc_row kc_row %4
            add kc_row kc_row %t_kc

            ;float score = 0.0f;
            movw score %0.0

            ;for (int i = 0; i < g_m.head_dim; i++)
            mov i %0
            mov qvi qv
            mov kci kc_row
            :loop3
                cmpw i %HeadDim
                jif endloop3 %3

                ;score += qv[i] * kc_row[i];
                muf ftmp @qvi @kci
                adf score score ftmp

                add i i %1
                add qvi qvi %4
                add kci kci %4
                jmp loop3
            :endloop3

            ;score /= sqrtf(g_m.head_dim);
            dif score score %SqrHeadDim

            ;attv[t] = score;
            mul tmp t %4
            add tmp tmp attv
            movw @tmp score

            add t t %1
            jmp loop2
        :endloop2

        ;softmax(attv,pos+1); <- inlined
        addw size pos %1
        ;float max_val = x[0];
        movw max_val @attv

        ;for (int i = 1; i < size; i++) {
        mov i %1
        add tmp attv %4 ; i = 1
        :loop4
            cmpw i size
            jif endloop4 %3

            ;if (x[i] > max_val) max_val = x[i];
            cmf @tmp max_val
            jif nomore %5
            movw max_val @tmp
            :nomore

            add tmp tmp %4
            add i i %1
            jmp loop4
        :endloop4

        ;float sum = 0.0f;
        movw sum %0.0
        ;for (int i = 0; i < size; i++) {
        mov i %0
        mov tmp attv
        :loop5
            cmpw i size
            jif endloop5 %3

            ;x[i] = expf(x[i] - max_val);
            suf ftmp @tmp max_val
            exp @tmp ftmp

            ;sum += x[i];
            adf sum sum @tmp

            add tmp tmp %4
            add i i %1
            jmp loop5
        :endloop5

        ;protection
        cmf sum %0.0
        jif endloop6 %1
        ;optimization
        cmf sum %1.0
        jif endloop6 %1

        ;for (int i = 0; i < size; i++) x[i] /= sum;
        mov i %0
        mov tmp attv
        :loop6
            cmpw i size
            jif endloop6 %3

            dif @tmp @tmp sum

            add tmp tmp %4
            add i i %1
            jmp loop6
        :endloop6
        ; --- end of softmax

        ;float* outv = out + (h * g_m.head_dim);
        mul outv h %HeadDim
        mul outv outv %4
        add outv outv out

        ;memset(outv,0,g_m.head_dim * sizeof(float)); <-- we don't want to use memzero inside this loop (for PPU-ing it later)
        mov i %0
        mov tmp outv
        :loop7
            cmpw i %HeadDim
            jif endloop7 %3

            movw @tmp %0

            add tmp tmp %4
            add i i %1
            jmp loop7
        :endloop7

        ;for (int t = 0; t <= pos; t++) {
        mov t %0
        mov awt attv
        :loop8
            cmpw t pos
            jif endloop8 %2

            ;vc_row = g_m.vc + loff + t * g_m.kv_dim + (h / nrep) * g_m.head_dim;
            mul vc_row t %KVdim
            add vc_row vc_row hoff
            add vc_row vc_row loff
            mul vc_row vc_row %4
            add vc_row vc_row %t_vc

            ;float a = attv[t]; // attention weight for this timestep
            movw a @awt

            ;for (int i = 0; i < g_m.head_dim; i++)
            mov i %0
            mov tmp outv
            mov vci vc_row
            :loop9
                cmpw i %HeadDim
                jif endloop9 %3

                ;outv[i] += a * vc_row[i];
                muf ftmp a @vci
                adf @tmp @tmp ftmp

                add tmp tmp %4
                add vci vci %4
                add i i %1
                jmp loop9
            :endloop9

            add awt awt %4
            add t t %1
            jmp loop8
        :endloop8

        add h h %1
        jmp loop1
    :endloop1

    jmp @ret

dd ret 0
dd out 0
dd q 0
dd k 0
dd v 0
dw l 0
dw pos 0

dd attv 0
dd qv 0
dd loff 0
dd lpos 0
dd kc_row 0
dd vc_row 0
dd tmp 0
dd h 0
dd t 0
dd i 0
dd hoff 0
dw score 0
dd qvi 0
dd kci 0
dw size 0
dw max_val 0
dw sum 0
dw ftmp 0
dd outv 0
dw a 0
dd awt 0
dd vci 0
<

;void inference(ftensor logs, int tok, int pos)
:inference
>inference
    ; get arguments
    mov ret @SP
    add SP SP %-4
    movw pos @SP
    add SP SP %-4
    movw tok @SP
    add SP SP %-8
    mov logs @SP

    ;dequant_q80(g_m.x,g_m.t_embed,tok,g_m.n_embed);
    mov @SP %t_x        ; y (dest vector)
    add SP SP %8
    mov @SP %EmbedBegin ; x (quantized)
    add SP SP %8
    movw @SP tok        ; tok (row)
    add SP SP %4
    movw @SP %NEmbed    ; size
    add SP SP %4
    mov @SP %retA1
    jmp dequant
    :retA1

    mov CTensor %LayersBegin

    ;for (int l = 0; l < g_m.n_layers; l++) {
    mov l %0
    :layers
        cmp l %NLayers
        jif endlayers %3

        ;rmsnorm(g_m.xb, g_m.x, g_m.tr[l].att_norm, g_m.n_embed);
        mov @SP %t_xb       ; out (dest vector)
        add SP SP %8
        mov @SP %t_x        ; x (input)
        add SP SP %8
        mov @SP CTensor     ; weights
        add SP SP %8
        movw @SP %NEmbed    ; size
        add SP SP %4
        mov @SP %retB1
        jmp rmsnorm
        :retB1

        ;quantize_q80(g_m.xq, g_m.xb, g_m.n_embed);
        mov @SP %t_xq       ; y (dest vector)
        add SP SP %8
        mov @SP %t_xb       ; x (float)
        add SP SP %8
        movw @SP %NEmbed    ; size
        add SP SP %4
        mov @SP %retB2
        jmp quantize
        :retB2

        add CTensor CTensor %AttNormLen

        ;matmul(g_m.q, g_m.xq, g_m.tr[l].att_q, g_m.n_embed, g_m.n_embed);
        mov @SP %t_q        ; out (dest tensor)
        add SP SP %8
        mov @SP %t_xq       ; x (quantized)
        add SP SP %8
        mov @SP CTensor     ; w (quantized)
        add SP SP %8
        movw @SP %NEmbed    ; n
        add SP SP %4
        movw @SP %NEmbed    ; d
        add SP SP %4
        mov @SP %retB3
        jmp matmul
        :retB3

        add CTensor CTensor %AttQLen

        ;matmul(g_m.k, g_m.xq, g_m.tr[l].att_k, g_m.n_embed, g_m.kv_dim);
        mov @SP %t_k        ; out (dest tensor)
        add SP SP %8
        mov @SP %t_xq       ; x (quantized)
        add SP SP %8
        mov @SP CTensor     ; w (quantized)
        add SP SP %8
        movw @SP %NEmbed    ; n
        add SP SP %4
        movw @SP %KVdim     ; d
        add SP SP %4
        mov @SP %retB4
        jmp matmul
        :retB4

        add CTensor CTensor %AttKVLen

        ;matmul(g_m.v, g_m.xq, g_m.tr[l].att_v, g_m.n_embed, g_m.kv_dim);
        mov @SP %t_v        ; out (dest tensor)
        add SP SP %8
        mov @SP %t_xq       ; x (quantized)
        add SP SP %8
        mov @SP CTensor     ; w (quantized)
        add SP SP %8
        movw @SP %NEmbed    ; n
        add SP SP %4
        movw @SP %KVdim     ; d
        add SP SP %4
        mov @SP %retB5
        jmp matmul
        :retB5

        add CTensor CTensor %AttKVLen

        ;rope(g_m.q, g_m.n_heads, pos);
        mov @SP %t_q      ; x (in-place)
        add SP SP %8
        movw @SP %Nheads  ; n_heads
        add SP SP %4
        movw @SP pos      ; pos
        add SP SP %4
        mov @SP %retB6
        jmp rope
        :retB6

        ;rope(g_m.k, g_m.n_kv_heads, pos);
        mov @SP %t_k      ; x (in-place)
        add SP SP %8
        movw @SP %Nkv_heads ; n_heads
        add SP SP %4
        movw @SP pos      ; pos
        add SP SP %4
        mov @SP %retB7
        jmp rope
        :retB7

        ;attention(g_m.xb, g_m.q, g_m.k, g_m.v, l, pos);
        mov @SP %t_xb   ; out
        add SP SP %8
        mov @SP %t_q    ; q
        add SP SP %8
        mov @SP %t_k    ; k
        add SP SP %8
        mov @SP %t_v    ; v
        add SP SP %8
        movw @SP l      ; layer
        add SP SP %4
        movw @SP pos    ; pos
        add SP SP %4
        mov @SP %retC1
        jmp attention
        :retC1

        ;quantize_q80(g_m.xq, g_m.xb, g_m.n_embed);
        mov @SP %t_xq       ; y (dest vector)
        add SP SP %8
        mov @SP %t_xb       ; x (float)
        add SP SP %8
        movw @SP %NEmbed    ; size
        add SP SP %4
        mov @SP %retC2
        jmp quantize
        :retC2

        ;matmul(g_m.xb2, g_m.xq, g_m.tr[l].att_out, g_m.n_embed, g_m.n_embed);
        mov @SP %t_xb2      ; out (dest tensor)
        add SP SP %8
        mov @SP %t_xq       ; x (quantized)
        add SP SP %8
        mov @SP CTensor     ; w (quantized)
        add SP SP %8
        movw @SP %NEmbed    ; n
        add SP SP %4
        movw @SP %NEmbed    ; d
        add SP SP %4
        mov @SP %retC3
        jmp matmul
        :retC3

        add CTensor CTensor %AttOutLen

        ;for (int j = 0; j < g_m.n_embed; j++) g_m.x[j] += g_m.xb2[j];
        mov j %0
        mov ptr %t_x
        mov ptr2 %t_xb2
        :loopC4
            cmpw j %NEmbed
            jif endloopC4 %3

            adf @ptr @ptr @ptr2
            add ptr ptr %4
            add ptr2 ptr2 %4
            add j j %1
            jmp loopC4
        :endloopC4

        ;rmsnorm(g_m.xb, g_m.x, g_m.tr[l].ffn_norm, g_m.n_embed);
        mov @SP %t_xb       ; out (dest vector)
        add SP SP %8
        mov @SP %t_x        ; x (input)
        add SP SP %8
        mov @SP CTensor     ; weights
        add SP SP %8
        movw @SP %NEmbed    ; size
        add SP SP %4
        mov @SP %retD1
        jmp rmsnorm
        :retD1

        add CTensor CTensor %FFNormLen

        ;quantize_q80(g_m.xq,g_m.xb,g_m.n_embed);
        mov @SP %t_xq       ; y (dest vector)
        add SP SP %8
        mov @SP %t_xb       ; x (float)
        add SP SP %8
        movw @SP %NEmbed    ; size
        add SP SP %4
        mov @SP %retD2
        jmp quantize
        :retD2

        ;matmul(g_m.hb, g_m.xq, g_m.tr[l].ffn_up, g_m.n_embed, g_m.n_ff);
        mov @SP %t_hb       ; out (dest tensor)
        add SP SP %8
        mov @SP %t_xq       ; x (quantized)
        add SP SP %8
        mov @SP CTensor     ; w (quantized)
        add SP SP %8
        movw @SP %NEmbed    ; n
        add SP SP %4
        movw @SP %NFeedForward ; d
        add SP SP %4
        mov @SP %retD3
        jmp matmul
        :retD3

        add CTensor CTensor %FFUpLen

        ;matmul(g_m.hb2, g_m.xq, g_m.tr[l].ffn_gate, g_m.n_embed, g_m.n_ff);
        mov @SP %t_hb2      ; out (dest tensor)
        add SP SP %8
        mov @SP %t_xq       ; x (quantized)
        add SP SP %8
        mov @SP CTensor     ; w (quantized)
        add SP SP %8
        movw @SP %NEmbed    ; n
        add SP SP %4
        movw @SP %NFeedForward ; d
        add SP SP %4
        mov @SP %retD4
        jmp matmul
        :retD4

        add CTensor CTensor %FFGateLen

        ;for (int i = 0; i < g_m.n_ff; i++) {
        mov i %0
        mov ptr %t_hb
        mov ptr2 %t_hb2
        :swiglu
            cmpw i %NFeedForward
            jif endswiglu %3

            ;const float g = g_m.hb2[i];
            ;const float silu_g = g / (1.0f + expf(-g));
            ngf g @ptr2
            exp g g
            adf g g %1.0
            dif g @ptr2 g

            ;g_m.hb[i] = silu_g * g_m.hb[i];
            muf @ptr @ptr g

            add i i %1
            add ptr ptr %4
            add ptr2 ptr2 %4
            jmp swiglu
        :endswiglu

        ;quantize_q80(g_m.xq, g_m.hb, g_m.n_ff);
        mov @SP %t_xq       ; y (dest vector)
        add SP SP %8
        mov @SP %t_hb       ; x (float)
        add SP SP %8
        movw @SP %NFeedForward ; size
        add SP SP %4
        mov @SP %retD5
        jmp quantize
        :retD5

        ;matmul(g_m.xb, g_m.xq, g_m.tr[l].ffn_down, g_m.n_ff, g_m.n_embed);
        mov @SP %t_xb      ; out (dest tensor)
        add SP SP %8
        mov @SP %t_xq       ; x (quantized)
        add SP SP %8
        mov @SP CTensor     ; w (quantized)
        add SP SP %8
        movw @SP %NFeedForward ; n
        add SP SP %4
        movw @SP %NEmbed    ; d
        add SP SP %4
        mov @SP %retD6
        jmp matmul
        :retD6

        add CTensor CTensor %FFDownLen

        ;for (int j = 0; j < g_m.n_embed; j++) g_m.x[j] += g_m.xb[j];
        mov j %0
        mov ptr %t_x
        mov ptr2 %t_xb
        :loopD7
            cmpw j %NEmbed
            jif endloopD7 %3

            adf @ptr @ptr @ptr2
            add ptr ptr %4
            add ptr2 ptr2 %4
            add j j %1
            jmp loopD7
        :endloopD7

        add l l %1
        jmp layers
    :endlayers

    ;rmsnorm(g_m.x, g_m.x, g_m.t_outnorm, g_m.n_embed);
    mov @SP %t_x        ; out (dest vector)
    add SP SP %8
    mov @SP %t_x        ; x (input)
    add SP SP %8
    mov @SP CTensor     ; weights
    add SP SP %8
    movw @SP %NEmbed    ; size
    add SP SP %4
    mov @SP %retE1
    jmp rmsnorm
    :retE1

    add CTensor CTensor %OutNormLen

    ;quantize_q80(g_m.xq, g_m.x, g_m.n_embed);
    mov @SP %t_xq       ; y (dest vector)
    add SP SP %8
    mov @SP %t_x        ; x (float)
    add SP SP %8
    movw @SP %NEmbed    ; size
    add SP SP %4
    mov @SP %retE2
    jmp quantize
    :retE2

    ;matmul(logs, g_m.xq, g_m.t_out, g_m.n_embed, g_m.vocab_size);
    mov @SP logs        ; out (dest tensor)
    add SP SP %8
    mov @SP %t_xq       ; x (quantized)
    add SP SP %8
    mov @SP CTensor     ; w (quantized)
    add SP SP %8
    movw @SP %NEmbed    ; n
    add SP SP %4
    movw @SP %VocabSize ; d
    add SP SP %4
    mov @SP %retE3
    jmp matmul
    :retE3

    jmp @ret

dd ret 0
dd logs 0
dw tok 0
dw pos 0

dd CTensor 0
dd l 0
dd i 0
dd j 0
dd ptr 0
dd ptr2 0
dw g 0
<

:sampler
>sampler
    mov ret @SP
    add SP SP %-8
    mov logs @SP

    ;int best_id = 0;
    movw best_id %0
    ;float best_v = logits[0];
    movw best_v @logs

    mov ix logs
    add ix ix %4 ; starting from [1]
    ;for (int i = 1; i < g_m.n_embed; i++) {
    movw i %1
    :loop
        cmpw i %NEmbed
        jif endloop %3

        ;if (logits[i] > best_v) {
        cmf @ix best_v
        jif nobetter %4
            ;best_v = logits[i];
            movw best_v @ix
            ;best_id = i;
            movw best_id i
        :nobetter

        add ix ix %4
        add i i %1
        jmp loop
    :endloop

    movw @SP best_id
    jmp @ret

dd ret 0
dd logs 0

dw best_id 0
dw best_v 0
dw i 0
dd ix 0
<


:main
>main
    ;Collect user's input
    out %user_prompt %4 ;Prompt the user
    mov istr %input_str
    :scan
        inpb ichr %0
        jif endscan %0x80
        cmpb ichr nl
        jif endscan %1

        movb @istr ichr
        add istr istr %1
        jmp scan
    :endscan
    movb @istr %0

    ;Tokenize the string
    mov @SP %tokens
    add SP SP %8
    mov @SP %input_str
    add SP SP %8
    mov @SP %1
    add SP SP %4
    mov @SP %0
    add SP SP %4
    mov @SP %rettokz
    jmp tokenize
    :rettokz

    ;Feed prompt tokens into inference engine
    mov istr %tokens
    :looptokz
        cmpw @istr %0
        jif endtokz %1

        movw tok @istr

        mov @SP %logits
        add SP SP %8
        movw @SP tok
        add SP SP %4
        movw @SP pos
        add SP SP %4
        mov @SP %retA1
        jmp inference
        :retA1

        add istr istr %4
        addw pos pos %1
        jmp looptokz
    :endtokz

    ;Sample and continue
    out %ai_prompt %4 ;Show that it's AI output now
    :generate
        cmpw pos %NContext ;check we have available context left
        jif endgenerate %3

        ;Sample
        mov @SP %logits
        add SP SP %8
        mov @SP %retB1
        jmp sampler
        :retB1

        ;Get the token, advance position
        movw tok @SP
        addw pos pos %1

        ;Print the token
        mov istr %0
        movw istr tok
        mul istr istr %8
        add istr istr %TokensTab
        mov istr @istr
        out istr %4

        ;Infer the next one
        mov @SP %logits
        add SP SP %8
        movw @SP tok
        add SP SP %4
        movw @SP pos
        add SP SP %4
        mov @SP %retB2
        jmp inference
        :retB2

        jmp generate
    :endgenerate
    hlt

dw pos 0
dw tok 0
dd istr 0
db ichr 0
<

db nl "\n"
db hexcodes "0123456789ABCDEF"
db user_prompt "User: \0"
db ai_prompt "\nAI: \0"

:break

;Input string
db input_str 0
.+MaxInput

;Prompt tokens array
.?8
dw tokens 0
.+TokenArrSize

;Logits array
.?8
db logits 0
.+TLogitsSize

>inference
; Working Tensors Memory (mostly private to inference())
.?8
db t_x 0
.+TEmbedSize
.?8
db t_xb 0
.+TEmbedSize
.?8
db t_xb2 0
.+TEmbedSize
.?8
db t_xq 0
.+TQFFNSize
.?8
db t_hb 0
.+TFFNSize
.?8
db t_hb2 0
.+TFFNSize
.?8
db t_q 0
.+TEmbedSize
.?8
db t_k 0
.+TKVSize
.?8
db t_v 0
.+TKVSize
<

>attention
; Multihead Attention Working Tensors (private to attention())
.?8
db t_kc 0
.+TKVCacheSize
.?8
db t_vc 0
.+TKVCacheSize
.?8
db t_att 0
.+TAttSize
<

.?32
:stack_begin
dd 0
.+StackSize

db 1 ; canary
.break ; returns IP back to before the big tensors, to minimize the output binary size
