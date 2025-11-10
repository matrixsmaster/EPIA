; The Maze - a simple raycaster demo for Syntheva EPIA NPUs
;(C) Dmitry 'sciloaf' Solovyev aka MatrixS_Master, 2005
;License: MIT

; Viewport size - feel free to adjust to your liking
#SCREENW 180
#SCREENH 60

; Game map size and generation config
#MAPW 30
#MAPH 30
#SNAKES 3
#FAILSAFE 10000

; Player move and rotation speed
#MOVESPD 0.1
#ROTSPD 0.05

; Texture size (embedded into data section)
#TEXW 64
#TEXH 64

; Calculated values
#SCREENW_FLT !(&SCREENW)
#SCREENH_FLT !(&SCREENH)
#TEXW_FLT !(&TEXW)
#TEXH_FLT !(&TEXH)
#MAPSIZE !(MAPW*MAPH)
#SBUFSIZE !(SCREENW*SCREENH)

; Code starts here
.0
jmp main

dd rng_ret 0

.?8
:rng
>rng
    movw x seed

    ;x ^= x << 13;
    shlw y x %13
    xorw x x y
    ;x ^= x >> 17;
    shrw y x %17
    xorw x x y
    ;x ^= x << 5;
    shlw y x %5
    xorw x x y

    movw seed x
    jmp @rng_ret

dw x 0
dw y 0
<

dd memset_ptr 0
dd memset_len 0
dd memset_ret 0
db memset_byte 0

.?8
:memset
>memset
    mov i %0
    :loop
        cmp i memset_len
        jif endloop %3

        movb @memset_ptr memset_byte
        add i i %1
        add memset_ptr memset_ptr %1
        jmp loop
    :endloop
    jmp @memset_ret

dd ret 0
dd ptr 0
dd len 0
dd i 0
<

dd genmap_ret 0

.?8
:genmap
>genmap
    ;memset(map,1,sizeof(map));
    movb memset_byte %1
    mov memset_ptr %map
    mov memset_len %MAPSIZE
    mov memset_ret %past_mapset
    jmp memset
    :past_mapset

    ;for (int i = 0; i < SNAKES; i++) {
    mov i %0
    :loop1
        cmpw i %SNAKES
        jif endloop1 %3

        ;int mcx = MAPW / 2;
        div mcx %MAPW %2
        ;int mcy = MAPH / 2;
        div mcy %MAPH %2

        ;for (int j = 0; j < FAILSAFE; j++) {
        mov j %0
        :loop2
            cmpw j %FAILSAFE
            jif endloop2 %3

            ;if (mcx < 0) break;
            cmsw mcx %0
            jif endloop2 %4
            ;if (mcy < 0) break;
            cmsw mcy %0
            jif endloop2 %4
            ;if (mcx >= MAPW) break;
            cmpw mcx %MAPW
            jif endloop2 %3
            ;if (mcy >= MAPH) break;
            cmpw mcy %MAPH
            jif endloop2 %3

            ;map[mcy*MAPW+mcx] = 0;
            mov idx %0
            mulw idx mcy %MAPW
            addw idx idx mcx
            add idx idx %map
            movb @idx %0

            ;rng();
            mov rng_ret %past_rng
            jmp rng
            :past_rng

            ;int d = seed & 3;
            andw d seed %3

            ;switch (d) {
                ;case 0: mcx++; break;
                cmpb d %0
                jif case1 %6
                addw mcx mcx %1
                jmp endswitch

                ;case 1: mcy++; break;
                :case1
                cmpb d %1
                jif case2 %6
                addw mcy mcy %1
                jmp endswitch

                ;case 2: mcx--; break;
                :case2
                cmpb d %2
                jif case3 %6
                subw mcx mcx %1
                jmp endswitch

                ;case 3: mcy--; break;
                :case3
                cmpb d %3
                jif endswitch %6
                subw mcy mcy %1
            :endswitch

            add j j %1
            jmp loop2
        :endloop2

        add i i %1
        jmp loop1
    :endloop1

    ;posx = (float)MAPW / 2.f;
    itf posx %MAPW
    dif posx posx %2.0

    ;posy = (float)MAPH / 2.f;
    itf posy %MAPH
    dif posy posy %2.0

    ;DEBUG
    ;for (int i = 0; i < MAPH; i++) {
    ;mov ptr %map
    ;mov i %0
    ;:dlp1
        ;cmpw i %MAPH
        ;jif edlp1 %3

        ;movw d %0
        ;;for (int j = 0; j < MAPW; j++) putchar(map[i*MAPW+j]+'0');
        ;mov j %0
        ;:dlp2
            ;cmpw j %MAPW
            ;jif edlp2 %3

            ;addb d @ptr %'0'
            ;outb d %0

            ;add ptr ptr %1
            ;add j j %1
            ;jmp dlp2
        ;:edlp2

        ;;putchar('\n');
        ;outb nl %0

        ;add i i %1
        ;jmp dlp1
    ;:edlp1

    jmp @genmap_ret

dd i 0
dd j 0
dw mcx 0
dw mcy 0
dw d 0
dd idx 0
dd ptr 0
<

dw pcmove_x 0
dd pcmove_ret 0

.?8
:pcmove
>pcmove
    ;float newx = dirx * MOVESPD;
    muf newx dirx %MOVESPD
    ;float newy = diry * MOVESPD;
    muf newy diry %MOVESPD

    muf newx newx pcmove_x
    muf newy newy pcmove_x

    ;newx = posx + newx;
    adf newx posx newx
    ;newy = posy + newy;
    adf newy posy newy

    ;if (map[(int)newy * MAPW + (int)newx] == 0) {
    fti idx newy
    fti tmp newx
    mul idx idx %MAPW
    add idx idx tmp
    add idx idx %map
    cmpb @idx %0
    jif noteq %6

        ;posy = newy;
        movw posy newy
        ;posx = newx;
        movw posx newx
        jmp @pcmove_ret

    :noteq
    ;} else {
        ;if (map[(int)posy * MAPW + (int)newx] == 0) posx = newx;
        fti idx posy
        fti tmp newx
        mul idx idx %MAPW
        add idx idx tmp
        add idx idx %map
        cmpb @idx %0
        jif noteq2 %6

            movw posx newx
        :noteq2
        jmp @pcmove_ret

        ;else if (map[(int)newy * MAPW + (int)posx] == 0) posy = newy;
        fti idx newy
        fti tmp posx
        mul idx idx %MAPW
        add idx idx tmp
        add idx idx %map
        cmpb @idx %0
        jif noteq3 %6

            movw posy newy
        :noteq3
        jmp @pcmove_ret
    ;}

dw newx 0
dw newy 0
dd idx 0
dd tmp 0
<

dw pcrot_x 0
dd pcrot_ret 0

.?8
:pcrot
>pcrot
    ;float olddirx = dirx;
    movw oldx dirx
    ;float rotdir = ROTSPD;
    muf rotd pcrot_x %ROTSPD

    ;dirx = dirx * cos(rotdir) - diry * sin(rotdir);
    cos cx rotd
    muf cx dirx cx
    sin cy rotd
    muf cy diry cy
    suf dirx cx cy

    ;diry = olddirx * sin(rotdir) + diry * cos(rotdir);
    sin cx rotd
    muf cx oldx cx
    cos cy rotd
    muf cy diry cy
    adf diry cx cy

    ;float oldplanex = camx;
    movw oldx camx

    ;camx = camx * cos(rotdir) - camy * sin(rotdir);
    cos cx rotd
    muf cx camx cx
    sin cy rotd
    muf cy camy cy
    suf camx cx cy

    ;camy = oldplanex * sin(rotdir) + camy * cos(rotdir);
    sin cx rotd
    muf cx oldx cx
    cos cy rotd
    muf cy camy cy
    adf camy cx cy

    jmp @pcrot_ret

dw oldx 0
dw rotd 0
dw cx 0
dw cy 0
<

dd walls_ret 0

.?8
:walls
>walls
    ;PARALLEL for (int x = 0; x < SCREENW; x++) {
    ppu x %SCREENW %endwalls
        clo camerax
        clo deltadistx
        clo deltadisty
        clo raydirx
        clo raydiry
        clo stepx
        clo stepy
        clo mapx
        clo mapy
        clo side
        clo sidedistx
        clo sidedisty
        clo perpwalldist
        clo wallx
        clo lineh
        clo drawstart
        clo texx
        clo tscale
        clo ty
        clo idx
        clo tmp
        clo i

        ;float camerax = 2.0 * x / SCREENW - 1.0; // camerax in range [-1,1]
        itf camerax x
        muf camerax camerax %2.0
        dif camerax camerax %SCREENW_FLT
        suf camerax camerax %1.0

        movw deltadistx %1e30
        ;float raydirx = dirx + camx * camerax;
        muf raydirx camx camerax
        adf raydirx raydirx dirx
        ;float deltadistx = (raydirx == 0) ? 1e30 : fabs(1.0 / raydirx);
        jif izero1 %0x80
        ivf deltadistx raydirx
        abf deltadistx deltadistx
        :izero1

        movw deltadisty %1e30
        ;float raydiry = diry + camy * camerax;
        muf raydiry camy camerax
        adf raydiry raydiry diry
        ;float deltadisty = (raydiry == 0) ? 1e30 : fabs(1.0 / raydiry);
        jif izero2 %0x80
        ivf deltadisty raydiry
        abf deltadisty deltadisty
        :izero2

        ;int mapx = (int)posx;
        ftiw mapx posx
        itfw mapx mapx ; we'll need floats more frequently
        ;int mapy = (int)posy;
        ftiw mapy posy
        itfw mapy mapy

        ;int side = 0;   // 0 for vertical hit (NS wall), 1 for horizontal hit (EW wall)
        movb side %0

        ;if (raydirx < 0) {
        cmf raydirx %0.0
        jif nolow1 %3
            ;stepx = -1;
            movw stepx %-1.0
            ;sidedistx = (posx - mapx) * deltadistx;
            suf sidedistx posx mapx
            muf sidedistx sidedistx deltadistx
            jmp next1

        ;} else {
        :nolow1
            ;stepx = 1;
            movw stepx %1.0
            ;sidedistx = (mapx + 1.0 - posx) * deltadistx;
            adf sidedistx mapx %1.0
            suf sidedistx sidedistx posx
            muf sidedistx sidedistx deltadistx
        :next1

        ;if (raydiry < 0) {
        cmf raydiry %0.0
        jif nolow2 %3
            ;stepy = -1;
            movw stepy %-1.0
            ;sidedisty = (posy - mapy) * deltadisty;
            suf sidedisty posy mapy
            muf sidedisty sidedisty deltadisty
            jmp next2

        ;} else {
        :nolow2
            ;stepy = 1;
            movw stepy %1.0
            ;sidedisty = (mapy + 1.0 - posy) * deltadisty;
            adf sidedisty mapy %1.0
            suf sidedisty sidedisty posy
            muf sidedisty sidedisty deltadisty
        :next2

        ;while (!hit) {
        :loop1
            ;if (sidedistx < sidedisty) {
            cmf sidedistx sidedisty
            jif nolow3 %3
                ;sidedistx += deltadistx;
                adf sidedistx sidedistx deltadistx
                ;mapx += stepx;
                adf mapx mapx stepx
                ;side = 0;
                movb side %0
                jmp next3

            ;} else {
            :nolow3
                ;sidedisty += deltadisty;
                adf sidedisty sidedisty deltadisty
                ;mapy += stepy;
                adf mapy mapy stepy
                ;side = 1;
                movb side %1
            :next3

            ;if (map[mapy*MAPW+mapx] > 0) hit = 1;
            fti idx mapy
            fti tmp mapx
            mul idx idx %MAPW
            add idx idx tmp
            add idx idx %map
            cmpb @idx %0
            jif endloop1 %2

            jmp loop1
        :endloop1

        ;if (side == 0)
        cmpb side %0
        jif nozero1 %6
            ;perpwalldist = (mapx - posx + (1 - stepx) / 2.0) / raydirx;
            suf tmp %1.0 stepx
            dif tmp tmp %2.0
            suf perpwalldist mapx posx
            adf perpwalldist perpwalldist tmp
            dif perpwalldist perpwalldist raydirx
            jmp next4
        ;else
        :nozero1
            ;perpwalldist = (mapy - posy + (1 - stepy) / 2.0) / raydiry;
            suf tmp %1.0 stepy
            dif tmp tmp %2.0
            suf perpwalldist mapy posy
            adf perpwalldist perpwalldist tmp
            dif perpwalldist perpwalldist raydiry
        :next4

        ;int lineh = (int)(SCREENH / perpwalldist);
        dif tmp %SCREENH_FLT perpwalldist
        ftiw lineh tmp

        ;center the line on the screen
        subw drawstart %SCREENH lineh
        divw drawstart drawstart %2

        ;if (side == 0)
        cmpb side %0
        jif nozero2 %6
            ;wallx = posy + perpwalldist * raydiry;
            muf wallx perpwalldist raydiry
            adf wallx wallx posy
            jmp next5
        ;else
        :nozero2
            ;wallx = posx + perpwalldist * raydirx;
            muf wallx perpwalldist raydirx
            adf wallx wallx posx
        :next5

        ;wallx -= floor(wallx);
        ftiw tmp wallx
        itfw tmp tmp
        suf wallx wallx tmp

        ;int texx = (int)(wallx * TEXW);
        muf texx wallx %TEXW_FLT
        ftiw texx texx

        ;if (side == 0 && raydirx > 0) texx = TEXW - texx - 1;
        ;if (side == 1 && raydiry < 0) texx = TEXW - texx - 1;
        cmpb side %0
        jif next6 %6
            cmf raydirx %0.0
            jif next6 %5
            subw texx %TEXW texx
            subw texx texx %1
        :next6
        cmpb side %1
        jif next7 %6
            cmf raydiry %0.0
            jif next7 %3
            subw texx %TEXW texx
            subw texx texx %1
        :next7

        ;float tscale = (float)TEXH / (float)lineh;
        itfw tscale lineh
        dif tscale %TEXH_FLT tscale
        ;float ty = 0;
        movw ty %0.0

        ;if (drawstart < 0) {
        cmsw drawstart %0
        jif noless2 %3
            ;ty = tscale * -((float)drawstart);
            itfw ty drawstart
            ngf ty ty
            muf ty tscale ty
            ;drawstart = 0;
            subw lineh lineh drawstart
            movw drawstart %0
        :noless2

        ;for (int i = drawstart; i <= drawend; i++) {
        mov i %0
        ;movw i drawstart
        :loop2
            cmpw i lineh
            jif endloop2 %3

            ;if (i >= SCREENH) break;
            addw tmp i drawstart
            cmpw tmp %SCREENH
            jif endloop2 %3

            ;sbuf[i*SCREENW+x] = tex[(int)ty * TEXW + texx]
            fti tmp ty
            mul tmp tmp %TEXW
            mov idx %0
            movw idx texx
            add tmp tmp idx
            add tmp tmp %tex_wall

            mov idx %0
            addw idx i drawstart
            mul idx idx %SCREENW
            add idx idx x
            add idx idx %sbuf

            movb @idx @tmp
            ;movb @idx %'#'

            ;ty += tscale;
            adf ty ty tscale

            add i i %1
            jmp loop2
        :endloop2
    :endwalls
    jmp @walls_ret

dd x 0
dw camerax 0
dw deltadistx 0
dw deltadisty 0
dw raydirx 0
dw raydiry 0
dw stepx 0
dw stepy 0
dw mapx 0
dw mapy 0
db side 0
dw sidedistx 0
dw sidedisty 0
dw perpwalldist 0
dw wallx 0
dw lineh 0
dw drawstart 0
dw texx 0
dw tscale 0
dw ty 0
dd idx 0
dd tmp 0
dd i 0
<

dd floors_ret 0

.?8
:floors
>floors
    ;PARALLEL for (int y = SCREENH / 2 + 1; y < SCREENH; y++) {
    ppu y %!(SCREENH/2) %endfloor
        add y y %!(SCREENH/2+1)

        clo x
        clo tmp
        clo idx
        clo src
        clo rdx0
        clo rdy0
        clo rdx1
        clo rdy1
        clo rowd
        clo stepx
        clo stepy
        clo floorx
        clo floory
        clo tx
        clo ty

        ;float rdx0 = dirx - camx;
        suf rdx0 dirx camx
        ;float rdy0 = diry - camy;
        suf rdy0 diry camy
        ;float rdx1 = dirx + camx;
        adf rdx1 dirx camx
        ;float rdy1 = diry + camy;
        adf rdy1 diry camy

        ;int p = y - SCREENH / 2;
        subw rowd y %!(SCREENH/2)
        itfw rowd rowd
        ;float posZ = 0.5 * SCREENH; // the floor height relative to the camera (could tweak)
        muf tmp %SCREENH_FLT %0.5
        ;float rowd = posZ / p;
        dif rowd tmp rowd

        ;float stepx = rowd * (rdx1 - rdx0) / SCREENW;
        suf stepx rdx1 rdx0
        muf stepx rowd stepx
        dif stepx stepx %SCREENW_FLT

        ;float stepy = rowd * (rdy1 - rdy0) / SCREENW;
        suf stepy rdy1 rdy0
        muf stepy rowd stepy
        dif stepy stepy %SCREENW_FLT

        ;float floorx = posx + rowd * rdx0;
        muf floorx rowd rdx0
        adf floorx floorx posx
        ;float floory = posy + rowd * rdy0;
        muf floory rowd rdy0
        adf floory floory posy

        ;for (int x = 0; x < SCREENW; x++) {
        mov x %0
        :loop
            cmpw x %SCREENW
            jif endloop %3

            ;int cx = (int)(floorx);
            ;int tx = (int)(TEXW * (floorx - cx)) & (TEXW - 1);
            ftiw tx floorx
            itfw tx tx
            suf tx floorx tx
            muf tx tx %TEXW_FLT
            ftiw tx tx
            andw tx tx %!(TEXW-1)

            ;int cy = (int)(floory);
            ;int ty = (int)(TEXH * (floory - cy)) & (TEXH - 1);
            ftiw ty floory
            itfw ty ty
            suf ty floory ty
            muf ty ty %TEXH_FLT
            ftiw ty ty
            andw ty ty %!(TEXH-1)

            ;sbuf[y*SCREENW+x] = (tex[ty*TEXW+tx] == '1')? '.':' ';
            mov src %0
            movw src ty
            mul src src %TEXW
            mov tmp %0
            movw tmp tx
            add src src tmp
            add src src %tex_floor

            mul idx y %SCREENW
            add idx idx x
            add idx idx %sbuf

            movb @idx @src

            ;sbuf[(SCREENH-y)*SCREENW+x] = (tex[ty*TEXW+tx] == '1')? '.':' ';
            suu src src %tex_floor
            add src src %tex_ceil

            suu idx %!(SCREENH-1) y
            mul idx idx %SCREENW
            add idx idx x
            add idx idx %sbuf

            movb @idx @src

            ;floorx += stepx;
            adf floorx floorx stepx
            ;floory += stepy;
            adf floory floory stepy

            add x x %1
            jmp loop
        :endloop
    :endfloor

    jmp @floors_ret

dd x 0
dd y 0
dd tmp 0
dd idx 0
dd src 0
dw rdx0 0
dw rdy0 0
dw rdx1 0
dw rdy1 0
dw rowd 0
dw stepx 0
dw stepy 0
dw floorx 0
dw floory 0
dw tx 0
dw ty 0
<

dd render_ret 0

.?8
:render
>render
    movb memset_byte space ; can't use ' ' here - it will be tokenized as zero-literal
    mov memset_ptr %sbuf
    mov memset_len %SBUFSIZE
    mov memset_ret %postclean
    jmp memset
    :postclean

    ; chain the calls and render everything
    mov floors_ret %walls
    mov walls_ret %postwall
    jmp floors
    :postwall

    ;printf("\e[H");
    out %cursor_home %4

    ;char* ptr = sbuf;
    mov ptr %sbuf
    ;for (int i = 0; i < SCREENH; i++) {
    mov i %0
    :loop1
        cmpw i %SCREENH
        jif endloop1 %3

        ;for (int j = 0; j < SCREENW; j++) putchar(*ptr++);
        mov j %0
        :loop2
            cmpw j %SCREENW
            jif endloop2 %3

            outb @ptr %0

            add ptr ptr %1
            add j j %1
            jmp loop2
        :endloop2

        ;printf("\n\r");
        out %cursor_next %4
        add i i %1
        jmp loop1
    :endloop1

    jmp @render_ret

dd i 0
dd j 0
dd ptr 0
<

.?8
:main
>main
    ;TODO - entropy

    ; generate the map
    mov genmap_ret %past_gen
    jmp genmap
    :past_gen

    ; hide the cursor
    out %cursor_hide %4

    ; chain the calls
    mov pcmove_ret %render
    mov pcrot_ret %render
    mov render_ret %gloop

    ; first frame
    jmp render

    ; main game loop - process keyboard events and render
    :gloop
        inpb x %0
        jif gloop %0x80
        cmpb x nl
        jif gloop %1

        cmpb x %'q'
        jif endgame %1

        cmpb x %'w'
        jif notw %6
        movw pcmove_x %1.0
        jmp pcmove

        :notw
        cmpb x %'s'
        jif nots %6
        movw pcmove_x %-1.0
        jmp pcmove

        :nots
        cmpb x %'a'
        jif nota %6
        movw pcrot_x %-1.0
        jmp pcrot

        :nota
        cmpb x %'d'
        jif notd %6
        movw pcrot_x %1.0
        jmp pcrot

        :notd
        jmp render
    :endgame

    ; show the cursor
    out %cursor_show %4

    hlt

db x 0
<

db nl "\n"
db space " "
db cursor_hide "\e[?25l\0"
db cursor_show "\e[?25h\0"
db cursor_home "\e[H\0"
db cursor_next "\n\r\0"

.?8
dw posx 0
dw posy 0
dw dirx 1.0
dw diry 0.0
dw camx 0.0
dw camy 0.66
dw seed 238612043

db map 0
.+MAPSIZE

db sbuf 0
.+SBUFSIZE

db tex_wall ".._IY?l|<=ssa, uou|>{**`  _%+'+<s_auwa,. _a1I'?Umqmwss_%=saa. ._"
db "iiliii|||i%XXe 3q'-^=%`:=vn' .)XXX?XXXmc:S1%|=:   :<iIvIvvvX;:nv"
db "lIli|i|iIv%voc /5=_. ` +IIi_.=>|I^  !Y*c:vlii=;:===iIInnvvnn+:nn"
db "i||>iiiIlvIvo( 3>vs==_|i%vnI`:<v(   :lv=:ll++|||%||<vvIvIvn2':n1"
db "|||++~~^+{+{*` r<}.%v<'^iXSi.-o2<||_:l>. '`.--~~~+~~'*++<<>`  {%"
db "s_a,,.        .   ~~       ~+      `  -`                   .s,ss"
db "'?9WW#Sa__._:3&  .s__s_%__sa,_umgaas,svv=_]WmmmZS2S13XwaaaoXTT?|"
db "== --{Xozi:=<no  {Y*SnnvnnoI'=;nX*>'1I|IlSi1i'+<>+=|iii3S2o}==:|"
db "||<`--{n1l;=ive .:vi. :<}{<|.=cve||=:)sv)n1+:-  -<vIii:`.+`. :=="
db "il+ :. +}|-=iv( ==i==:=<=|--  u{ni=ivvI>-^:vi =_=%%|+=_.   .:.::"
db "|| :-.. +|;inn; |i+=+i%il=. ,:1-onvon!'`.  )vilnal~<||>: .:.._;|"
db ">._|||._=v|no2 -o===%v|>  -%>)<,)ZZ?'   =a+'ivvnn(;_ii-=||==|ivi"
db "_|i|i<vvn|'3S+ .{v,+I%<=. =S; 3#;-     _ ~=.:+^'`:Insi|vivwqowo'"
db "vnnovvI=. .<%_=:viis%|vv%vno( 2(    ....._<==i___.--'^''v1**YY1v"
db "Y!'++~---  `iuv<1{*|iv||nXr-<.r ._ii|=|=v}^<=-~-==_|:...._:-~ .{"
db "             ~`---       '   .=uwmZou1o1^    . .  -`   --       "
db "    .. __a,,  .;.           .       -^              ____,,      "
db "__|ive)WQVVY1i|i` . _awwwwqZUWmmQc  ]BVUV9TTYY3XXU#i~{3VW6;._|=_"
db "iivvv=-=i{vovvuv.=ooX1I1||+|=` )Vo; )vli|||||ii%I3Y'%i==+vn;ivii"
db "i|iiv(.=1==IoS1a;:XSvli;=<=:+;.:/{( )vIlIi|iilvvvvi <||=|vv`vli|"
db "|||il>=}<l^  -<l  :+li|=:===:._=|I= inovvnvnvnvv>'` =nv|i%>`vv||"
db "--- - -   .  `.=` :<+|=..++;==|iuXi >'*!}!'!il>` . .i%illi~`'l+~"
db "usi=%vo,     :''`{1+Iv>=|==;=|v>'1X;                >'Iii%;  <sw"
db "SS1ni'=_.__,as,. l}; +v||i%vvl~..:3c   __s_ssaauwawo   ~~`   . ?"
db "'+|+ {nXomm211l= ~-.  <%vvn}^:,_=|I<..<v+-+<|I'-=>?+ _>~` _,=i=)"
db ".:|= :<|{v>.-<nc  ....|**'~ ;=+=|+<)(-Sv_i{o2`.<i%s_=oc=i3#E-|i%"
db ".=i;.+ivs%+` ,3m,;<siii>;. .-:+|=%v-+ ]Souod+.=%Iv><.o::;{}- =}+"
db ";<li :='-<i<=%iv'<=nn|~|%i=;=i||vv>i> <3##Xz` <vv}.- i:==i` =>>;"
db "iili ___,=nnnoV` v>{v>;)<ivvvvvvvn')<  <11v;:=~~'+<v {|=v% v-;=<"
db "|--{.I'l+~)?!''. %svi..:vnnoo2Sn2ni`-:%~='^==_%;. :> >vXo|=|. =+"
db "-  ::=;        -:`---::-~-''^` ~ --{=:'/`   -'1+`     -'^--i-::`"
db "                                    `:          _as_aa,s_aasaa,,"
db "s_ugwa,, _uXXYVQQmmwa_s,=<ss   ..._vXVSInsaaac qmmSUVVV'- =%'!lI"
db "#ZTUX#mc:SovIi|`:=<iIvvIvvv3;:vIiiIv1IlIIlvU#e 3m(+'vv++<vn' :)X"
db "I^  !Y*c:vlIi||<|||ilvvnvvnn+-nnvIlliillvvIvoc )5<_..` <IIl_.=>="
db "(   :lv=:ll++|||||+<vIvvIvn2':nll||>|iiIlvlvn; 3+vs:=.|i%vnI`.<i"
db "<||_.l>. '`.--~~^++~'*++<<>`  {%|||++~~^+{+{*` r<}.%%{'^<Xoi.-ne"
db "     --`                   .s,ss_as,,.        .   -~       ++   "
db "gaas,%vv=_]WmmmXXSS13XwaaauXTT?|!!YBW#Sa__._:3&  .s,_s_%__sa,_um"
db "**+'1I|IlSi1i'+<|+=|iii3S2o}+:==+= --vXozi:=<no  {Y?SnnvnnoI':;n"
db "I||==|sv<SI+:-  -<vIii= :+`. :==i|<`--{nvl;=ive .:vi. :<}{<|.)c3"
db "l|=iivv>-^:vi =_=i%|+=_.   .:..=il+ :. '}|-=iv( ==|>::=<|=--  v{"
db "nnvoo!'`.  )vilnul~<||>: .:.._;||> :-.. +|;inn; |i+=+i%ii+. ,:1-"
db ")XZ?'   =a+'ivvvn(=_ii-=||==|ivv+ _|||._=v|vo2 -o===%v|>  -%>)<,"
db ";-    ._ ~=.:=^'`_Ivsi|vivwwwoo(_|ii|isvn|'3X+ .{v,+I%<=. =S= 3Z"
db "    _. .._<==i___.-~'^''v1**?V1ivvnnvII=. .<s_=:viis%|vv%vno> v("
db "._ii=<|=v}^<=-~-==_|: ..._:-~  !*?'+<~---  `ivz<1{*|iv||nXr-<.r "
db "wmZoo1nI'    . .  -`   ~-                    ~`---       '  - =u"
db "   .-~              _____,          .. __s,,  .;.               "
db "mL  ]BVUV9TTYY3XXU#i~{YVW6;.=|;__=|ive)WQUVY1i||` . _augwwqZ$Wmm"
db "Vo; )vli|||||ii%I3Y'%i==+vn;ilii|llvv> <i*vovvuv.=ooX111||+|=` |"
db "/{( )vIivi|iilvvvvi <||=|vv`Ili||iiIv>.=l=;Io21a;:XSvli;=<=:+;::"
db "=I= inovvnvnvnvv>'` =vv|i%>`vv||||||l>=}<I^  -<I  :+li|=:==+.:=="
db "uXi >'*!}!'!il>` . .ivilli~`'l+~--- - -   .  `.=` :<+|=..++;==|i"
db "'1X;                >'l|i%;  =nwua><<uo,.    :''`{1+Iv>=|====iv>"
db ".=3c   __s_ssaauwawo   ~^`   ..?SS1ni'=_.__,as,. l}; +v||i%vvi~."
db "=<I<..<v+-+<|I'-=>?+ _>~` _,=i=)}+|+ {noXdmS11l= ~-.  <%vvn}^:,_"
db "|+=)(-Sv_i{n2`.=s%s_=oc=i3#E-||% :|= :<i)n>.-<nc  ....|**'~ ;=|="
db "=iv-+ ]Souod^.=%vv+<.o::;i}- =}+:=i;.+<vs%+` _3m,;<siii>_...-:+|"
db "vv>i> <3##Xz`.<vv}.- i.|;i` :>>;;=v| :>^-<i<;%iv'<=nn|~|%i=_=|||"
db "vn')<  i*1n;:=~~'~|v {|=%% %-;=<iili =__,=nnowX` v>{v>;)<ivvsvvv"
db "Xos;-=%+|'^+i_s=. =% >vSo==+. =i|~~v;v+I>={Y!'',.vous:.+vonXo2So"
db " --Sc/1%`   -?Sl~     -'^ -+-.==|..<iI%..    -=v=>-'+<i='~'''--^"
db "    <u.    .   -saasaa,s_assaa,%`---|^++=-. -- <c-:: .          \0"

db tex_floor "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "+++++++++++++++++++++++++++++++++++++++++++++...............+++++....."
db "..........+++++...............+++++++++...............+++++..........."
db "....+++++...............+++++++++...............+++++...............++"
db "+++...............+++++++++...............+++++...............+++++..."
db "............+++++++++...............+++++...............+++++........."
db "......+++++++++...............+++++...............+++++..............."
db "+++++++++...............+++++...............+++++...............++++++"
db "+++...............+++++...............+++++...............+++++++++..."
db "............+++++...............+++++...............+++++++++........."
db "......+++++...............+++++...............+++++++++..............."
db "+++++...............+++++...............+++++++++...............+++++."
db "..............+++++...............+++++++++...............+++++......."
db "........+++++...............+++++++++...............+++++............."
db "..+++++...............+++++++++...............+++++...............++++"
db "+...............++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++....."
db "..........+++++...............+++++...............+++++++++..........."
db "....+++++...............+++++...............+++++++++...............++"
db "+++...............+++++...............+++++++++...............+++++..."
db "............+++++...............+++++++++...............+++++........."
db "......+++++...............+++++++++...............+++++..............."
db "+++++...............+++++++++...............+++++...............+++++."
db "..............+++++++++...............+++++...............+++++......."
db "........+++++++++...............+++++...............+++++............."
db "..+++++++++...............+++++...............+++++...............++++"
db "+++++...............+++++...............+++++...............+++++++++."
db "..............+++++...............+++++...............+++++++++......."
db "........+++++...............+++++...............+++++++++............."
db "..+++++...............+++++...............+++++++++...............++++"
db "+...............+++++...............++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "+++++++++++++++...............+++++...............+++++..............."
db "+++++++++...............+++++...............+++++...............++++++"
db "+++...............+++++...............+++++...............+++++++++..."
db "............+++++...............+++++...............+++++++++........."
db "......+++++...............+++++...............+++++++++..............."
db "+++++...............+++++...............+++++++++...............+++++."
db "..............+++++...............+++++++++...............+++++......."
db "........+++++...............+++++++++...............+++++............."
db "..+++++...............+++++++++...............+++++...............++++"
db "+...............+++++++++...............+++++...............+++++....."
db "..........+++++++++...............+++++...............+++++..........."
db "....+++++++++...............+++++...............+++++...............++"
db "+++++++...............+++++...............+++++...............++++++++"
db "+...............+++++...............+++++...............++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
db "++++++++++++++++++++++++++++++++++++\0"

db tex_ceil "     ..  .         .      .....  ... .   .... ..    ...  ...         ."
db ".    .... .. . ......  .   .  ...        .    ....  ..    .    ..   .."
db "... . .. .   .. ..  .     .        .  .    .   ..   ....  . .......   "
db " ...      ..  ..    .   .    .       ..  ..     ... .  .   ..    .  . "
db "     .  .   .     .    . ...    .  ....   .  ..          .   .        "
db "  .      .  ..   .   .     . ...    .  ..     .        .   .      .   "
db "           .         .....  .......         .  . ..         ...       "
db "     ..        .................... .........    . .  .    .. ........"
db ".......... .....             ..  ..    ............  .  . ..       .  "
db "       .     . ..      .            .   .       .    .   ...          "
db "        ...      .       .    .    .      .    .. ..... ...  ....     "
db "   .      ..            .    .      . .             ........      ...."
db ".  ...     .   .  ...  .     .....     ..  .         .          ...  ."
db "....... ..............       .       .. .. .   .               . .    "
db "      .       ..       ..    . .    .    .               .      ..    "
db ".       ..      .. ..    ......   ..  ..   .   ..... .   ...    .     "
db "  ..    .. .  ..   .......   ..............  ..... ..    ..        .  "
db " .  .                   ..  ..     . ..  .      ....      .  .        "
db "     . .   .....  . .        .    ..     .. ...   .    .              "
db " .   .     .. .     ....    .........  ... ... ........ ..   ......  ."
db "..............     .      .  ...        ..      ........  .  ...  ...."
db "       .  ..            .         .            ..   .             ..  "
db "     .   .   ..   .                      .       ..      ... .. ..   ."
db ". ...       .   .  ..  .   ..      ..     ..  .......   .. ..  .     ."
db "      .         .....        ..     .    ..   . .   ..   ..           "
db ".     ..  . ....   .......         ...  ..     ... .            .     "
db "            .... .          ...   .      .    .           ..       .. "
db "      .    .         .      . .   .....................  .......    .."
db "    .....     .    .........         .....       ..          ...      "
db "  .....         .. ..       ... ..     .   ...   ....   .            ."
db "      .. ..  .  ...        .    ....  ... .      ..    ..      .  ...."
db ".. .        .        .  .    .   ..   .    ..   ..      .. .   .. ..  "
db ".     .   .    .       ..  ..   ....  . .......    ...      ..  .   . "
db "    .    . ...    .  ....   ... .  .   ..    .  .      .  .      .  .."
db "   .   .     . ...    .  ..          .   .          .              .  "
db "       .....    .  ..     .        .   .      ...            ..       "
db " ..............         .  . ..              .. .................. ..."
db "............... .........    . .. ...  .  . ..       .         .      "
db "         ..  ..    .........      .    .   ...                 . ..   "
db "   .            .   .       .    .. ..... ...  ....       ...      .  "
db "     .    .    .      . .             ........       .      ..        "
db "    .    .     .....     ..  .         .      .....  ...     .   .  .."
db ".  .....       .       .. .. ..  .          ...  ........ ..........  "
db "..       ..    . .    .    .               . .          .       ..    "
db "  .. ..    ......   ..               .      ..    .       ..    .. .  "
db "..   .......   .....   .   ..... .   ...    .        .   .  .         "
db "          .............  ..... ..    ..     .  .             . .   ..."
db "..  .   ..     . ..  .      ....         .               .   .      . "
db ".        .    ..     .. ...   .. ........ ..   ......  ..........     "
db "....    .........  ... .....      ........  .  ...  ....  ......     ."
db "      .  ...                     ..   .                  .  ..        "
db "    .                      .       ..      .....       .   .   ..   . "
db "      .  .   ..      ..     ..  .......  .. ..   .. ...       .   .  ."
db ".. ....        ..     .    ..   .   .. ..  .     .      .           .."
db "..   .......         ...  ...   ..   ..           .     ..          .."
db ".. .          ...   .      ... .            .         .       .    .  "
db "       .      .      .    .           ..       ...    ..    .....     "
db ".. . ......   ........... .........  .....     ...        .....       "
db "  . ...          .....       ..     \0"

STOP

TODO - add entropy reader
for (int i = 0; i < 6;) {
    char x = 0;
    read(1,&x,1);
    if (!x) continue;
    seed |= (uint32_t)x << i*8;
    i++;
}
