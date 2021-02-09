;
; "Copper bar" effect in 256 bytes, including the background picture
; Machine: BK-0011M in default configuration; color monitor required.
;
; (c) 2021 Alexander "Sandro" Tishin
; 
; License: MIT
;
; Required compiler: pdpy11
;
; Recommended music: Doom 2016 soundtrack by Mike Gordon ;)
;
; All table addresses should be 256 bytes aligned
;
; start address for 6 8x10x2 tile images; also the end of unpacked tile map
; should have low byte set to zero for correct decompression.
tiles	= 5000 ; beginning of cleared memory
; bar height in words
hbar	= 2000 ; 16 lines
; magical miltiple-use numbers
; place to assemble the decompressed picture and the bit-magic number 20000 plus constant 12 (tile block height)
picdst	= 20000
magic	= picdst + 12
; very magical
palpage	= 46000 ; no vsync, show page 1 (#5), palette #12 (RGC)
tbuf	= palpage ; target (screen) buffer for bars (#46000 )
ultra	= palpage + 40 ; one-shot loop counter in low byte; ingnored by system register, zeroed after loop

.link 1000
start:	; first -- force RAM page map; ANDOS compatible for the fun of it ;)
	mov #16200, @#177716 ; will do nothing on BK-0010
	; save magic constant #20012 on stack top
	mov #magic, -(sp)
	; clear RAM, screen and scroll to center the image
	mov @pc, r0
1$:	clr (r0)+
	tst r0
	bpl 1$
	mov #ultra, r1 ; #46040; both video palette/page setting and starting color bar address, and constant #40
	; will cause trap to 4 on BK-0010 ....
	mov r1, @#177662 ; no vsync, show page 1 (#5), palette #12 (RGC)
	mov #1350, @#177664 ; slightly scroll up to align image better
	; make tiles
	; tiles are generated in reverse, so final address (in r5) is
	; used as the tile table base address when decompressing;
	; this also saves a constant for the end-of-loop comparison (zero)
	mov #tiles + 5 * 24 - 4, r5 ; skip tile 0 (empty) and two lines
	; tile 4 is a right-up triangle
	asr r0 ; makes constant 140000 from 100000 already present in the r0
	mov #10, r2 ; tile height minus two (for proper stitching when flipped)
12$:	mov r0, -(r5)
	asr r0
	asr r0 ; as a side effect, at the last iteration sets the carry flag
	sob r2, 12$
	mov r5, r0
	; tile 3 is the tile 4 flipped vertically
	;
	; now top of stack contains byte constant #12 and r2 is free
	; this code zeroes it, making the constant a proper number #20000
	; instead, it can be loaded into r2, to be reused later, but it will require
	; offsetting all related addresses later by -012 << 4 (3? -- I'm too lazy to calculate ;) )
	; ... saving two bytes more by greatly complicating the math
	; but since .bin is already 256 bytes, I won't do that
13$:	mov (r0)+, -(r5)
	decb @sp
	bne 13$
	; tiles 2..0 are flipped and inverted tiles 3..5
	mov r5, r0
14$:	mov (r0)+, -(r5)
	com @r5
	bitb r5, r5 ; instead of tstb will keep the carry flag
	bne 14$
	; unpack tile map
	mov #tmap, r4
	mov r5, r0 ; tile map will be unpacked backwards, in front of tiles
	mov @sp, r3 ; #20000 magic constant
	;sec ; end-of-word bit; no need -- carry is already set by "asr r0" way up
21$:	mov (r4)+, r2
22$:	rol r2
	beq 21$ ; carry flag carries end-of-word 1 bit
	rol r3
	bcc 22$
	movb ofstbl(r3), -(r0)
	mov @sp, r3 ; #20000 magic constant
	cmp r0, #tiles - 32. * 5 + 1
	bcc 22$
	; draw tiles
	; unpacked tile map pointer is already in r0
	; tile table pointer is already in r5
	mov @sp, r2 ; picdst == magic == #20000
33$:	movb @pc, r4 ; use low byte of the next insn as the constant 5
32$:	bisb (r0)+, r5
	mov #12, r3
31$:	mov (r5)+, @r2
	add #100, r2
	sob r3, 31$
	clrb r5; move up? ;)
	sob r4, 32$
	sub #5 * 12 * 100 - 2, r2
	decb r1 ; #40 initially
	bne 33$
	; done initializing, infinite loop
	; at start:
	; r0 - never used
	; r1 - #46000; both video palette/page setting and starting color bar address
	; r2 - picdst + 100
	; r3 - 0
	; r4 - 0
	; r5 - tile table pointer (tiles, 5000)
	; a single bar version (multiple bars do not fit into 256 bytes)
bbuf	= 10000 ; back buffer (with decompressed picture)
lines	= 300 ; 300 - hbar ; effect height
delay	= 1400 ;1000 is ok, but the start effect is too fast ...
	 ; color code for effect
	;mov #52525, r5 ; red/green bar
	mov #125252, r5 ; green/red bar
	; r1 already contains the video buffer address (#46000)
40$:	mov #lines, r3 ; line count
41$:	call nextl
; hacks for pdpy11
baseofs	= bbuf - tbuf
nhbar	= 0 - hbar ; pdpy11 does not understand unary minus (only here?)
barofs	= baseofs - hbar
;
42$:	mov barofs(r1), nhbar(r1) ; trailing (upper) edge of bar
	xor r5, (r1)+ ; leading (bottom) edge of bar; overdraw with translucent color
	sob r2, 42$
	sob r3, 41$
; could save 1 word by packing this loop into previous one in this way:
; following loop is 10 words; 4 is the working code, 6 maintenance
; call the pic updating insns from single loop:
; 1 call, 2 rets -> 4 words.
; xor-patch -> 4 words (mov #,reg; xor reg,@#addr)
; 2 words extra(8 - 6), but: call nextl/ret -> 3 words
; so inlining nextl gives net result of saving 1 word
;
; but:
;
; call @#addr -> mov #addr, reg; call @reg -> +1 word
; mov #patch, reg; xor reg, reg -> +3 words
; same 4 words as the xor-patch, but if regs can be preloaded?
;
; jsr r4, cont (sub follows) -> 2 words
; mov (r4)+, r0 -> 1 word (loads xor-patch value)
; saves 1 word wrt to naive loading
;

	mov #lines, r3 ; line count
50$:	jsr pc, nextl
51$:	mov baseofs-2(r1), -(r1) ; trailing (bottom) edge of bar
	; upper edge is already drawn, needs only recoloring
	bis r5, nhbar(r1)
	sob r2, 51$
	sob r3, 50$
	
	br 40$ ; infinite loop

; next line prologue
nextl:	mov #delay, r2
	sob r2, .
	mov #40, r2 ; words per line
	rts pc
	
	; tile image offsets by tile index
	; order: empty, filled, rd, ld, ru, lu
	; Some tile references are offset for prettier (better stitched) chars,
	; so tiles overlap. Also see the length/offset tricks in generator.
ofstbl:	.byte 144, 0, 76, 50, 116, 22
	; tile map; 3bpt, 32 tiles/row, 5 rows
	; 1 row == 96 bits == 12 bytes == 6 words
	; tile map is stored backwards, from right bottom to upper left
	; column first;
	; tiles are encoded MSB to LSB in 3-bit packs
tmap:	.word 072227 ; cols 31 & 30 R
	.word 004004 ; cols 30 & 29
	.word 111110 ; cols 29 & 28
	.word 000002 ; cols 28 & 27 E
	.word 000044 ; cols 27 & 26
	.word 010141 ; cols 26 & 25
	.word 022400 ; cols 25 & 24
	.word 000024 ; cols 24 & 23 P
	.word 113010 ; cols 23 & 22
	.word 002222 ; cols 22 & 21
	.word 044000 ; cols 21 & 20
	.word 000511 ; cols 20 & 19 P
	.word 060200 ; cols 19 & 18
	.word 044444 ; cols 18 & 17
	.word 100000 ; cols 17 & 16 O
	.word 122226 ; cols 16 & 15
	.word 040006 ; cols 15 & 14
	.word 011120 ; cols 14 & 13
	.word 000002 ; cols 12 & 11 C
	.word 000044 ; cols 11 & 10
	.word 000141 ; cols 10 & 9
	.word 022400 ; cols 9 & 8
	.word 000000 ; cols 8 & 7
	.word 000511 ; cols 7 & 6 O
	.word 026200 ; cols 6 & 5
	.word 006044 ; cols 5 & 4
	.word 120000 ; cols 4 & 3
	.word 002222 ; cols 3 & 2 N
	.word 140020 ; cols 2 & 1
	.word 111111 ; cols 1 & 0
	; 65. digrams? 3 *65 = 195 bits, 24 bytes, 12 words?
	; bpl; negb;movb -> 3 words, 1w const -> 6-4 = 2w. So much for compressing doubles.
tend:
.end
