;;----------------------------------------------------------------------------------------------------------------------
;; Low-level screen printing routines
;;----------------------------------------------------------------------------------------------------------------------

; work-around for CSpect inaccuracy in copper wait instruction emulation
W_OFSY          EQU     1                       ; 0 = real HW board, 1 = CSpect

;;----------------------------------------------------------------------------------------------------------------------
;; Video modes

videoMaxX       db      80
videoMaxY       db      42

InitVideo:
                xor     a
                out     (254),a

                call    SetFullTilemapPalette
                call    ClearScreen

        ; Set up the tilemap
        ; Memory map:
        ;       $4000   80x32x2 tilemap (5120 bytes)
            ;*; (80x43x2 = 6880 bytes)
        ;       $6000   128*32 tiles (4K)
        ;
                nextreg $07,2                   ; Set speed to 14Mhz
                nextreg $6b,%11000001           ; Tilemap control
                    ;*; !enable!80col-noAttr-palNum-r-r-UlaOverTm!tmOverUla
                nextreg $6e,$00                 ; Tilemap base offset
                    ;*; $4000 is start .. $5400 end (80x32) ... $5AE0 end (80x43)
                nextreg $6f,$20                 ; Tiles base offset
                    ;*; $6000
                nextreg $4c,$0f                 ; Transparency colour (last item)
                nextreg $68,%10000000           ; Disable ULA output
                    ;*; !disableUla-blending-r-r-r-r-r-stencil
                nextreg $1C,$08                 ; reset tilemap clip window index to 0
                nextreg $1B,0                   ; reset clip window to 640x256
                nextreg $1B,159
                nextreg $1B,0
                nextreg $1B,255
                nextreg $30,0                   ; reset TM.Xoffset=0

        ;; Set up Copper code to display 80x42.6 8x6px tiles
        ; set up Copper control to "stop" + index 0

                nextreg $61, 0                  ; COPPER_CONTROL_LO
                nextreg $62, 0                  ; COPPER_CONTROL_HI
                ; set COPPER_DATA for write by OUT (c)
                ld      bc,$243B                ; TBBLUE_REGISTER_SELECT
                ld      a,$60                   ; COPPER_DATA
                out     (c),a                   ; select copper data register
                inc     b                       ; BC = TBBLUE_REGISTER_ACCESS = $253B
                ld      hl,$8000 | (39<<9) | 3+W_OFSY  ; first WAIT instruction (at [3, 39])
                ld      de,$310C                ; first TM.Yoffset=12 instruction
        ; copper is restarted at [0,0] where everything is pre-set already from end of frame = nothing to do
videoMode80x42_CopperSetupL1
                ; every 6 pixels bump Tilemap-Y-Offset by two to squish original tiles to 8x6 pixels
                ; copper horizontal compare 39 = the beam is in H-blank (HW does fetch tilemap data 8-16px ahead!)
                out     (c),h                   ; wait instruction
                out     (c),l
                ; check if Y-offset == 64 -> change base address
                bit     6,e
                jr      z,keepBaseAddress
                bit     7,e
                jr      nz,keepBaseAddress
                ; set base address to connect text on $5400+ as lines 32+
                ; this requires a bit tricky setup... (to not wrap around toward end of screen)
                ; scroll -64 (8 rows "down"), $4A00 base address (original row 16)
                ld      a,$6e
                out     (c),a
                ld      a,$0A                   ; +10 (16 lines below original base)
                out     (c),a
                ld      e,192                   ; Y-offset will now grow from -64 (8 rows)
keepBaseAddress
                out     (c),d                   ; set tilemap Y-offset instruction
                out     (c),e
                inc     e                       ; Y-offset += 2
                inc     e
                ld      a,l
                add     a,6
                ld      l,a
                cp      231+W_OFSY              ; wait-for-line-226 (225) was written?
                jr      nz,keepScanline
                ; first fully invisible line was reached, reset everything for [0,0] tile
                ; the wait + yoffset was already filled in -> just setup all
                ; set base address back to $4000 for tilemap line 0
                ld      a,$6e
                out     (c),a
                xor     a
                out     (c),a
                ;; FIXME calculate real top border scanlines (280..316 is just hardcoded experiment)
                ld      l,$FF&(279+W_OFSY)      ; WAIT for line 280-1
                inc     h
                ld      e,0                     ; Y-offset = 0
keepScanline
                cp      $FF&(315+W_OFSY)        ; when WAIT == 316-1 => whole code is done
                jr      nz,videoMode80x42_CopperSetupL1    ; add further code
                ld      a,h
                rra
                jr      nc,videoMode80x42_CopperSetupL1    ; CF=0 = not 316-1
                ; add HALT at the end of copper code
                ld      a,$FF
                out     (c),a
                out     (c),a

                ; start up the copper thing
                nextreg $62, %11000000          ; COPPER_CONTROL_HI ; reset+start, reset on every frame [0,0]
DoneVideo:
                ret

; somewhere in top border (around H-line 310..319 - 32) set Y-offset to 0
; Start with ZX48 50Hz timing => 311 is last top-border line
; Sprite.y  Copper.line48   Yofs    tilemapLine     tm.base
;   0       280             0       0               $40
;   6       286             2       1               $40
;  12       292             4       2               $40
;  18       298             6       3               $40
;  24       304             8       4               $40
;  30       310            10       5               $40
;  36         4            12       6               $40
;  42        10            14       7               $40
; ...
; 186       154            62      31               $40
; 192       160      192= -64      32 (+16 from Y)  $4A
; ...
; 246       214      210= -46      41               $4A
; 252       220      212= -44      42 (+16 from Y)  $4A     ; only 4px visible
; 258       226           -42      43               $4A     ; fully outside view
; 264       232           -40      44               $4A

;;----------------------------------------------------------------------------------------------------------------------
;; Palette control
; palette slots:
; 0 - white on black                1 - black on white (inverse 0)
; 2 - bright white on black         3 - black on bright white (inverse 2)
; 4 - light blue on black           5 - light green on black
; 6 - light cyan on black           7 - light yellow on black
; 8 - white on blue (+8 sel)        9 - bright white on red (cursor)
; A - bright white on blue (+8 sel) B - white on dark grey (non-inverse menu/status/etc)
; C - light blue on blue (+8 sel)   D - light green on blue (+8 sel)
; E - light cyan on blue (+8 sel)   F - light yellow on blue (+8 sel)

PalBaseData:
                db  %000'000'00,0       ; black (paper)
                db  %001'001'00,1       ; dark grey (25% ink)
                db  %011'011'01,1       ; light grey (75% ink)
                db  %101'101'10,1       ; white (full ink)
                db  %001'001'11,0       ; blue
                db  %110'001'00,1       ; red
                db  %110'001'11,0       ; magenda
                db  %000'101'01,0       ; dark green
                db  %000'110'11,0       ; cyan
                db  %111'110'00,1       ; yellow
                db  %001'011'10,1       ; light blue
                db  %101'011'00,1       ; brown
                db  %101'110'00,1       ; light green
                db  %011'111'11,1       ; light cyan
                db  %111'111'01,1       ; light yellow
                db  %111'000'11,1       ; transparent E3
PalSlotData:
        ; slot 1: black on white (inverse slot 0)
                db  %101'101'10,1       ; white (paper)
                db  %011'011'01,1       ; light grey (25% ink)
                db  %001'001'00,1       ; dark grey (75% ink)
                db  %000'000'00,0       ; black (full ink)
        ; slot 2: bright white on black
                db  %000'000'00,0       ; black (paper)
                db  %010'010'01,0       ; dark grey (25% ink)
                db  %101'101'10,1       ; light grey (75% ink)
                db  %111'111'11,1       ; bright white (full ink)
        ; slot 3: black on bright white (inverse slot 2)
                db  %111'111'11,1       ; bright white (paper)
                db  %101'101'10,1       ; light grey (25% ink)
                db  %010'010'01,0       ; dark grey (75% ink)
                db  %000'000'00,0       ; black (full ink)
        ; slot 4: light blue on black
                db  %000'000'00,0       ; black (paper)
                db  %000'000'00,1       ; (25% ink)
                db  %000'010'10,0       ; (75% ink)
                db  %001'011'10,1       ; light blue (full ink)
        ; slot 5: light green on black
                db  %000'000'00,0       ; black (paper)
                db  %001'010'00,0       ; (25% ink)
                db  %100'101'00,0       ; (75% ink)
                db  %101'110'00,1       ; light green (full ink)
        ; slot 6: bright cyan on black
                db  %000'000'00,0       ; black (paper)
                db  %000'010'01,0       ; (25% ink)
                db  %001'101'10,1       ; (75% ink)
                db  %011'111'11,1       ; light cyan (full ink)
        ; slot 7: bright yellow on black
                db  %000'000'00,0       ; black (paper)
                db  %010'010'00,0       ; (25% ink)
                db  %101'101'00,1       ; (75% ink)
                db  %111'111'01,1       ; light yellow (full ink)
        ; slot 8: white on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %001'011'01,1       ; dark grey (25% ink)
                db  %011'011'01,1       ; light grey (75% ink)
                db  %101'101'10,1       ; white (full ink)
        ; slot 9: bright white on red (cursor)
                db  %111'000'00,0       ; red (paper)
                db  %111'010'01,0       ; (25% ink)
                db  %111'101'10,1       ; (75% ink)
                db  %111'111'11,1       ; white (full ink)
        ; slot A: bright white on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %010'010'01,1       ; dark grey (25% ink)
                db  %101'111'11,1       ; light grey (75% ink)
                db  %111'111'11,1       ; bright white (full ink)
        ; slot B: white on dark grey (non-inverse menu/status/etc)
                db  %010'010'01,0       ; dark grey (paper)
                db  %011'011'01,1       ; dark grey (33% ink)
                db  %101'101'10,1       ; light grey (75% ink)
                db  %111'111'11,1       ; bright white (full ink)
        ; slot C: light blue on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %000'010'01,1       ; (25% ink)
                db  %000'010'10,1       ; (75% ink)
                db  %001'011'10,1       ; light blue (full ink)
        ; slot D: light green on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %001'010'01,0       ; (25% ink)
                db  %100'101'00,1       ; (75% ink)
                db  %101'110'00,1       ; light green (full ink)
        ; slot E: light cyan on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %000'010'10,0       ; (25% ink)
                db  %001'101'10,1       ; (75% ink)
                db  %011'111'11,1       ; light cyan (full ink)
        ; slot F: light yellow on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %010'010'01,0       ; (25% ink)
                db  %101'101'00,1       ; (75% ink)
                db  %111'111'01,1       ; light yellow (full ink)

SetFullTilemapPalette:
                nextreg $43,%00110000   ; Set tilemap palette0
                nextreg $40,0           ; reset index
                ld      c,16            ; do 16x16 identical palettes first
.SlotLoop:      ld      b,32
                ld      hl,PalBaseData
.ColourLoop:    ld      a,(hl)
                inc     hl
                nextreg $44,a
                djnz    .ColourLoop
                dec     c
                jr      nz,.SlotLoop
                ; patch each slot (1..F) with specialized letter colours
                ld      hl,PalSlotData
                ld      a,16
.SlotPatchLoop: ld      b,8
                ld      c,a             ; preserve index in C
                nextreg $40,a           ; set index for slot
.ColourLoop2:   ld      a,(hl)
                inc     hl
                nextreg $44,a
                djnz    .ColourLoop2
                ld      a,c
                add     a,16
                jr      nz,.SlotPatchLoop
                ret

;;----------------------------------------------------------------------------------------------------------------------
;; Utitlies

CalcTileAddress:
        ; Input:
        ;       B = Y coord (0-31)
        ;       C = X coord (0-79)
        ; Output:
        ;       HL = Tile address
                push    bc
                push    de
                ld      e,b
                ld      d,80
                mul                 ; DE = 80Y
                ex      de,hl       ; HL = 80Y
                pop     de
                ld      b,$20       ; BC = tilemap base address $4000/2 + X coord
                add     hl,bc
                add     hl,hl       ; 2 bytes per tilemap cell
                pop     bc
                ret

;;----------------------------------------------------------------------------------------------------------------------
;; Low-level printing

Print:
        ; Input
        ;       B = Y coord (0-31)
        ;       C = X coord (0-79)
        ;       DE = string
        ;       A = colour (0-15)
        ; Output:
        ;       DE = points after string
        ; Uses:
        ;       BC, HL, DE, A

        ; Calculate tilemap address
                call    CalcTileAddress
                swapnib
                ld      c,a
                jr      .loopEntry
.l1             ld      (hl),a      ; Write out character
                inc     hl
                ld      (hl),c      ; Write out attribute
                inc     hl
.loopEntry      ld      a,(de)      ; Read next string character
                inc     de
                and     a
                jr      nz,.l1
                ret

PrintChar:
        ; Input:
        ;       B = Y coord (0-31)
        ;       C = X coord (0-79)
        ;       D = Colour (0-15)
        ;       E = character
        ; Output:
        ;       HL = Tilemap address of following position
        ; Uses:
        ;       A
                call    CalcTileAddress
                ld      (hl),e
                inc     hl
                ld      a,d
                swapnib
                ld      (hl),a
                inc     hl
                ret

AdvancePos:
        ; Advances position to next position on screen.  This will wrap to next line or back to the top of the screen.
        ; Input:
        ;       B = Y coord (0-31)
        ;       C = X coord (0-79)
        ; Output:
        ;       BC = next position XY.
        ; Uses:
        ;       A
        ;
                inc     c
                ld      a,80
                sub     c
                ret     nz
                ld      c,a
                inc     b
                ld      a,32
                sub     b
                ret     nz
                ld      b,a
                ret

WriteSpace:
        ; Draw a rectangular area of spaces
        ; Input
        ;       B = Y coord (0-31) of start
        ;       C = X coord (0-79) of start
        ;       D = height
        ;       E = width
        ;       A = colour (0-15)
        ; Uses:
        ;       HL, BC, DE, A
                call    CalcTileAddress     ; HL = start corner
                swapnib
                ld      c,a                 ; C = colour
                ld      a,80
                sub     e
                add     a,a                 ; A = 160 - 2*width = deltaHL
.row            ld      b,e                 ; reset width counter
.col            ld      (hl),' '            ; Write space
                inc     hl
                ld      (hl),c              ; Write colour
                inc     hl
                djnz    .col
                add     hl,a                ; HL = next row
                dec     d
                jr      nz,.row
                ret

ClearScreen:
        ; Clear screen (write spaces in colour 0 everywhere)
        ; Uses:
        ;       BC, HL, A
                ld      bc,80*43
                ld      hl,$4000
.l1             ld      (hl),' '
                inc     hl
                ld      (hl),0
                inc     hl
                dec     bc
                ld      a,b
                or      c
                jr      nz,.l1
                ret

