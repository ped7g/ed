;;----------------------------------------------------------------------------------------
;; Video-mode low level routines, initializing tilemode and copper, printing single char
;;----------------------------------------------------------------------------------------
;; # List of functions:
;; InitVideo                    - set up tilemode 80x42 (except copper), 28MHz, ...
;; DetectMode                   - returns video.MODE_* value (current display mode)
;; CopperNeedsReinit            - ZF=0 when copper needs reinit (mode change detected)
;; GetModeConfigData            - converts video.MODE_* value to address of mode data
;; GetInvisibleScanlines_byMode - get current mode limits
;; GetInvisibleScanlines_byIX   - get current mode limits
;; CopperReinit                 - generate copper code (when map-config or mode changes)
;; ClearScreen                  - sets whole virtual map ($4000..font) to ' ' in color 0
;; SetFullTilemapPalette        - (internal) setup tilemode palette
;; CalcTileAddress              - memory address of particular single character
;; CalcLineAddress              - memory address of particular row (in virtual map)
;; WriteSpace                   - fills "rectangle" in virtual map with ' ' in color 0
;; Print                        - print C-string
;; PrintChar                    - print single character
;; AdvancePos                   - advance BC coordinates to next position in virtual map
;;----------------------------------------------------------------------------------------

    ; default values for configuration-defines
    IFNDEF VIDEO_FONT_ADR
        DEFINE VIDEO_FONT_ADR   $6000       ; must fit into $4000..$7FFF region
        ; this also limits the tile-map region, which is always $4000 to VIDEO_FONT_ADR
        ; make sure all tiles fit into the Bank5 region (i.e. $7000 max for 128 tiles)
    ENDIF

    MACRO negR16toR16 fromR16?, toR16?
        ; 6B 24T, uses A
        xor     a
        sub     low fromR16?
        ld      low toR16?,a    ; low = 0 - low
        sbc     a,a
        sub     high fromR16?
        ld      high toR16?,a   ; high = 0 - high - borrow
    ENDM

    MODULE video

FONT_ADR            = VIDEO_FONT_ADR                ; convert DEFINE to regular symbol

VIRTUAL_ROWS        = (VIDEO_FONT_ADR - $4000)/160  ; 51 for font=$6000, 76 for $7000

    STRUCT SDisplayMap
; Account for the current "fully visible 6px rows" from GetInvisibleScanlines, creating
; layout which fits on screen (if the layout is higher, the generator should survive it,
; but it isn't recommended situation, the layout should be valid for current video mode).
; The invisible top scanlines are skipped by default, "skipScanlines" is in visible range
; the xOffset must be valid 0..79 value only, otherwise illegal values will be sent to
; NextReg $2F (will work on core 3.0 as expected, but may stop working in newer cores).
rows                BYTE    0   ; number of rows (0 = end of list, 1..43)
tilemapY            BYTE    0   ; map 0..101 (but some region is font data)
    ; so 8kiB reserved for map ($4000..$5FFF) is then Y: 0..50 (font at $6000)
    ; 12kiB reserved for map ($4000..$6FFF) is Y: 0..75 range (font at $7000)
    ; the particular line address in map is: $4000 + tilemapY * 160
skipScanlines       BYTE    0   ; number of scanlines to skip (with tilemode off)
xOffset             BYTE    0   ; tile number to start line at (wraps around) 0..79 only!
    ENDS

HORIZONTAL_COMPARE  EQU         39  ; after the right border is finished in all modes

MODE_HDMI_50        EQU         0
MODE_ZX48_50        EQU         1
MODE_ZX128_50       EQU         2
MODE_PENTAGON_50    EQU         3
MODE_HDMI_60        EQU         4
MODE_ZX48_60        EQU         5
MODE_ZX128_60       EQU         6
MODE_COUNT          EQU         7   ; pentagon at 60Hz is invalid combination

    STRUCT SDisplayCfg
modeNumber          BYTE            ; to have it accessible even if input argument is just IX pointer
startScanline       WORD            ; 33 scanlines ahead of pixel area (in HDMI 60Hz way into blank area)
invisibleTop        BYTE        0   ; how many top lines are in invisible range
invisibleBottom     BYTE        0   ; how many bottom lines are in invisible range
    ENDS

;; core 2.00 scanlines table (needs refresh for 60Hz modes and core 3.0, it's different)
; hdmi 50 | hdmi 60 | zx48 50 | zx48 60 | zx128 50 | zx128 60 | pentagon
; 272-311 | 245-261 | 263-311 | 239-261 | 263-310  | 239-260  | 255-319  ; top border (visible)
;  0   40 | 15   17 |  0   49 |  9   23 |  0   48  | 10   22  |  0   64  ; top tilemode invisible/visible
;  0   40 | 12   20 |  0   57 |  0   33 |  0   57  |  0   33  |  0   49  ; bottom scanlines invisible/visible
;     279 |     229 |     279 |     229 |     278  |     228  |     287  ; line at -33 (ahead of first tilemode line)

DisplayCfgTab:
.hdmi_50            SDisplayCfg MODE_HDMI_50,       279             ; => 42 rows +4px
.zx48_50            SDisplayCfg MODE_ZX48_50,       279
.zx128_50           SDisplayCfg MODE_ZX128_50,      278
.pentagon           SDisplayCfg MODE_PENTAGON_50,   287
; FIXME - refresh 60Hz mode configurations for core 3.0
.hdmi_60            SDisplayCfg MODE_HDMI_60,       229, 15, 12     ; => 38 rows +1px
.zx48_60            SDisplayCfg MODE_ZX48_60,       229,  9         ; => 41 rows +1px
.zx128_60           SDisplayCfg MODE_ZX128_60,      228, 10         ; => 41 rows +0px
; pentagon 60Hz is not an option (values as ZX48)
.invalid            SDisplayCfg MODE_COUNT,         229,  9

;;----------------------------------------------------------------------------------------
;; Init all video related settings to default state for 80x42 tilemode (has to be called
;; at least once before the app will enter main loop).
;; In main loop the CopperReinit is enough to call, when the need does arise - reported
;; by CopperNeedsReinit or by change in layout done by app itself.
;;
;; Settings:
;;   black border, ULA disabled, 28MHz turbo selected, keys F8+F3+Multiface enabled
;;   4bit tilemode 80x32 with attribute bytes, tiles base to VIDEO_FONT_ADR
;;   resets tilemode clip window (to full 640x256), sets palette
;;   clears tilemap (from $4000 to VIDEO_FONT_ADR)

InitVideo:
                xor     a
                out     (254),a                 ; black border

                call    SetFullTilemapPalette   ; setup tilemode palette
                call    ClearScreen             ; reset the $4000..VIDEO_FONT_ADR region
                ; enable F8, F3 and Multiface
                ld      a,$06
                call    ReadNextReg
                or      %1010'1000
                nextreg $06,a
                ; set other NextRegs to default settings of 80x42 tilemode component
                nextreg $07,3                   ; Set speed to 28Mhz
                nextreg $6b,%11000001           ; Tilemap control
                    ;*; +enable+80col-noAttr-palNum-textMode-r-512tile+forceTmOverUla
                ; $6E (TILEMAP_BASE_ADR_NR_6E) is not set up, because copper code does it
                ; The "tilemapY" value in config is like 0..101, into map starting @ $4000
                ; But the tiles def (font) must also reside somewhere, use tilemapY around
                nextreg $6f,high VIDEO_FONT_ADR ; Tiles base offset
                nextreg $4c,$0f                 ; Transparency colour (last item)
                nextreg $68,%10000000           ; Disable ULA output
                    ;*; +disableUla-blending-r-r-r-r-r-stencil
                nextreg $1C,$08                 ; reset tilemap clip window index to 0
                nextreg $1B,0                   ; reset clip window to 640x256
                nextreg $1B,159
                nextreg $1B,0
                nextreg $1B,255
                ; make sure the need of copper init is signalled
                ld      a,video.MODE_COUNT
                ld      (video.CopperNeedsReinit.CurrentMode),a
                ret

;;----------------------------------------------------------------------------------------
;; Detect current video mode:
;; 0, 1, 2, 3 = HDMI, ZX48, ZX128, Pentagon (all 50Hz), add +4 for 60Hz modes
;; (Pentagon 60Hz is not a valid mode => value 7 shouldn't be returned in A)

DetectMode:
        ; Output:
        ;       A = current mode 0..6
        ; Uses:
        ;       B, side effect: selects NextReg $11 or $03 on I/O port

                ;; read current configuration from NextRegs and convert it to 0..6 value
                ; read 50Hz/60Hz info
                ld      a,$05 ; PERIPHERAL_1_NR_05
                call    ReadNextReg
                and     $04             ; bit 2 = 50Hz/60Hz configuration
                ld      b,a             ; remember the 50/60 as +0/+4 value in B
                ; read HDMI vs VGA info
                ld      a,$11 ; VIDEO_TIMING_NR_11
                call    ReadNextReg
                inc     a               ; HDMI is value %111 in bits 2-0 -> zero it
                and     $07
                jr      z,.hdmiDetected
                ; if VGA mode, read particular zx48/zx128/pentagon setting
                ld      a,$03
                call    ReadNextReg
                ; a = bits 6-4: %00x zx48, %01x zx128, %100 pentagon
                swapnib a
                rra
                inc     a
                and     $03             ; A = 1/2/3 for zx48/zx128/pentagon
.hdmiDetected:  add     a,b             ; add 50/60Hz value to final result
                ret

;;----------------------------------------------------------------------------------------
;; Detect current video mode, and check if the copper code has to be reprogrammed

CopperNeedsReinit:
        ; Output:
        ;       A = current mode 0..6
        ;       B = copper-code mode (0..7) (7 = no copper yet)
        ;       ZF=0 => copper needs reprogramming (ZF=1 copper code is valid)
        ; Uses:
        ;       side effect: selects NextReg $11 or $03 on I/O port

                call    DetectMode
                ;; compare with previously stored value (by copper code generator)
.CurrentMode = $+1                      ; self-modify code used as value storage
                ld      b,MODE_COUNT    ; last copper code programmed for mode X
                cp      b               ; set ZF (ZF=1 => copper code is still valid)
                ret

;;----------------------------------------------------------------------------------------
;; Set IX to point to the mode-config data

GetModeConfigData:
        ; Input:
        ;       A = mode number (0..6 like in DetectMode)
        ; Output:
        ;       IX = pointer to SDisplayCfg structure
        ;       A = if (valid) original-value else MODE_COUNT

                cp      MODE_COUNT
                jr      c,.validMode
                ld      a,MODE_COUNT    ; clamp the value and return IX=DisplayCfgTab.invalid
.validMode:     ld      ix,DisplayCfgTab
                push    de
                ld      e,a
                ld      d,SDisplayCfg
                mul     de
                add     ix,de
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Get "invisible" (outside of screen) top/bottom scanlines for desired video mode.
;; You should always skip at least this many when configuring new screen-layout.
;; (configuring it with 0 to skip will not crash or anything, but some tile rows will
;; be not displayed)

GetInvisibleScanlines_byMode:
        ; Input:
        ;       A = mode to read values for (0..6 like in DetectMode)
        ; Output:
        ;       D = invisible top lines
        ;       E = invisible top lines
        ;       B = fully visible 6px heigh rows
        ;       A = remaining visible scanlines after last full row (0..5)
        ;       IX = pointer to SDisplayCfg structure

                call    GetModeConfigData

GetInvisibleScanlines_byIX:
        ; Input:
        ;       IX = pointer to SDisplayCfg structure
        ; Output:
        ;       D = invisible top lines
        ;       E = invisible top lines
        ;       B = fully visible 6px heigh rows
        ;       A = remaining visible scanlines after last full row (0..5)

                ld      d,(ix+SDisplayCfg.invisibleTop)
                ld      e,(ix+SDisplayCfg.invisibleBottom)
                ; calculate fully visible 6px heigh tile-rows (258px = 43 rows)
                ld      b,42            ; fully visible rows init (-1 already done)
                ld      a,-2            ; 640x256 tile-mode has 2px invisible (42.66)
                sub d : sub e           ; A = -(total invisible scanlines)
.calcVisibleRows:
                add     a,6
                ret     c               ; B = full 6px rows, A = extra scanlines
                djnz    .calcVisibleRows
                ; can't reach this point ever

;;----------------------------------------------------------------------------------------
;; Re-programs the Copper for current video mode with new display-map configuration

CopperReinit:
        ; Input:
        ;       DE = pointer to SDisplayMap array (terminating item has `rows == 0`)
        ;       IX = pointer to SDisplayCfg structure
        ; Output:
        ;       Copper is reprogrammed and started (in %01 mode, wrap-around infinite run)
        ; Uses:
        ;       AF, BC, DE, HL, IX
        ;       side effect: selects NextReg $60 on I/O port
                ; remember which mode will be inited now
                ld      a,(ix + SDisplayCfg.modeNumber)
                ld      (CopperNeedsReinit.CurrentMode),a
                ; init the copper code generator variables
                ld      hl,(ix + SDisplayCfg.startScanline) ; ok ; HL = first scanline
                add     hl,$8000 | (HORIZONTAL_COMPARE<<9)  ; turn it into copper WAIT instruction
                ld      a,l
                add     a,32            ; last valid WAIT (+1 is first overflown)
                ld      (.checkWscanMaxLo),a
                negR16toR16 hl,bc       ; BC = constant to convert the WAIT into scanline
                ld      (.Lscan2Wscan1),hl  ; scanline -> WAIT W-line
                ld      (.Wscan2Lscan1),bc  ; WAIT W-line -> scanline
                ld      (.Wscan2Lscan2),bc  ; WAIT W-line -> scanline
                ; advance the first wait by invisible top lines
                ld      a,(ix + SDisplayCfg.invisibleTop)
                add     hl,a
                ld      ix,de           ; ok ; IX = Display map array
                ;; IX = SDisplayMap array, HL = W-line
                ; set up Copper control to "stop" + index 0
                nextreg $61,0           ; COPPER_CONTROL_LO_NR_61
                nextreg $62,0           ; COPPER_CONTROL_HI_NR_62
                ; set COPPER_DATA for write by OUT (c)
                ld      bc,$243B        ; TBBLUE_REGISTER_SELECT_P_243B
                ld      a,$60           ; COPPER_DATA_NR_60
                out     (c),a           ; select copper data register
                inc     b               ; BC = TBBLUE_REGISTER_ACCESS_P_253B
                ;; IX = SDisplayMap array, HL = W-line, BC = $253B I/O port
                out     (c),h           ; initial WAIT
                out     (c),l
                ; start copper before the full code is generated to maximize chance
                ; to catch "this" frame, if the generator was called early after interrupt
                nextreg $62,%01'000'000 ; COPPER_CONTROL_HI_NR_62 ; restart copper from 0
                call    .DisplayMapLoopEntry
                ; add tilemap OFF after last display map (WAIT is already inserted)
                ld      e,$6B           ; TILEMAP_CONTROL_NR_6B
                out     (c),e
                out     (c),0           ; switch OFF tilemap
                ; fill up remaining copper code with NOOPs - calculate amount of NOOPs
                ld      a,$62           ; COPPER_CONTROL_HI_NR_62
                call    ReadNextReg     ; read it for calculating count + starting copper
                ld      d,a
                ld      a,$61           ; COPPER_CONTROL_LO_NR_61
                call    ReadNextReg
                ld      e,a             ; DE = current copper index (11 bit value 0..2047 + copper mode)
                ld      b,3
                bsrl    de,b            ; E = current copper index>>3 (8bit 0..255)
                or      %1111'1000
                ld      d,a             ; D = low 3 bits of copper index, top bits set (-8..-1 value)
                ; fill up remaining copper code with NOOPs - actual fill
                ld      bc,$243B        ; TBBLUE_REGISTER_SELECT_P_243B
                ld      a,$60           ; COPPER_DATA_NR_60
                out     (c),a           ; select copper data register
                inc     b               ; BC = TBBLUE_REGISTER_ACCESS_P_253B
                xor     a
                ; align the 3-bit small value
.NoopFillLoop:  out     (c),a
                inc     d
                jp      nz,.NoopFillLoop
                ; burst the rest in 8x unrolled outs
                jp      .NoopFillLoop2Entry ; do `inc e` first
.NoopFillLoop2: .8 out (c),a            ; unroll for better performance
.NoopFillLoop2Entry:
                inc     e
                jp      nz,.NoopFillLoop2
                ret

.DisplayMapLoop:
                .(SDisplayMap) inc ix   ; ++displayMapPtr
.DisplayMapLoopEntry:
                ld      a,(ix + SDisplayMap.rows)
                or      a
                ret     z
                ;; create "skip scanlines" in copper code (switch OFF + ON tilemode)
                ld      a,(ix + SDisplayMap.skipScanlines)
                or      a
                jr      z,.noSkipScanline
                ld      e,$6B           ; TILEMAP_CONTROL_NR_6B
                out     (c),e
                out     (c),0           ; switch OFF tilemap
                add     hl,a            ; Wline += skipLines
                call    .HandleOnTopBorderLeave
                out     (c),h           ; WAIT
                out     (c),l
.noSkipScanline:
                ;; IX = SDisplayMap array, HL = W-line, BC = $253B I/O port
                ld      de,$6B00 + %11000001    ; TILEMAP_CONTROL_NR_6B = %11000001 (ON)
                out     (c),d
                out     (c),e
                ;; read config: xOffset -> setup X offset
                ld      e,(ix + SDisplayMap.xOffset)
                ld      d,8
                mul     de              ; DE = x offset 0..639 (from valid 0..79 input)
                ld      a,$2F
                out     (c),a           ; TILEMAP_XOFFSET_MSB_NR_2F = high xOffset*8
                out     (c),d
                inc     a
                out     (c),a           ; TILEMAP_XOFFSET_LSB_NR_30 = low xOffset*8
                out     (c),e
                ;; read config: tilemapY, rows
                ld      a,(ix + SDisplayMap.rows)
                ld      e,(ix + SDisplayMap.tilemapY)
                ld      d,8
                mul     de              ; DE = tilemapY*8
                ;; IX = SDisplayMap, HL = W-line, BC = $253B I/O port, DE = tilemapY*8, A = rows
.Row6pxLoop:
                ; set up base address of tilemap = (tilemapY/32 * (high 32*160))
                push    de              ; D = tilemapY/32 (because DE = tilemapY*8)
                ld      e,high (32*160)
                mul     de              ; DE = base address of tile map
                ld      d,$6E           ; TILEMAP_BASE_ADR_NR_6E
                out     (c),d
                out     (c),e
                pop     de
                ; calculate y offset = tilemapY*8 - scanline
.Wscan2Lscan1:  equ     $+2
                add     hl,$DEAD        ; self-modify, change W-scanline to scanline
                push    de
                ex      de,hl
                sub     hl,de           ; ok
                ex      de,hl
                ld      d,$31           ; TILEMAP_YOFFSET_NR_31 = yOfs
                out     (c),d
                out     (c),e
                pop     de
                ; ++tilemapY
                add     de,8
                ; scanline += 6
                add     hl,6
                bit     0,h
                push    af              ; NZ = (256 <= scanline) => remember this one
.Lscan2Wscan1:  equ     $+2
                add     hl,$DEAD        ; self-modify, change scanline to W-scanline
                ; handle crossing of top-border to pixel-area
                call    .HandleOnTopBorderLeave
                out     (c),h           ; WAIT
                out     (c),l
                pop     af              ; restore rows counter and (256 <= scanline test)
                ret     nz              ; 256 <= scanline, gone outside of screen
                dec     a
                jp      nz,.Row6pxLoop
                jp      .DisplayMapLoop

.HandleOnTopBorderLeave:
        ; Input:
        ;       HL = WAIT W-scan overflow outside of top area
        ; Output:
        ;       HL = fixed WAIT W-scan
        ; Uses:
        ;       AF
                ld      a,$80 | (HORIZONTAL_COMPARE<<1) | 1
                cp      h
                ret     nz              ; high byte must be equal when W overflows
.checkWscanMaxLo: equ $ + 1
                ld      a,$F0
                cp      l
                ret     nc              ; W is still valid
                ; reconfigure W to WAIT(HORIZONTAL_COMPARE, scanline - 32 - 1)
.Wscan2Lscan2:  equ     $+2
                add     hl,$DEAD        ; self-modify, change W-line to scanline
                ; reconfigure self-modify adders
                push    de
                ld      de,-($8000 | ((HORIZONTAL_COMPARE-1)<<9) | (-33&$1FF))
                ld      (.Wscan2Lscan1),de  ; scanline -> W-line
                ld      de,$8000 | ((HORIZONTAL_COMPARE-1)<<9) | (-33&$1FF)
                ld      (.Lscan2Wscan1),de  ; scanline -> W-line
                add     hl,de           ; HL = WAIT(HORIZONTAL_COMPARE, scanline - 32 - 1)
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Clear screen (write spaces in colour 0 everywhere in $4000..VIDEO_FONT_ADR region)

ClearScreen:
        ; Uses:
        ;       BC, HL, A
                push    de
                ld      bc,VIDEO_FONT_ADR-$4002
                ld      hl,$4001
                ld      de,hl               ; fake de = hl
                inc     de                  ; de = $4002
                ldd     (hl),0              ; fake [hl--] = 0
                ld      (hl),' '
                ldir
                pop     de
                ret

;;----------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------
;; Setup palette of tilemode

SetFullTilemapPalette:
        ; Uses:
        ;       A, BC, HL, NextRegs [$40, $44]
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

;;----------------------------------------------------------------------------------------
;; Calculate address of coordinate [x=C 0..79,y=B 0..101] into the tilemap = HL

CalcTileAddress:
        ; Input:
        ;       B = Y coord (0-101)
        ;       C = X coord (0-79)
        ; Output:
        ;       HL = Tile address (base is $4000)
                push    de
                ld      h,$20
                ld      l,c         ; HL = tilemap base address $4000/2 + X coord
                ld      d,80
                ld      e,b
                mul     de
                add     hl,de
                add     hl,hl
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Calculate address of line B (0..101) into the tilemap = HL

CalcLineAddress:
        ; Input:
        ;       B = Y coord (0-101)
        ; Output:
        ;       HL = Tile address (base is $4000)
                push    de
                ld      hl,$4000
                ld      d,160
                ld      e,b
                mul     de
                add     hl,de
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Draw a rectangular area of spaces

WriteSpace:
        ; Input
        ;       B = Y coord (0-101) of start
        ;       C = X coord (0-79) of start
        ;       D = height (1-102)
        ;       E = width (1-80)
        ;       A = colour (0-15)
        ; Uses:
        ;       HL, DE, A
                push    bc
                call    CalcTileAddress     ; HL = start corner
                swapnib
                ld      c,a                 ; C = colour
                ld      a,80
                sub     e
                add     a,a                 ; A = 160 - 2*width = deltaHL
.row            ld      b,e                 ; reset width counter
.col            ld      (hl),' '            ; Write space
                inc     l                   ; only L: even -> odd value, can't overflow
                ld      (hl),c              ; Write colour
                inc     hl
                djnz    .col
                add     hl,a                ; HL = next row
                dec     d
                jr      nz,.row
                pop     bc
                ret

;;----------------------------------------------------------------------------------------
;; Utilities

;;----------------------------------------------------------------------------------------
;; Low-level printing

Print:
        ; Input
        ;       B = Y coord (0-101)
        ;       C = X coord (0-79)
        ;       DE = string (make sure the string will fit into virtual map!)
        ;       A = colour (0-15)
        ; Output:
        ;       DE = points after string
        ; Uses:
        ;       BC, HL, DE, A

                call    video.CalcTileAddress
                swapnib
                ld      c,a
                jr      .loopEntry
.l1             ld      (hl),a      ; Write out character
                inc     l           ; only L: even -> odd value, can't overflow to H
                ldi     (hl),c      ; fake [HL++] = C ; Write out attribute
.loopEntry      ldi     a,(de)      ; fake A = [DE++] ; Read next string character
                or      a
                jr      nz,.l1
                ret

PrintChar:
        ; Input:
        ;       B = Y coord (0-101)
        ;       C = X coord (0-79)
        ;       D = Colour (0-15)
        ;       E = character
        ; Output:
        ;       HL = Tilemap address of following position
        ; Uses:
        ;       A
                call    video.CalcTileAddress
                ld      (hl),e
                inc     l           ; only L: even -> odd value, can't overflow to H
                ld      a,d
                swapnib
                ldi     (hl),a      ; fake [HL++] = A ; Write out attribute
                ret

AdvancePos:
        ; Advances position to next position on screen. This will wrap to next line.
        ; When at the last row of virtual map (depends on VIDEO_FONT_ADR), wraps to Y=0
        ; Input:
        ;       B = Y coord (0-?) (0..50 font=$6000, 0..75 font=$7000)
        ;       C = X coord (0-79)
        ; Output:
        ;       BC = next position XY.
        ; Uses:
        ;       A
                inc     c
                ld      a,80
                sub     c
                ret     nz
                ld      c,a
                inc     b
                ld      a,VIRTUAL_ROWS
                sub     b
                ret     nz
                ld      b,a
                ret

    ENDMODULE

;;----------------------------------------------------------------------------------------
