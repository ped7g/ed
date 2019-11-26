;;----------------------------------------------------------------------------------------------------------------------
;; Next Editor
;;----------------------------------------------------------------------------------------------------------------------

IFDEF _SJASMPLUS

        OPT --zxnext=cspect         ; enable also Z80N instructions
        DEFINE message DISPLAY /D,  ; DISPLAY is mostly compatible with snasm MESSAGE
        DEFINE PC +($)              ; for current address the operator "$" has to be used
        DEVICE ZXSPECTRUM48         ; setup virtual device for SAVESNA functionality
        CSPECTMAP "ed.sna.map"

ELSE

;; snasm specific options and macros
opt     sna=Start:$BFFE
opt     zxnext
opt     zxnextreg

BREAK   macro
        dw      $01dd
        endm

EXIT    macro
        dw      $00dd
        endm

ENDIF

;;----------------------------------------------------------------------------------------------------------------------
;; Constants
;;

EOL             equ     $0d
EOF             equ     $1a
CMDBUFFER       equ     $bd

;;----------------------------------------------------------------------------------------------------------------------
;; Memory map

; $0000         ROM
; $2000         ROM
; $4000         Tilemap
; $6000         Tiles
; $8000         IM2 interrupt table (->$8181)
; $8101         code
; $bd00         Commandbuffer
; $be00         Keyboard circular buffer
; $bf00         Stack
; $c000         Data

;;----------------------------------------------------------------------------------------------------------------------
;; Font

        org     video.FONT_ADR

        include "../data/tilemap_font_8x6.i.s"
;         incbin  "../data/font.bin"

;;----------------------------------------------------------------------------------------------------------------------
;; Sample text

        org     $c000

        incbin  "../data/test.txt"
        db      EOF

textlen equ PC - $c000


;;----------------------------------------------------------------------------------------------------------------------
;; This ORGs at $7fff

        include "keyboard.s"

;;----------------------------------------------------------------------------------------------------------------------

MODE_NORMAL     equ     0
MODE_INSERT     equ     1
MODE_SELECT     equ     2
MODE_LINESELECT equ     3

;;----------------------------------------------------------------------------------------------------------------------
;; Start

Start:
                ld      sp,$c000
                call    Initialise
                call    InitKeys
                jp      Main

;;----------------------------------------------------------------------------------------------------------------------
;; Initialise
;; Initialise the screen and video modes

Initialise:
                call    video.InitVideo
                ret

;;----------------------------------------------------------------------------------------------------------------------
;; Modules

        include "utils.s"
        include "screen.s"
        include "display.s"
        include "cmdtable.s"
        include "state.s"

;;----------------------------------------------------------------------------------------------------------------------
;; Main
;; The main loop

Main:
                call    video.ClearScreen

;; TEST video.CalcTileAddress
;                 ld      de,$BEEF
;                 ld      a,$F0
;                 ld      bc,$0000    ; => $4000
;                 call    video.CalcTileAddress
;                 ld      bc,$0100    ; => $40A0
;                 call    video.CalcTileAddress
;                 ld      bc,$0001    ; => $4002
;                 call    video.CalcTileAddress
;                 ld      bc,$0a00    ; => $4640
;                 call    video.CalcTileAddress
;                 ld      bc,$000a    ; => $4014
;                 call    video.CalcTileAddress
;                 ld      bc,$4433    ; => $6ae6
;                 call    video.CalcTileAddress

;; TEST video.CalcLineAddress
;                 break
;                 ld      de,$BEEF
;                 ld      a,$F0
;                 ld      bc,$00F0    ; => $4000
;                 call    video.CalcLineAddress
;                 ld      b,$01       ; => $40A0
;                 call    video.CalcLineAddress
;                 ld      b,$0a       ; => $4640
;                 call    video.CalcLineAddress
;                 ld      b,$44       ; => $6a80
;                 call    video.CalcLineAddress

;; TEST video.GetInvisibleScanlines_byMode
;                 xor     a
; .TestGetInvisibleScanlines:
;                 push    af
;                 call    video.GetInvisibleScanlines_byMode
;                 break : nop : nop
;                 pop     af
;                 inc     a
;                 cp      video.MODE_COUNT+1
;                 jr      nz,.TestGetInvisibleScanlines

MainLoop:
                call    video.CopperNeedsReinit
                jr      z,.videoConfigIsOk
                call    video.GetInvisibleScanlines_byMode
        ;       D = invisible top lines
        ;       E = invisible top lines
        ;       B = fully visible 6px heigh rows
        ;       A = remaining visible scanlines after last full row (0..5)
        ;       IX = pointer to ModeCfg structure

.videoConfigIsOk:
                halt
                call    DisplayScreen

    DEFINE DEBUG_SHOW_FULL_TILESET
    IFDEF DEBUG_SHOW_FULL_TILESET
                ;;FIXME DEBUG
                ld      bc,$2300
                ld      de,$0624
                xor     a
                call    video.WriteSpace
                ld      hl,$4000+($24*160)+4
                call    .debugFullTileSet
                ld      hl,$4000+($24*160)+80

;                 call    .debugFullTileSet     ; use this instead of next code to see all chars in colour blocks
                ld      a,$40
                call    .debugLoopRows
                ld      a,$40
                call    .debugLoopRows
                xor     a
4:
                ld      hl,$4000+($24*160)+81
                ld      e,4
3:
                ld      c,4
2:
                ld      b,8
1:
                ld      (hl),a
                inc     hl
                inc     hl
                djnz    1B
                add     a,$10
                dec     c
                jr      nz,2B
                add     hl,160-$40
                dec     e
                jr      nz,3B

;                jr      $       ; comment this out to see colour blocks advancing
4:
                ld      b,204
1:
                ei
                halt
                push    bc

                ; FIXME extra delay
;                 push    bc
;                 ld      b,0
;                 .7 djnz    $
;                 pop     bc

;; FIXME test of new rewrite of copper code generator
                ; make third map "scroll" (by changing skipped scanlines
                ld      a,b
                srl     a
                ld      (.map3.skipScanlines),a
                ld      (.map2.tilemapY),a
                srl     a
                ld      (.map3.xOffset),a
                ; force re-init any way
;                 and     (1<<3)-1 : jr nz,100F     ; every n-th frame only
                ld      a,video.MODE_COUNT
                ld      (video.CopperNeedsReinit.CurrentMode),a
;                jr      100F
                call    video.CopperNeedsReinit
                jr      z,100F  ; no reinit needed
                call    video.GetInvisibleScanlines_byMode
                ld      de,.debugSDisMap
                call    video.CopperReinit
                jr      100F
.debugSDisMap:
;                 video.SDisplayMap { 42, 0, 2 }
;                 db      0
                video.SDisplayMap { 5, 0, 1 }
.map2:          video.SDisplayMap { 12, 0, 2 }
.map3:          video.SDisplayMap { 20, 0, 4 }
                db      0
100:
                pop     bc
                djnz    1B

                ;; DEBUG read nextreg $6c (default tilemode attribute)
                push    af
                ld      a,$6c
                call    ReadNextReg
                ld      (DebugValue),a
                call    DisplayDebugger
                pop     af

                ;; DEBUG
                push    af
                xor     a
301:            push    af
                call    video.GetInvisibleScanlines_byMode
                ld      (DebugValue),a
                call    DisplayDebugger
                pop     af
                inc     a
                cp      video.MODE_COUNT+1
                jr      c,301B
                pop     af

                add     a,$10   ; offset colour blocks
                ld      hl,$4000+($24*160)+5
                ld      e,4
3:
                ld      b,32
2:
                ld      (hl),a
                inc     hl
                inc     hl
                djnz    2B
                add     hl,160-$40
                dec     e
                jp      nz,3B
                jp      4B

.debugFullTileSet:
                xor     a
.debugLoopRows:
                ld      b,$20
.debugLoop32:
                ld      (hl),a
                inc     hl
                inc     hl
                inc     a
                djnz    .debugLoop32
                add     hl,160-$40
                jp      p,.debugLoopRows
                ret
    ENDIF

.l1             ld      a,(cursorY)
                ld      b,a
                ld      a,(cursorX)
                ld      c,a
                ld      a,2
                call    DisplayCursor
                call    DisplayDebugger

        ; Read keyboard and insert commands into the command buffer
                ld      hl,KFlags
                bit     0,(hl)
                jr      z,.l1           ; Still waiting for a key
                ld      a,(Key)
                res     0,(hl)          ; Consume key
                ld      e,a             ; E = key code
                ld      c,a             ; C = key code


ProcessKey:
                ld      hl,ModeTable
                ld      a,(mode)
                add     a,a
                add     hl,a
                add     hl,a
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a             ; HL = mode table to look up

                ld      a,e
                add     hl,a
                add     hl,a            ; HL = address of routine address

                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a             ; HL = Routine to run
                ld      b,CMDBUFFER
                ld      ix,CmdBufferState
                ld      a,c
                call    CallHL          ; Run command (with A = key code)

                call    FlushCommands   ; Interpret commands

                jp      MainLoop

;;----------------------------------------------------------------------------------------------------------------------
;; Command control

;;----------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------
;; COMMANDS
;;
;; The command routine job is to insert a command into the command buffer for later flushing.  Three registers will help
;; with that.  Firstly, B is already loaded with the CMDBUFFER value so that a call to BufferInsert will insert a value
;; into the correct buffer.  Secondly, A (and C) are loaded with the key-code.  Finally, IX is set to the correct buffer
;; state variables.
;;
;; Because of this, inserting BufferInsert into the command table will just insert the actual key-code into the command
;; buffer if they are the same.  For example, the key 'h' for cursor left is also 'h' as a command.
;;----------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------
;; Cursor commands

Cmd_CursorLeft:
                ld      c,'h'
                jp      BufferInsert

Cmd_CursorRight:
                ld      c,'l'
                jp      BufferInsert

Cmd_CursorUp:
                ld      c,'k'
                jp      BufferInsert

Cmd_CursorDown:
                ld      c,'j'
                jp      BufferInsert

Cmd_Home:
                ld      c,'^'
                jp      BufferInsert

Cmd_End:
                ld      c,'$'
                jp      BufferInsert

;;----------------------------------------------------------------------------------------------------------------------
;; Command interpreter

FlushCommands:
        ; Keep pulling commands off the buffer until done.  It's a simple state machine.
        ;
                ld      b,CMDBUFFER
                ld      ix,CmdBufferState
                call    BufferRead              ; A = next command
                ret     z

                sub     32
                ld      hl,MainCmdTable
                add     hl,a
                add     hl,a
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a                     ; HL = Address of command implementor
                call    CallHL

                jr      FlushCommands

;;----------------------------------------------------------------------------------------------------------------------

AdvanceReal:
        ; Input
        ;       HL = real address of buffer
        ;
                call    RealToVirt
                inc     hl
                call    VirtToReal
                ret

;;----------------------------------------------------------------------------------------------------------------------
;; Document access functions
;; All access to the document must be via these functions so they can be refactored later.  For example, I would like
;; to add virtual access and 24-bit file sizes.

Doc_FetchChar:
        ; Output:
        ;       A = character under cursor
                push    hl
                ld      hl,(pos)
                call    VirtToReal
                ld      a,(hl)
                pop     hl
                ret

Doc_AtStartDoc:
        ; Output:
        ;       ZF = 1 if at start of doc
        ; Uses:
        ;       A
        ;
                push    hl
                ld      hl,(pos)
                ld      a,h
                or      l
                pop     hl
                ret

Doc_AtEndDoc:
        ; Output:
        ;       ZF = 1 if at end of doc
        ;       A = current character
        ;
                call    Doc_FetchChar
                cp      EOF
                ret

Doc_LineOffset:
        ; Output:
        ;       HL = position in line
        ;       ZF = 1 if at start of line
        ;       CF = 0
        ;
                push    de
                ld      hl,(linepos)
                ex      de,hl           ; DE = start of line position
                and     a
                ld      hl,(pos)        ; HL = position in document
                sbc     hl,de
                pop     de
                ret

Doc_ToNextLine:
        ; Output:
        ;       BC = number of characters to end of line
        ;
                push    af
                push    de
                push    hl
                ld      hl,(pos)
                call    VirtToReal

                ld      bc,0
.l1             ld      a,(hl)          ; Get next character
                cp      EOF             ; End of file?
                jr      z,.done
                cp      EOL             ; End of line?
                jr      z,.done
                inc     bc
                call    AdvanceReal
                jr      .l1
.done           pop     hl
                pop     de
                pop     af
                ret

Doc_AtEndLine:
        ; Output:
        ;       ZF = 1 if at end of line
        ;       CF = 0
        ;       A = Current character under cursor
        ;
                call    Doc_FetchChar
                cp      EOL
                ret

Doc_MoveBack:
        ; Will not move past start of document
        ; Input:
        ;       DE = number of places to move
        ;
                ; Check the trivial case of not moving any places
                push    af
                ld      a,d
                or      e
                jr      nz,.not0        ; Return if DE == 0
                pop     af
                ret

                ; Check to see if moving back doesn't go past beginning of document.  If so,
                ; clamp it at the beginning.
.not0           push    de
                push    hl
                ld      hl,(pos)
                and     a
                sbc     hl,de
                jr      nc,.ok          ; Jump if there was room to move back that amount
                ld      hl,0
                ld      (linepos),hl
                jr      UpdatePos

                ; We can move back DE number of places
.ok             ld      hl,(pos)        ; HL = current position

.l1             dec     hl
                ld      (pos),hl
                call    Doc_FetchChar   ; Get the current character under pos
                cp      EOL             ; Reached end of line?
                jr      z,.eol          ; Yes, we need to update linepos and cursor

.cont           dec     de
                ld      a,d
                or      e
                jr      nz,.l1          ; Keep moving DE places
                jr      UpdatePos

.eol            ; We've passed a $0d character, and so we need to adjust linepos and cursor
                push    hl              ; Store current position

.l2             ld      a,h
                or      l               ; HL = beginning of document?
                jr      nz,.search

.update         ld      (linepos),hl    ; Update beginning of line position
                ld      hl,(cursorLine)
                dec     hl
                ld      (cursorLine),hl
                pop     hl
                jr      .cont           ; Keep moving back

.search         dec     hl
                ld      (pos),hl
                call    Doc_FetchChar
                cp      EOL
                jr      nz,.l2          ; Keep searching back
                inc     hl              ; Go back to beginning of next line
                jr      .update

                ; Update the position and clean up
UpdatePos:      ld      (pos),hl
                pop     hl
                pop     de
                pop     af
                ret

                
Doc_MoveForward:
        ; Move forward DE places in document.  Will not move past end of document.
        ; Input:
        ;       DE = number of places to move
        ;
                ; Check the trivial case of not moving any places
                push    af
                ld      a,d
                or      e
                jr      nz,.not0        ; Return if DE == 0
                pop     af
                ret

.not0           ; Lets try to move forward.
                push    de
                push    hl
                ld      hl,(pos)

.l1             ld      (pos),hl
                call    Doc_FetchChar
                cp      EOF             ; End of file before we even begin?
                jr      z,UpdatePos

                inc     hl              ; Move to next position
                cp      EOL             ; If EOL, update linepos and cursorLine
                jr      nz,.no_eol

                push    hl
                ld      (linepos),hl
                ld      hl,(cursorLine)
                inc     hl
                ld      (cursorLine),hl
                pop     hl

.no_eol         dec     de
                ld      a,d
                or      e
                jr      nz,.l1

                jr      UpdatePos

;;----------------------------------------------------------------------------------------------------------------------
;; CursorVisible
;; Manipulates top, dx, cursorX, cursorY to ensure cursor is on screen.  Only does something if the cursor is currently
;; off-screen

StartPoint      dw      0       ; Start range of screen on axis
EndPoint        dw      0       ; End range of screen on axis
CursorPoint     dw      0       ; Current cursor position

OutOffset       dw      0       ; Offset required to keep cursor on screen
OutCursor       dw      0       ; Cursor position from top of screen

ProcessAxis:
                ld      hl,(CursorPoint)
                ld      de,(StartPoint)
                call    Compare16               ; X < S?
                jr      c,.to_left
                ld      de,(EndPoint)
                call    Compare16               ; X < E?
                jr      c,.centre

                ; Here the cursor is past the endpoint
                ; HL = Cursor
                ld      de,(EndPoint)
                dec     de
                and     a
                sbc     hl,de                   ; HL = difference between cursor and end point
                ex      de,hl
                ld      hl,(StartPoint)
                add     hl,de
                ld      (OutOffset),hl          ; New offset

                ex      de,hl                   ; DE = offset
                ld      hl,(CursorPoint)        ; HL = cursor point
                and     a
                sbc     hl,de                   ; HL = relative cursor position
                ld      (OutCursor),hl
                ret

.to_left        ; Here the cursor is before the start point
                ld      (OutOffset),hl
                ld      hl,0
                ld      (OutCursor),hl
                ret

.centre         ; Everything is just fine
                ld      de,(StartPoint)
                and     a
                sbc     hl,de                   ; HL = position from left of screen
                ld      (OutCursor),hl
                ld      (OutOffset),de
                ret

CursorVisible:
                ; Remove cursor
                ld      bc,(cursorX)
                xor     a
                call    DisplayCursor

                ;;
                ;; X cursor
                ;;

                call    Doc_LineOffset          ; HL = offset into current line
                ld      (CursorPoint),hl
                ld      hl,(dx)
                ld      (StartPoint),hl
                ld      a,80
                add     hl,a
                ld      (EndPoint),hl
                call    ProcessAxis

                ld      hl,(OutOffset)
                ld      (dx),hl
                ld      hl,(OutCursor)
                ld      a,l
                ld      (cursorX),a

                ;;
                ;; Y Cursor
                ;;

                ld      hl,(cursorLine)
                ld      (CursorPoint),hl
                ld      hl,(topLine)
                ld      (StartPoint),hl
                ld      a,30
                add     hl,a
                ld      (EndPoint),hl
                call    ProcessAxis

                ; Scroll up or scroll down?
                ld      hl,(topLine)
                ld      de,(OutOffset)
                ld      (topLine),de
                and     a
                sbc     hl,de                   ; topLine < OutOffset
                jr      c,.scroll_down          ; Yes, need to scroll downwards from topLine to OutOffset
                jr      z,.no_scroll            ; No scrolling required

                ; topLine > OutOffset, which means we have to scroll upwards
                ; HL = number of lines to scroll
                ld      c,l
                ld      b,h
                ld      hl,(pos)
                push    hl                      ; Store position
                ld      hl,(linepos)
                push    hl
                ld      hl,(cursorLine)
                push    hl
                ld      hl,(top)
                ld      (pos),hl

.l1             ld      hl,(pos)
                dec     hl                      ; Move to end of previous line
                ld      (pos),hl
                call    Doc_LineOffset          ; HL = number of characters in line
                ex      de,hl
                call    Doc_MoveBack            ; Move to beginning of line
                dec     bc
                ld      a,b
                or      c
                jr      nz,.l1

.update_top     ld      hl,(pos)
                ld      (top),hl
                pop     hl
                ld      (cursorLine),hl
                pop     hl
                ld      (linepos),hl
                pop     hl
                ld      (pos),hl                ; Restore position
                jr      .no_scroll

.scroll_down    ; topLine < OutOffset, which means we have to scroll downwards
                ld      c,l
                ld      b,h

                ; Store current position, since we're going to move the cursor to the top of the
                ; screen to do the adjustments
                ld      hl,(pos)
                push    hl
                ld      hl,(linepos)
                push    hl
                ld      hl,(cursorLine)
                push    hl
                ld      hl,(top)
                ld      (pos),hl

.l2             push    bc
                call    Doc_ToNextLine          ; BC = length of line
                ld      e,c
                ld      d,b
                inc     de
                call    Doc_MoveForward         ; Move to beginning of next line
                pop     bc
                inc     bc
                ld      a,b
                or      c
                jr      nz,.l2
                jr      .update_top

.no_scroll:     ; topLine is now in the right place
                ld      hl,(OutCursor)
                inc     hl                      ; HL = Cursor Y position, +1 to skip title
                ld      a,l
                ld      (cursorY),a

                ;;
                ;; End
                ;;
                ld      hl,0
                ld      (Counter),hl            ; Ensure the cursor is visible
                ret

;;----------------------------------------------------------------------------------------------------------------------
;; Commands

MoveLeft:
                call    Doc_AtStartDoc
                jp      z,CursorVisible         ; At beginning of document

                ld      de,1
                call    Doc_MoveBack
                jp      CursorVisible

.at_edge:
                ;#todo
                ;Move cursor up and to the end
                ret

;;----------------------------------------------------------------------------------------------------------------------

MoveRight:
                call    Doc_FetchChar
                cp      EOF                     ; End of file?
                ret     z                       ; Yes, no cursor movement

                ld      de,1
                call    Doc_MoveForward
                jp      CursorVisible           ; Make it visible again

;;----------------------------------------------------------------------------------------------------------------------

LastX           dw      0

MoveUp:
                ;#todo - Make the cursor return to original horizontal position if possible
                call    Doc_LineOffset          ; HL = horizontal position
                ld      (LastX),hl              ; Store it

                ; Move to beginning of line
                ex      de,hl
                inc     de
                call    Doc_MoveBack            ; Move back to end of previous line
                call    Doc_AtStartDoc          ; At beginning of doc?
                jp      z, CursorVisible        ; Yes, move no more!

                call    Doc_LineOffset          ; HL = size of previous line
                ld      de,(LastX)
                and     a
                sbc     hl,de                   ; HL >= DE is good!  HL = distance to move back
                jr      c,.done

                ex      de,hl
                call    Doc_MoveBack
.done           jp      CursorVisible

;;----------------------------------------------------------------------------------------------------------------------

MoveDown:
                ;#todo - Make the cursor return to original horizontal position if possible
                call    Doc_LineOffset
                ld      (LastX),hl              ; Store the horizontal position
                call    Doc_ToNextLine
                ld      d,b
                ld      e,c
                call    Doc_MoveForward         ; Jump to the end of the line
                call    Doc_FetchChar           ; Is it an EOF?
                cp      EOF
                jp      z,CursorVisible         ; Yes, go no further

                ld      de,1
                call    Doc_MoveForward         ; Move to beginning of next line
                call    Doc_ToNextLine          ; BC = length of line
                ld      hl,(LastX)              ; HL = intended position
                ld      d,b
                ld      e,c                     ; DE = line length
                call    Max                     ; DE = actual position
                call    Doc_MoveForward

                jp      CursorVisible

;;----------------------------------------------------------------------------------------------------------------------

MoveHome:
                call    Doc_LineOffset
                ex      de,hl
                call    Doc_MoveBack
                jp      CursorVisible

;;----------------------------------------------------------------------------------------------------------------------

MoveEnd:        
                call    Doc_ToNextLine
                ld      d,b
                ld      e,c
                call    Doc_MoveForward
                jp      CursorVisible

;;----------------------------------------------------------------------------------------------------------------------

ReadNextReg:
        ; reads nextreg in A into A
        ; Input
        ;       A = nextreg to read
        ; Output:
        ;       A = value in nextreg
        ; Uses:
        ;       A, [currently selected NextReg on I/O port $243B]
                push    bc
                ld      bc, $243B   ; TBBLUE_REGISTER_SELECT_P_243B
                out     (c),a
                inc     b       ; bc = TBBLUE_REGISTER_ACCESS_P_253B
                in      a,(c)   ; read desired NextReg state
                pop     bc
                ret

;;----------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------

        message "Final address: ",PC

        SAVESNA "ed.sna", Start
