;;----------------------------------------------------------------------------------------------------------------------
;; High-level display routines
;; Displays the editor user interface
;;----------------------------------------------------------------------------------------------------------------------

videoMaxX       db      80
videoMaxY       db      42

Title       db      "Ed (V0.1)",0
Footnote    db      "Ln 0001  Col 0001 \"debug text: gpq\"",0

;;----------------------------------------------------------------------------------------------------------------------

FindScrollStart:
        ; Scan a line to find the first character that is on the left side of the screen.  If the line is shorter
        ; than the scrolled offset, return the address of the newline
        ;
        ; Input:
        ;   HL = start of line
        ; Output:
        ;   HL = start of line on left side of screen, or pointing to $0a/$1b at end of line
        ; Destroys
        ;   A
        ;
                ld      a,(dx)
                and     a
                ret     z
                push    bc
                ld      b,a
.l1             ld      a,(hl)
                cp      EOL         ; Is it new line?
                jr      z,.finish
                cp      EOF
                jr      z,.finish
                inc     hl
                djnz    .l1
.finish
                pop     bc
                ret

;;----------------------------------------------------------------------------------------------------------------------

FindNextLine:
        ; Find the address of the next line or address of $1a character if EOF found.
        ;
        ; Input:
        ;   HL = Document address
        ; Output:
        ;   HL = Address of next line
        ; Destroys:
        ;   A
        ;
                ld      a,(hl)
                cp      EOL             ; Test for end of line
                jr      z,.eol_found
                cp      EOF             ; Test for end of file
                ret     z               ; HL stays pointing to EOF
                inc     hl
                jr      FindNextLine
.eol_found:
                inc     hl              ; Move past end of line
                ret

;;----------------------------------------------------------------------------------------------------------------------

DisplayRow:
        ; Display a single row of text on the screen
        ; 
        ; Input
        ;   HL = beginning of row
        ;   DE = tilemap address
        ; Output
        ;   HL = beginning of next document line
        ;   DE = points to next line on tilemap
        ; Destroys
        ;   A
        ;
                push    bc
                call    FindScrollStart     ; HL = beginning of visible row
                ld      b,80
.l1             ld      a,(hl)
                cp      EOL
                jr      z,.endline
                cp      EOF
                jr      z,.endline
                ld      (de),a          ; Write character
                inc     hl
                inc     de
                xor     a               ; Normal text colours
                ld      (de),a          ; Write attribute
                inc     de
                djnz    .l1

        ; Managed to fill whole row of screen
                call    FindNextLine
                pop     bc
                ret

.endline
                call    FindNextLine    ; Skip to next line, ready for next display row
                ld      c,' '
                xor     a
                ex      de,hl
.l2             ld      (hl),c          ; Write out space
                inc     hl
                ld      (hl),a          ; Write out attribute
                inc     hl
                djnz    .l2
                ex      de,hl
                pop     bc
                ret

;;----------------------------------------------------------------------------------------------------------------------

DisplayScreen:
        ; Title
                ld      bc,$0000        ; At (0,0)
                ld      de,$0150        ; 80x1
                ld      a,1             ; Colour 2
                call    video.WriteSpace
                ld      bc,$0001
                ld      de,Title
                ld      a,1
                call    video.Print

        ; Status bar
                ld      a,(videoMaxY)
                dec     a
                ld      b,a
                ld      c,0             ; At(0, MaxY-1)
                push    bc              ; keep it also for drawing footnote
                push    bc              ; keep it also for drawing rows of text
                ld      de,$0150
                ld      a,1
                call    video.WriteSpace
                pop     bc
                inc     c
                ld      de,Footnote
                ld      a,1
                call    video.Print

                ;;; DEBUG BEGIN
                ; one more extra footnote beyond the screen to debug 80x42.6 mode
                pop     bc
                push    bc
                inc     b
                ld      de,Footnote
                ld      a,9
                call    video.Print
                ;;; DEBUG END

        ; Write the rows
                ld      bc,$0100
                call    video.CalcTileAddress
                ex      de,hl           ; DE = Tile address
                push    de
                ld      hl,(top)        ; HL = Pointer to document that is at the top
                ld      de,$c000
                add     hl,de           ; Convert to real address
                pop     de
                pop     bc
                dec     b               ; B = videoMaxY - 2
.l1             call    DisplayRow
                djnz    .l1
                ret

;;----------------------------------------------------------------------------------------------------------------------

DisplayCursor:
        ; Display the cursor based on the frame counter.  If the cursor is currently blinking off, use colour slot 0.
        ; Input
        ;       BC = YX coord of cursor
        ;       A = cursor colour slot (0-15)
                push    de
                push    hl
                call    video.CalcTileAddress
                inc     hl                      ; Advance to attribute part
        
                swapnib
                ld      e,a                     ; E = cursor colour
        
        ; Let's find out the actual colour of the cursor based on the frame counter
                ld      a,(Counter)
                and     16
                jr      z,.cursor_appears
                ld      e,0
        
.cursor_appears ld      a,(hl)                  ; Replace colour
                and     $0f
                or      e
                ld      (hl),a
        
                pop     de
                pop     hl
                ret

;;----------------------------------------------------------------------------------------------------------------------

DebugValue      dw      0

GetNibble:
        ; Input:
        ;       A = Value to get lower nibble as an ASCII character
        ; Output:
        ;       D = New value (A shifted right 4 times)
        ;       E = ASCII character of lower nibble
                ld      d,a
                and     $f
                cp      10
                sbc     a,$69
                daa
                ld      e,a
                ld      a,d
                and     $f0
                swapnib
                ld      d,a
                ret

DebugByte:
        ; Input:
        ;       A = byte to output
        ;       BC = YX coords
        ; Output:
        ;       BC = Coord of next position
        ; Uses:
        ;       DE, A
        ;
                swapnib                         ; Swap nibbles - print lower one first
                call    GetNibble
                push    de                      ; Store next value
                ld      d,14                    ; Colour 14
                call    video.PrintChar         ; Print the upper nibble
                pop     de                      ; Restore next value
                ld      a,d
                inc     c                       ; Increment X coord
                call    GetNibble               ; Get ASCII for next nibble
                ld      d,13                    ; Colour 13
                call    video.PrintChar         ; Print the lower nibble
                inc     c
                ret


DisplayDebugger:
                push    bc
                ld      bc,(videoMaxY-1)        ; B = videoMaxY
                dec     b
                ld      c,$4b                   ; BC = [maxY-1, 75]

                ; First nibble
                ld      a,(DebugValue+1)
                call    DebugByte
                ld      a,(DebugValue)
                call    DebugByte
                pop     bc
                ret


