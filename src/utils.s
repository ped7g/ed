;;----------------------------------------------------------------------------------------------------------------------
;; Various utilities required throughout all the source code
;;----------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------
;; Cursor management

VirtToReal:
        ; Convert a virtual mark to a real mark.  Virtual marks represent an offset into the document.
        ; Real marks are actual offsets into the memory the document resides and respects the buffer gap.
        ;
        ; Input:
        ;   HL = virtual mark
        ; Output:
        ;   HL = real mark
        ret

;;----------------------------------------------------------------------------------------------------------------------
;; Circular buffer - uses the print buffer at $5b00

PRead   db      0       ; Offset in buffer of read point (should be <= PWrite)
PWrite  db      0       ; Offset in buffer of write point

; Empty buffer:
;
;       +-------------------------------------------+
;                       ^^
;                       RW
;
;       R points to just before read point
;       W points at new place to write
;       R should never meet W while reading
;
; Full buffer
;       +XXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXX+
;                       ^
;                       R
;                       W

BufferPage:     db      $5b

BufferInsert:
                ; Input
                ;       C = Value to insert
                push    hl
                push    af

                ld      a,(PRead)
                ld      hl,PWrite
                cp      (hl)            ; Has PWrite reached PRead yet?
                ret     z               ; Return without error (buffer is full)

                push    hl
                ld      l,(hl)
                ld      h,(BufferPage)  ; DE = write address
                ld      (hl),c          ; write value in buffer
                pop     hl
                inc     (hl)
                pop     af
                pop     hl
                ret

BufferRead:     ; Output
                ;       A = Value
                ;       ZF = 1 if nothing to read
                push    hl
                push    de
                ld      a,(PWrite)
                dec     a
                ld      hl,PRead
                cp      (hl)            ; Buffer is empty?
                ret     z

                inc     (hl)
                ld      e,(hl)
                ld      d,(BufferPage)
                ld      a,(hl)          ; Read data
                and     a               ; Clear ZF
                pop     hl
                pop     de
                ret


