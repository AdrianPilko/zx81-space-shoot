; Copyright (c) 2024 Adrian Pilkington

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.



setRandomNumberZeroOne

   	ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
	inc hl
	ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex 
	cp h
	jr z, resetRandSeed_2
	ld (randomSeed),hl
	jr endOfUpdateRandomSeed
resetRandSeed_2
    ld hl, 0
	ld (randomSeed), hl
endOfUpdateRandomSeed
	
    ld a, (hl)
    and %00000001
    ; a now contains random number zero or one
    ret

setRandomNumberFive
    ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
    inc hl
    ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex 
    cp h
    jr z, resetRandSeed_Four
    ld (randomSeed),hl
    jr endOfUpdateRand_Four
resetRandSeed_Four
    ld hl, 0
    ld (randomSeed), hl
endOfUpdateRand_Four

    ld a, (hl)
    and %00000111
    ; a now contains random number 0,1,2,3,4,5
    ret




; chance of getting n 1's in a row is 0.5^n
; so 0.1 = 0.5^n gives us n=(ln(0.1) / ln(0.5))
; this is about 3.32 iterations

setRandomNumberOneInTen
    ;ld b, 3
    ld b, 255 ; changed to make it less likely

NInARow
    dec b
    ld a, b
    cp 0
    jr endOfOneInTenSetAOne
   	ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
	inc hl
	ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex 
	cp h
	jr z, resetRandSeed_3
	ld (randomSeed),hl
	jr endOfUpdateRandomSeed_1
resetRandSeed_3
    ld hl, 0
	ld (randomSeed), hl
endOfUpdateRandomSeed_1
	
    ld a, (hl)
    and %00000001
    cp 1      ; try to get a 1 in ten by looping 4 times in a row if one every time
    jr z, NInARow 
    jr endOfOneInTenSetAZero
endOfOneInTenSetAOne
    ;; at this point we had enough 1's in a row
    ld a, 1
    ret
endOfOneInTenSetAZero
    ld a, 0
    ret


waitABit
	ld b,128
waitABit_WaitLoop_1
    push bc
    ld b, 222
waitABit_WaitLoop_2
    djnz waitABit_WaitLoop_2
    pop bc
	djnz waitABit_WaitLoop_1
    ret

; this prints at to any offset (stored in bc) from the top of the screen Display, using string in de
printstring
    push de ; preserve de
    ld hl,Display
    add hl,bc
printstring_loop
    ld a,(de)
    cp $ff
    jp z,printstring_end
    ld (hl),a
    inc hl
    inc de
    jr printstring_loop
printstring_end
    pop de  ; preserve de
    ret

print_number16bits    ; bc stores the 16bits, print b then c, de stores offset from Display
    ld a, b
    call print_number8bits
    ld a, c
    inc de  ; move de over by 2
    inc de
    call print_number8bits
    ret


print_number8bits
    ld hl, Display+1
    add hl, de
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a

    ret

printNumber
    ld hl,Display
    add hl,bc
printNumber_loop
    ld a,(de)
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a
    ret


;;;; sprite code
;;;; our sprites are custom 8 by 8 charactor blocks - so will look fairly big (maybe too big)
;;;; the generic routines will look at an area of memory stored in hl before the call

;;;; on the zx81 each block is 2 "pixels" horizontally and 2 vertically pre encoded in the sprite memory
;;;; size of sprite in memory using bit pack is 16 * 16 = 256bits ==>>> 32bytes


;;; hl = start of sprite memory
;;; de = offset position in screen memory top left of sprite - no limit check done (yet)
;;; c  = width of sprite (normally 8 to keep things "simple")
;;; b  = rows in sprite (normally 8 to keep things "simple")
drawSprite         
    push bc    
    push de
    ld b, 0               ;; just doing columns in c so zero b
    ldir                  ;; ldir repeats ld (de), (hl) until bc = 0 and increments hl and de
    pop de
    ex de, hl    
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite    
    ret


;; Keyboard check
;; notes that to use this you "call" and must pop stack immediately 
;; in the moveLeft moveRight etc. moveLeft, moveRight, moveUp, moveDown, gameLoopKeyRet
;; must be defined as well as gameLoop in the main code

KEYBOARD_READ_PORT_P_TO_Y	EQU $DF
; for start key
KEYBOARD_READ_PORT_A_TO_G	EQU $FD
; keyboard port for shift key to v
KEYBOARD_READ_PORT_SHIFT_TO_V EQU $FE
; keyboard space to b
KEYBOARD_READ_PORT_SPACE_TO_B EQU $7F
; keyboard q to t
KEYBOARD_READ_PORT_Q_TO_T EQU $FB

; starting port numbner for keyboard, is same as first port for shift to v
KEYBOARD_READ_PORT EQU $FE
; keyboard layout for reading keys on ZX81
; BIT   left block      right block  BIT
; off                                off in <port>, when ld a, <port>
;       0  1 2 3 4     4 3 2 1 0                 <<< bit to check for each column after in a, $fe
; 3   ( 1  2 3 4 5 ) ( 6 7 8 9 0 )     4
; 2   ( Q  W E R T ) ( Y U I O P )     5
; 1   ( A  S D F G ) ( H I K L n/l)    6
; 0   (sft Z X C V ) ( B N M . spc)    7
;
; to read keys 1 2 3 4 5
; set all bits except bit 3 of register A = 1 1 1 1 0 1 1 1= f7, then execute in a, $fe  (fe is the "keyboard read port")
; now register a will contain a bit pattern to check for which key in that block was set, eg Key "1" = bit 0 of a
; ld a, $f7
; in a, $fe
; similarly for the rest, to read from block A S D F G, set a to 1 1 1 1 1 1 1 0 1 = $fd
readKeys:
    ;; read keys
    ld a, KEYBOARD_READ_PORT_P_TO_Y
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 1, a                            ; O
    jp z, moveLeft

    ld a, KEYBOARD_READ_PORT_P_TO_Y
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a					        ; P
    jp z, moveRight

    ld a, KEYBOARD_READ_PORT_Q_TO_T
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a						    ; Q
    jp z, moveUp

    ld a, KEYBOARD_READ_PORT_A_TO_G
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a						    ; A
    jp z, moveDown
	jp gameLoopKeyRet
    ret ;; never gets here

;check if TV synchro (FRAMES) happend
vsync
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
endOfVsync
	ret


fillScreenBlack
    ld hl,Display+1
    ld de, 33
    add hl, de
    ld a, 128
    ld b, 21
rowLoop
    push bc
    ld b, 32
colLoop
    ld (hl),a
    inc hl
    djnz colLoop
    inc hl
    pop bc
    djnz rowLoop
    ret