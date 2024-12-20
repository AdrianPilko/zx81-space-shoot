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

;;; Space shooter game
;;;
;;; https://youtube.com/@byteforever7829

;;; Known bug(s)


;;; todo list
;;; 1a) Decided to make the background move and keep the player space ship in middle
;;;    This should give the illusion of moving around a "landscape" at same time as
;;;    being easier to handle as no case where player might touch the edge of screen
;;; 1b) make the map come from a preconfigured memory area (sparsely populated (hopefully))
;;; 2) make the player shoot work
;;; 3) add enemies - probably have asteroids that start out small and start getting bigger 
;;; 4) add collision detection - player dies when hit by asteroid
;;; 5) add levels - start out with one asteroid increase each level and make them move faster
;;; 6) allow collection of tokens to collect on the "map"
;;; 7) add scoring and high score

;pasmo only accepts DEFINE


CLS EQU $0A2A
PRINTAT EQU $08f5
PRINT EQU $10

SCREEN_SCROLL_MEM_OFFSET EQU 693
SCREEN_SCROLL_MEM_TOP_OFFSET EQU 99

SCREEN_WIDTH EQU 32
SCREEN_HEIGHT EQU 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines
MISSILE_COUNTDOWN_INIT EQU 18
;#define PLAYER_START_POS 604
PLAYER_START_POS EQU 637
PLAYER_LIVES EQU 3
LEVEL_COUNT_DOWN_INIT EQU 4
LEV_COUNTDOWN_TO_INVOKE_BOSS EQU 1

VSYNCLOOP       EQU      1

; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt
_SD:			EQU	$0D	;$
_DOLLAR:        EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F
_INV_QUOTES                     EQU $8B
_INV_A          EQU $A6

_DOOR_OPEN_CHARACTER          EQU $08    ; grey block
_DOOR_LOCKED_CHARACTER            EQU $b1      ; inverse L
_DOOR_OFFSET                    EQU 62
;;;; this is the whole ZX81 runtime system and gets assembled and
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address

VERSN
    DB 0
E_PPC:
    DW 2
D_FILE:
    DW Display
DF_CC:
    DW Display+1                  ; First character of display
VARS:
    DW Variables
DEST:           DW 0
E_LINE:         DW BasicEnd
CH_ADD:         DW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DW 0
STKBOT:         DW BasicEnd+5
STKEND:         DW BasicEnd+5                 ; Empty stack
BREG:           DB 0
MEM:            DW MEMBOT
UNUSED1:        DB 0
DF_SZ:          DB 2
S_TOP:          DW $0002                      ; Top program line number
LAST_K:         DW $fdbf
DEBOUN:         DB 15
MARGIN:         DB 55
NXTLIN:         DW Line2                      ; Next line address
OLDPPC:         DW 0
FLAGX:          DB 0
STRLEN:         DW 0
T_ADDR:         DW $0c8d
SEED:           DW 0
FRAMES:         DW $f5a3
COORDS:         DW 0
PR_CC:          DB $bc
S_POSN:         DW $1821
CDFLAG:         DB $40
MEMBOT:         DB 0,0 ;  zeros
UNUNSED2:       DW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DB $00,$0a                    ; Line 10
                DW Line1End-Line1Text         ; Line 10 length
Line1Text:      DB $ea                        ; REM

onceAtGameLoad
    xor a
    ld (high_score_tens),a
    ld (high_score_hund),a


start
    call CLS
    ld hl, Display+1
    ld de, 71
    add hl, de
    ld b,22
titleLoop1
    ld (hl),_INV_QUOTES
    inc hl
    djnz titleLoop1


    ld bc, 105
    ld de, START_GAME_TITLE
    call printstring


    ld hl, Display+1
    ld de, 137
    add hl, de
    ld b,22
titleLoop2
    ld (hl),_INV_QUOTES
    inc hl
    djnz titleLoop2


    ld bc, 204
    ld de, START_TEXT2
    call printstring

    ld bc, 234
    ld de, START_TEXT_TIP_1
    call printstring

    ld bc, 267
    ld de, START_TEXT_TIP_2
    call printstring

    ld bc, 432
    ld de, START_TEXT3
    call printstring

    ld bc, 498
    ld de, START_TEXT4
    call printstring

    ld bc, 562
    ld de, START_GAME_CRED1
    call printstring


    ld bc, 628
    ld de, START_GAME_CRED2
    call printstring


    ld bc, 698
    ld de, START_GAME_CRED3
    call printstring


	ld bc,337
	ld de,high_Score_txt
	call printstring

    ld bc, 377
    ;ld de, last_score_mem_hund ; load address of hundreds
    ld de, high_score_hund
	call printNumber
	ld bc, 379			; bc is offset from start of display
	;ld de, last_score_mem_tens ; load address of  tens
	ld de, high_score_tens
	call printNumber


    xor a
    ld (score_mem_tens),a
    ld (score_mem_hund),a
    ld (levelCount),a
introWaitLoop
	ld b,64
introWaitLoop_1
    push bc
    pop bc
	djnz introWaitLoop_1

   	ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
	inc hl
	ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex
	cp h
	jr z, resetRandSeed_1
	ld (randomSeed),hl
	jp read_start_key
resetRandSeed_1
    ld hl, 0
	ld (randomSeed), hl
	jp read_start_key     ;; have to have 2 labels as not a call return

read_start_key
	ld a, KEYBOARD_READ_PORT_A_TO_G
	in a, (KEYBOARD_READ_PORT)					; read from io port
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop

    jr preinit  ; not really necessary

preinit

    ld hl,(D_FILE) ;initialise road start memory address
	ld de, SCREEN_SCROLL_MEM_OFFSET
	add hl, de	
	ld (var_scroll_screen_bottom_from), hl
	ld de, 33
	add hl, de
	ld (var_scroll_screen_bottom_to), hl


    ld hl,(D_FILE) ;initialise road start memory address
	ld de, SCREEN_SCROLL_MEM_TOP_OFFSET
	add hl, de	
	ld (var_scroll_screen_top_from), hl
	ld de, 33
	add hl, de
	ld (var_scroll_screen_top_to), hl

    ld a, 20
    ld (genRow), a
    ld a, 1
    ld (genCol), a

	;call CLS  ; clears screen and sets the boarder
    ld bc, 1
    ld de, LEVEL_TEXT
    call printstring
    call fillScreenWhite
    xor a
    ld (enemyAddedFlag),a


    ld de, 310
    ld hl, Display+1
    add hl, de
    ld (currentPlayerLocation), hl


    ld hl, defaultPlayerSprite
    ld (playerSpritePointer), hl

    ld hl, playerDirectionAddSubs
    ld (pointerToMovement), hl

    ld a, 13
    ld (playerX), a
    ld a, 9
    ld (playerY), a
    ld hl, playerMovementXY_X
    ld (playerX_IncPtr), hl
    ld hl, playerMovementXY_Y
    ld (playerY_IncPtr), hl

    ld hl, Display+1;
    ld de, 67
    add hl, de
    ld (hl), 0
    ld de, 33
    add hl, de
    ld (hl), 8

gameLoop

    ld hl, (playerAbsAddress)
    ld a, _INV_A
    ld (hl),a

	ld b,VSYNCLOOP
waitForTVSync
    call vsync
    djnz waitForTVSync

    call moveBackground
    ld hl, (playerSpritePointer)
    ld de, (currentPlayerLocation)
    ld c, 8
    ld b, 8
    call drawSprite
    call readKeys
gameLoopKeyRet   ; this gets jumped to if no keys pressed
    pop hl  ; stack jiggery pokery to make stack consistent without using return
    jp gameLoop


leftPressed
    pop hl
    ld hl,(playerSpritePointer)
    ld de,eigthPlayerSprite
    ld a, e
    cp l
    jp nz, rotateLeft
    ld a, d
    cp h
    jp nz, rotateLeft
    call wrapPointerStart
    jp gameLoop
rotateLeft
    ld hl,(playerSpritePointer)
    ld de, 64
    add hl, de
    ld (playerSpritePointer), hl
    ;; need to set the current direction - this also moves through a sequence based on
    ;; how many "compass point directions" there are (8)
    ld hl, (pointerToMovement)
    inc hl
    inc hl
    ld (pointerToMovement), hl

    ld hl, (playerX_IncPtr)
    inc hl   ; this array is byte sized elements so only one inc
    inc hl
    ld (playerX_IncPtr), hl

    ld hl, (playerY_IncPtr)
    inc hl   ; this array is byte sized elements so only one inc
    inc hl
    ld (playerY_IncPtr), hl

    jp gameLoop

rightPressed
    pop hl
    ld hl,(playerSpritePointer)
    ld de,defaultPlayerSprite
    ld a, e
    cp l
    jp nz, rotateRight
    ld a, d
    cp h
    jp nz, rotateRight
    call wrapPointerEnd
    jp gameLoop
rotateRight
    ld hl,(playerSpritePointer)
    ld de, 64
    or a   ; clear the casrry flag otherwise will subtract one more than expected (sometimes)
    sbc hl, de
    ld (playerSpritePointer), hl
    ;; need to set the current direction - this also moves through a sequence based on
    ;; how many "compass point directions" there are (8)
    ld hl, (pointerToMovement)
    dec hl
    dec hl
    ld (pointerToMovement), hl

    ld hl, (playerX_IncPtr)
    dec hl   ; this array is byte sized elements so only one inc
    dec hl
    ld (playerX_IncPtr), hl
    ld hl, (playerY_IncPtr)
    dec hl   ; this array is byte sized elements so only one inc
    dec hl
    ld (playerY_IncPtr), hl

    jp gameLoop

upPressed
    pop hl
    jp gameLoop
downPressed
    pop hl
    jp gameLoop
firePressed
    pop hl

    jp gameLoop

spacePressed ; this like the other key press are just jumped to not called
    pop hl
    ld a, (playerMoving)  ; toggle player moving
    cp 1
    jp z, stopMoveOnFire
    ld a, 1
    ld (playerMoving),a
    jp gameLoop
stopMoveOnFire
    xor a
    ld (playerMoving), a
    jp gameLoop


moveBackground
    ; go via a to dereference pointer to movement
    ld hl, (playerY_IncPtr)
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    ld a, e
    cp -1
    push af
    call z, scrollUp
    pop af
    cp 1
    call z, scrollDown

    ld hl, (playerX_IncPtr)
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    ld a, e
    cp 1
    push af
    call z, scrollLeft
    pop af
    cp -1
    call z, scrollRight
    ret

scrollUp
    ld a, (toggleLineY)
    cp 1
    jp z, setLineGreyUp

    ld a, 1
    ld (toggleLineY),a
    ld a, 128  ; black block
    ld c, 8
    jp doTheMoveUp
setLineGreyUp:
    xor a
    ld (toggleLineY),a
    ld a, 8  ; grey block
    ld c, 128

doTheMoveUp:
;;; add a line of non white "stuff"
    ld hl, Display+1;
    ld de, 66
    add hl, de
    ld b, 32
 
addLineAtTopUp
    ld (hl),a
    inc hl
    djnz addLineAtTopUp
    ld a, c
    inc hl
    ld b, 32
 
addLineAtTopNextUp
    ld (hl),a
    inc hl
    djnz addLineAtTopNextUp
  
	;scroll screen up	
	ld hl,(var_scroll_screen_bottom_from)  ; load left road address	
	ld de,(var_scroll_screen_bottom_to) ; load right road address		
	;ld bc,694 ;694 = 32columns * 21 rows - 1
    ld bc, 660 ;  = 33columns * 20 rows
	; LDDR repeats the instruction LDD (Does a LD (DE),(HL) and increments 
	; each of DE, HL, and BC) until BC=0. Note that if BC=0 before 
	; the start of the routine, it will try loop around until BC=0 again.	
	lddr
    ret

scrollDown
    ld a, (toggleLineY)
    cp 1
    jp z, setLineGreyDown

    ld a, 1
    ld (toggleLineY),a
    ld a, 128  ; black block
    ld c, 8
    jp doTheMoveDown
setLineGreyDown:
    xor a
    ld (toggleLineY),a
    ld a, 8  ; grey block
    ld c, 128

doTheMoveDown:
;;; add a line of non white "stuff"
    ld hl, Display+1;
    ld de, 726
    add hl, de
    ld b, 32
 
addLineAtBottomDown
    ld (hl),a
    inc hl
    djnz addLineAtBottomDown
    ld a, c
    inc hl
    ld b, 32
 
addLineAtTopPrevDown
    ld (hl),a
    inc hl
    djnz addLineAtTopPrevDown
  
  	;scroll screen down	
	ld hl,(var_scroll_screen_top_to)  ; load left road address	
	ld de,(var_scroll_screen_top_from) ; load right road address		
	;ld bc,694 ;694 = 32columns * 21 rows - 1
    ld bc, 660 ;  = 33columns * 20 rows
	; LDDR repeats the instruction LDI (Does a LD (DE),(HL) and decrements 
	; each of DE, HL, and BC) until BC=0. Note that if BC=0 before 
	; the start of the routine, it will try loop around until BC=0 again.	
	ldir   
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; scroll screen left and add alternating lines at eeach edge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scrollLeft
    ld a, (toggleLineX)
    cp 1
    jp z, setLineGreyLeft

    ld a, 1
    ld (toggleLineX),a
    ld a, 128  ; black block
    ld c, 8
    jp doTheMoveLeft
setLineGreyLeft:
    xor a
    ld (toggleLineX),a
    ld a, 8  ; grey block
    ld c, 128
 doTheMoveLeft
 ;;; add a column of non white "stuff" on left
 ;; eventually this should come from a map data area in memory
    ld hl, Display+1
    ld de, 64
    add hl, de
    ld b, 23
    ld de, 33
addLineRight
    ld (hl),a
    add hl, de
    djnz addLineRight
    ld a, c
    ld de, 63
    add hl, de
    ld b, 23
    ld de, 33 
addLineAtRightNext
    ld (hl),a
    add hl, de
    djnz addLineAtRightNext

    ld b, 23
    ld hl,(D_FILE)          
    inc hl
    ld de, 33
    add hl, de
    push hl
    pop de
screenScrollLeftRowLoop
    push bc       
    
    ;; this is the super fast way to copy  the columns to the left
    ld bc, 31   
    inc hl    
    ldir            ;; this is where the magic happens!!! 
                    ;; LDIR : Repeats LDI (LD (DE),(HL), then increments DE, HL, and decrements BC) until BC=0.
    inc hl
    push hl
    pop de
    pop bc
    djnz screenScrollLeftRowLoop
    ret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; scroll screen right and add alternating lines at each edge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scrollRight
    ld a, (toggleLineX)
    cp 1
    jp z, setLineGreyRight

    ld a, 1
    ld (toggleLineX),a
    ld a, 128  ; black block
    ld c, 8
    jp doTheMoveRight
setLineGreyRight:
    xor a
    ld (toggleLineX),a
    ld a, 8  ; grey block
    ld c, 128
doTheMoveRight
 ;;; add a column of non white "stuff" on left
 ;; eventually this should come from a map data area in memory
    ld hl, Display+1
    ld de, 33
    add hl, de
    ld b, 23
    ld de, 33
addLineLeft
    ld (hl),a
    add hl, de
    djnz addLineLeft
    ld a, c
    inc hl
    ld b, 23
    ld de, 33 
addLineAtLeftNext
    ld (hl),a
    add hl, de
    djnz addLineAtLeftNext



    ld b, 23
    ld hl,(D_FILE)          
    inc hl
    ld de, 64
    add hl, de
    push hl
    pop de
screenScrollRightRowLoop
    push bc       
    push hl
    ;; this is the super fast way to copy  the columns to the left
    ld bc, 31   
    dec hl   
    lddr            ;; this is where the magic happens!!! 
                    ;; LDIR : Repeats LDI (LD (DE),(HL), then increments DE, HL, and decrements BC) until BC=0.
    pop hl
    ld de, 33
    add hl, de
    push hl
    pop de
    pop bc
    djnz screenScrollRightRowLoop 
    ret

movePlayer
    ld a, (playerMoving)
    cp 1
    jp z, do_movePlayer
    ret
do_movePlayer

    ;ld a, (playerX)
    ;ld de, 45
    ;call print_number8bits

    ;ld a, (playerY)
    ;ld de, 48
    ;call print_number8bits

     ; go via a to dereference pointer to movement
    ld hl, (pointerToMovement)
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d

    ;; so now de has the value to add for movements
    ld hl, (currentPlayerLocation)
    add hl, de
    ld (currentPlayerLocation), hl


    ; go via a to dereference pointer to player x
    ld hl, (playerX_IncPtr)
    ld a, (hl)

    push af
    pop bc
    or a
    ld a, (playerX)
    add a,b
    ld (playerX), a

    ; go via a to dereference pointer to player y
    ld hl, (playerY_IncPtr)
    ld a, (hl)

    push af
    pop bc
    or a
    ld a, (playerY)
    add a,b
    ld (playerY), a

   ; check if on edge
    ld a, (playerX)
    cp 2
    jp z, reversePlayerDirection
    cp 22
    jp z, reversePlayerDirection
    ld a, (playerY)
    cp 3
    jp z, reversePlayerDirection
    cp 16
    jp z, reversePlayerDirection
    jp returnFromMovePlayer
reversePlayerDirection
    ;xor a
    ;ld (playerMoving),a
    ld b, 4
reversePlayerLoop
    push bc
    ld hl,(playerSpritePointer)
    ld de,eigthPlayerSprite
    ld a, e
    cp l
    jp nz, incPtrsToReverseMov
    ld a, d
    cp h
    jp nz, incPtrsToReverseMov

    call wrapPointerStart
    jp endOfReverseMovementLoop
incPtrsToReverseMov
    ld hl,(playerSpritePointer)
    ld de, 64
    add hl, de
    ld (playerSpritePointer), hl
    ;; need to set the current direction - this also moves through a sequence based on
    ;; how many "compass point directions" there are (8)
    ld hl, (pointerToMovement)
    inc hl
    inc hl
    ld (pointerToMovement), hl

    ld hl, (playerX_IncPtr)
    inc hl   ; this array is byte sized elements so only one inc
    inc hl
    ld (playerX_IncPtr), hl

    ld hl, (playerY_IncPtr)
    inc hl   ; this array is byte sized elements so only one inc
    inc hl
    ld (playerY_IncPtr), hl
endOfReverseMovementLoop
    pop bc
    djnz reversePlayerLoop
    xor a
    ld (playerMoving), a
returnFromMovePlayer
    ret

gameWon
    ld bc, 308
    ld de, YOU_WON_TEXT_0
    call printstring
    ld bc, 341
    ld de, YOU_WON_TEXT_1
    call printstring
    ld bc, 374
    ld de, YOU_WON_TEXT_2
    call printstring

    ld a, (levelCount)
    inc a
    daa
    ld (levelCount), a

    call waitABit
    call waitABit
    call waitABit
    call waitABit
    jp preinit
    ret

gameOver
    ld bc, 308
    ld de, YOU_WON_TEXT_0 ;; reuse boarder
    call printstring
    ld bc, 341
    ld de, YOU_LOST_TEXT_1
    call printstring
    ld bc, 374
    ld de, YOU_WON_TEXT_2
    call printstring
    xor a
    ld (currentDollarCount), a

    call waitABit
    call waitABit
    call waitABit
    call waitABit
    pop bc ; maintain stack integrity as previous call no return
    jp start


increaseScore
    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
    inc a
    daa									; z80 daa instruction realigns for BCD after add or subtract
    ld (score_mem_tens),a
    cp $99
    jr z, addOneToHund
    jr skipAddHund
addOneToHund
    xor a
    ld (score_mem_tens), a
    ld a, (score_mem_hund)
    inc a
    daa                                   ; z80 daa instruction realigns for BCD after add or subtract
    ld (score_mem_hund), a
skipAddHund



    ; if the player has collected at least 10 dollars open the door
    ld a, (currentDollarCount)
    inc a
    cp 10
    jr z, openDoor
    ld (currentDollarCount),a
    jr checkHighScore
openDoor
    ld de, _DOOR_OFFSET   ; exit location
    ld hl, Display+1
    add hl, de
    ld a, _DOOR_OPEN_CHARACTER
    ld (hl),a

checkHighScore
; compare with high score and set that if higher

    ld a, (score_mem_hund)
    ld b,a     ; load the second 8-bit number into register b (via a)
    ld a, (high_score_hund)   ; load the first 8-bit number into register a
    cp b            ; compare a with the second 8-bit number (in register b)
    jr c, setHighScore ; jump if carry flag is set (high_score_hund < score_mem_hund)

    ; check if equal, and if so then check the tens, could be 00 50, or 01 50 in high score and current score
    jr z, highScoreHundEqualCheckTens
    ; high_score_hund > score_mem_hund so don't set
    jr skipCheckRestHighScore

highScoreHundEqualCheckTens
    ld a, (score_mem_tens)
    ld b, a
    ld a, (high_score_tens)
    cp b
    jp c, setHighScore ; jump if carry flag is set (a < b)

    jr skipCheckRestHighScore

setHighScore
    ld a, (score_mem_tens)
    ld (high_score_tens), a
    ld a, (score_mem_hund)
    ld (high_score_hund), a
    jr skipCheckRestHighScore
skipCheckRestHighScore
increaseScoreEnd
    ret

decreaseScore						; z80 daa instruction realigns for BCD after add or subtract
    ld a,(score_mem_tens)
    cp 0
    jr z, decrementHund
    dec a
    daa
    ld (score_mem_tens),a
    jr skipDecrementHund
decrementHund
    xor a
    ld (score_mem_tens), a
    ld a, (score_mem_hund)
    cp 0
    jr z, skipDecrementHund
    dec a
    daa                                   ; z80 daa instruction realigns for BCD after add or subtract
    ld (score_mem_hund), a
skipDecrementHund
    ret



printLivesAndScore
    push hl
    ld a, (levelCount)
    ld de, 6
    call print_number8bits

    ld bc, 31
    ld de, score_mem_tens
    call printNumber

    ld bc, 29
    ld de, score_mem_hund
    call printNumber
    pop hl
    ret

wrapPointerEnd
    push hl
      ld hl, eigthPlayerSprite
      ld (playerSpritePointer),hl
      ld hl, playerDirectionAddSubs_end
      ld (pointerToMovement), hl
      ld hl, playerMovementXY_X_end
      ld (playerX_IncPtr), hl
      ld hl, playerMovementXY_Y_end
      ld (playerY_IncPtr), hl


    pop hl
    ret


wrapPointerStart
    push hl
      ld hl, defaultPlayerSprite
      ld (playerSpritePointer), hl
      ld hl, playerDirectionAddSubs
      ld (pointerToMovement), hl
      ld hl, playerMovementXY_X
      ld (playerX_IncPtr), hl
      ld hl, playerMovementXY_Y
      ld (playerY_IncPtr), hl

    pop hl
    ret

INCLUDE commonUtils.asm

                DB $76                        ; Newline
Line1End
Line2			DB $00,$14
                DW Line2End-Line2Text
Line2Text     	DB $F9,$D4                    ; RAND USR
				DB $1D,$22,$21,$1D,$20        ; 16514
                DB $7E                        ; Number
                DB $8F,$01,$04,$00,$00        ; Numeric encoding
                DB $76                        ; Newline
Line2End
endBasic

Display        	DB $76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                DB $76
DisplayEnd

Variables
score_mem_tens
    DB 0
score_mem_hund
    DB 0
high_score_tens
    DB 0
high_score_hund
    DB 0
prevPlayerAddress
    DW 0
playerAbsAddress
    DW 0
enemyLocation
    DW 0
prevEnemyLocation
    DW 0
enemyAddedFlag
    DB 0
genCol
    DB 0
genRow
    DB 0
mazeVisitedLocations
    DS 32*21, 0
mazeScreenBuffer
    DS 32*21, 8
randomSeed
    DW 0
levelCount
    DB 0
currentDollarCount
    DB 0

playerSpritePointer
    DEFW 0

currentPlayerLocation
    DEFW 0
playerX
    DEFB 10
playerY
    DEFB 10
playerMoving
    DEFB 0
pointerToMovement
    DEFW  0
tempPointerToMovement
    DEFW 0
; this controls the movements of the player based on how many blocks have to be added or subtracted to move
; in the 8 directions
playerDirectionAddSubs
    DEFW -33  ; north
    DEFW -34  ; north-west
    DEFW -1   ; west
    DEFW +32  ; south-west
    DEFW +33  ; south
    DEFW +34  ; south-east
    DEFW +1   ; east
playerDirectionAddSubs_end
    DEFW -32  ; north-east

; this controls how much is added to X or Y for the player directions
; they are bytes because the playerX playerY are byte also
playerX_IncPtr
    DEFW 0
playerY_IncPtr
    DEFW 0

DEBUG1
    DEFW $CAC0
    DEFW $CAFE


playerMovementXY_X
    DEFW  0      ; north
    DEFW -1     ; north-west
    DEFW -1     ; west
    DEFW -1     ; south-weat
    DEFW 0      ; south
    DEFW +1     ; south-east
    DEFW +1     ; east
playerMovementXY_X_end
    DEFW +1     ; north-east
playerMovementXY_Y
    DEFW -1     ; north
    DEFW -1
    DEFW 0      ; west
    DEFW +1
    DEFW +1     ; south
    DEFW +1
    DEFW 0      ; east
playerMovementXY_Y_end
    DEFW -1


DEBUG2
    DEFW $BAD0
    DEFW $BEEF

toggleLineX
  DEFB 0

toggleLineY
  DEFB 0



;;; this is 8 x 8 (16 by 16 "pixels" times 8 sprites, one for each of the compass points and in between
;; this amounts to 512 bytes of RAM (wow!!!)
defaultPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $85, $05, $00, $00, $00
  DEFB $00, $00, $00, $81, $82, $00, $00, $00
  DEFB $00, $00, $80, $80, $80, $80, $00, $00
  DEFB $00, $85, $01, $85, $05, $02, $05, $00
  DEFB $00, $00, $00, $84, $07, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
secondPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $80, $04, $00, $87, $00, $00
  DEFB $00, $00, $02, $80, $81, $07, $86, $00
  DEFB $00, $00, $00, $81, $80, $04, $04, $00
  DEFB $00, $00, $87, $07, $02, $80, $04, $00
  DEFB $00, $00, $00, $86, $02, $02, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
thirdPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $83, $00, $00
  DEFB $00, $00, $00, $00, $80, $01, $00, $00
  DEFB $00, $00, $83, $81, $80, $83, $82, $00
  DEFB $00, $00, $03, $84, $80, $03, $07, $00
  DEFB $00, $00, $00, $00, $80, $04, $00, $00
  DEFB $00, $00, $00, $00, $00, $03, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
fourthPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $06, $87, $00, $00, $00
  DEFB $00, $00, $02, $82, $87, $82, $00, $00
  DEFB $00, $00, $00, $84, $80, $01, $01, $00
  DEFB $00, $00, $87, $80, $84, $82, $06, $00
  DEFB $00, $00, $80, $01, $00, $02, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
fifthPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $81, $82, $00, $00, $00
  DEFB $00, $85, $04, $85, $05, $87, $05, $00
  DEFB $00, $00, $80, $80, $80, $80, $00, $00
  DEFB $00, $00, $00, $84, $07, $00, $00, $00
  DEFB $00, $00, $00, $85, $05, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
sixthPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $04, $04, $86, $00, $00, $00
  DEFB $00, $02, $80, $04, $81, $01, $00, $00
  DEFB $00, $02, $02, $80, $07, $00, $00, $00
  DEFB $00, $86, $81, $07, $80, $04, $00, $00
  DEFB $00, $00, $01, $00, $02, $80, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
seventhPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $83, $00, $00, $00, $00, $00
  DEFB $00, $00, $02, $80, $00, $00, $00, $00
  DEFB $00, $81, $83, $80, $82, $83, $00, $00
  DEFB $00, $84, $03, $80, $07, $03, $00, $00
  DEFB $00, $00, $87, $80, $00, $00, $00, $00
  DEFB $00, $00, $03, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
eigthPlayerSprite
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
  DEFB $00, $00, $04, $00, $87, $80, $00, $00
  DEFB $00, $06, $84, $82, $80, $01, $00, $00
  DEFB $00, $87, $87, $80, $82, $00, $00, $00
  DEFB $00, $87, $80, $01, $84, $04, $00, $00
  DEFB $00, $00, $01, $01, $06, $00, $00, $00
  DEFB $00, $00, $00, $00, $00, $00, $00, $00
endPlayerSpriteMem

var_scroll_screen_bottom_from
    DW 0
var_scroll_screen_bottom_to
    DW 0

var_scroll_screen_top_from
    DW 0
var_scroll_screen_top_to
    DW 0
YOU_WON_TEXT_0
    DB 7,3,3,3,3,3,3,3,3,3,3,3,3,132,$ff
YOU_LOST_TEXT_1
    DB 5,_Y,_O,_U,__,_L,_O,_S,_E,_CL,_MI,_OP,__,133,$ff
YOU_WON_TEXT_1
    DB 5,__,__,_Y,_O,_U,__,_W,_O,_N,__,__,133,$ff
YOU_WON_TEXT_2
    DB 130,131,131,131,131,131,131,131,131,131,131,131,131,129,$ff
LEVEL_TEXT
    DB _F,_U,_E,_L,_CL,0,0,0,0,0,_A,_I,_R,_CL,0,0,0,0,0,_C,_A,_S,_H,_CL,0,0,0,0,0,$FF
START_GAME_TITLE
    DB 	139,0,_Z,_X,_8,_1,0,_S,_P,_A,_C,_E,0,_S,_H,_O,_O,_T,_E,_R,0,139,$ff
START_GAME_CRED1
   DB  1, 2, 1, 2 ,1,0, _Y,_O,_U,_T,_U,_B,_E,_CL,0,_B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,0,1,2,1,2,1,$ff
START_GAME_CRED2
    DB 1,2,1,2,1,0,_B,_Y,0,_A,0,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,0,_2,_0,_2,_4,0,1,2,1,2,1,$ff
START_GAME_CRED3
    DB 1, 2, 1, 2 ,1,0,_V,_E,_R,_S,_I,_O,_N,0,_V,_0,_DT,_2,0,1,2,1,2,1,$ff
START_TEXT2
    DB 0,0,0,_D, _E, _F, _E, _A, _T,0, _T, _H, _E, 0, _A, _L, _I, _E, _N, _S, $ff
START_TEXT_TIP_1
    DB 0,$ff
START_TEXT_TIP_2
    DB 0,$ff
START_TEXT3
    DB _K, _E, _Y, _S,_CL,0,_P, _R, _E, _S, _S, 0, _S,0, _T, _O, 0, _S, _T, _A, _R, _T,$ff
START_TEXT4
    DB _Q, 0, _U, _P, _CM,0, _A, 0, _D, _O, _W, _N, _CM,0, _O, 0, _L, _E, _F,_T,_CM,0,_P,0,_R,_I,_G,_H,_T,$ff
high_Score_txt
	DB 21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff

PlayerUniversePosition
    DB 0,0
UniverseMemory
;; The start posiiton in the universe will be a random position and stored in PlayerUniversePosition
;; The universe can have different elements including, each will be a single character (may change later)
;;     enemy ships - use <
;;     planets - use O - show a window with options like trade etc
;;     stars - use asterisk - die if hit
;;     Fuel - uses an inverted F, 
;;     black holes - inverted B
;;     asteroids use inverted $  
;;     . will be used as the start line offset
;; player has to collet fuel to keep going and collect "ore" from an asteroid the score increases
;; we'll make the size of the "universe" as 12K which should allow us plenty of room for program code
;; example line in universe as in memory, block followed by gap to next, each line has to add up to 32
;; maximum of 32 including the drawn characters themselves
;; .,3,*,5,F,10,B,8,0,255,.,32,255,.,7,O,6,*,255,.,
;; this will give:
;; 12345678911111111112222222223333   "Column numbers"
;;          01234567890123456789012
;;----------------------------------
;;    *     F          B        O
;;       
;;        O      *    
;;
;; the draw routing simply has to read from the current location, draw the character then skip the number of
;; characters and draw the next. The end of line will be 0xff = 255. if the next number is 255 then the 
;; character is the last on the line.
;;
;; This is only an idea at the moment!
DB _DT,3,_AS,'F'+128,255

VariablesEnd:   DB $80
BasicEnd:


