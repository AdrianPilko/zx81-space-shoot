// Adrian Pilko 2024
// ZX81 Space shooter

#include <input.h>
#include <zx81.h>
#include <stdio.h>

uint16_t highScore = 0;

uint8_t spriteMem[256] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x81,
    0x82, 0x00, 0x00, 0x00, 0x04, 0x00, 0x87, 0x05, 0x85, 0x04, 0x00, 0x87,
    0x05, 0x87, 0x80, 0x07, 0x84, 0x80, 0x04, 0x85, 0x80, 0x80, 0x80, 0x07,
    0x84, 0x80, 0x80, 0x80, 0x02, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01,
    0x00, 0x00, 0x03, 0x02, 0x01, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x81, 0x82, 0x00, 0x00, 0x00, 0x04, 0x00, 0x87, 0x05,
    0x85, 0x04, 0x00, 0x87, 0x05, 0x87, 0x80, 0x07, 0x84, 0x80, 0x04, 0x85,
    0x80, 0x80, 0x80, 0x07, 0x84, 0x80, 0x80, 0x80, 0x02, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x01, 0x00, 0x00, 0x03, 0x02, 0x01, 0x03, 0x00, 0x00,
    0x00, 0x00, 0x06, 0x02, 0x04, 0x86, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x81, 0x82, 0x00, 0x00, 0x00,
    0x04, 0x00, 0x87, 0x05, 0x85, 0x04, 0x00, 0x87, 0x05, 0x87, 0x80, 0x07,
    0x84, 0x80, 0x04, 0x85, 0x80, 0x80, 0x80, 0x07, 0x84, 0x80, 0x80, 0x80,
    0x02, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01, 0x00, 0x00, 0x03, 0x02,
    0x01, 0x03, 0x00, 0x00, 0x00, 0x00, 0x86, 0x00, 0x01, 0x06, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x81,
    0x82, 0x00, 0x00, 0x00, 0x04, 0x00, 0x87, 0x05, 0x85, 0x04, 0x00, 0x87,
    0x05, 0x87, 0x80, 0x07, 0x84, 0x80, 0x04, 0x85, 0x80, 0x80, 0x80, 0x07,
    0x84, 0x80, 0x80, 0x80, 0x02, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01,
    0x00, 0x00, 0x07, 0x02, 0x05, 0x07, 0x00, 0x00, 0x00, 0x00, 0x87, 0x87,
    0x00, 0x06, 0x00, 0x00
};

// got scroll_left() pretty much from https://www.z88dk.org/forum/viewtopic.php?t=11668 , except I only scroll top half of the screen
// I might re-write using ldir instruction or similar 

int __FASTCALL__ scroll_left()
{
	#asm
	ld	hl,(16396)	; D_FILE
	inc	hl

	ld b, 12		; scrolling 12 lines left
.loop1
	ld a, (hl)
	cp 0x76
	jp z, empty_line

	ld d, h
	ld e, l		; de -> previous char
	inc hl		; hl -> current char

.first
	ld a, (hl)
	cp 0x76
	jr nz, copying

	ld a, 0
	ld (de), a		; fill last char with space
	jr empty_line

.copying
	ld (de), a
	inc de
	inc hl

	jr first

.empty_line

	inc hl
	djnz loop1

	#endasm
}

int __FASTCALL__ fast_scroll_left()
{
	#asm
	ld	hl,(16396)	; D_FILE
	inc	hl

	push hl
	pop de

	ld b, 12		; scrolling 12 lines right
.scrollRow
    push bc
	    ld bc, 31
		inc hl
		ldir 

		inc hl
		push hl
		pop de

    pop bc
    djnz scrollRow
	#endasm
}


int __FASTCALL__ scroll_right()
{
	#asm
	ld	hl,(16396)	; D_FILE
	inc	hl
	ld de, 760 ; offset to bottom of screen 
	add hl, de

	ld b, 12		; scrolling 12 lines right
.rloop1
	ld a, (hl)
	cp 0x76
	jp z, rempty_line

	ld d, h
	ld e, l		; de -> previous char
	dec hl		; hl -> current char

.rfirst
	ld a, (hl)
	cp 0x76
	jr nz, rcopying

	ld a, 0
	ld (de), a		; fill last char with space
	jr rempty_line

.rcopying
	ld (de), a
	dec de
	dec hl

	jr rfirst

.rempty_line

	dec hl
	djnz rloop1

	#endasm
}


// got from https://www.z88dk.org/forum/viewtopic.php?t=11668 
// combine 2 chars into 16 bit int
int combine(uchar y, uchar x)
{
	int p;
	p = (y<<8) + x;
	return p;
}


// got from https://www.z88dk.org/forum/viewtopic.php?t=11668 
int __FASTCALL__ init_screen(uchar i)
{
	#asm
	ld a, l
	ld hl,(16396)	; D_FILE
	inc	hl

	ld b, 23

.loop_init
	push bc
	ld b, 32

.loop_row1
	ld (hl), a
	inc hl
	djnz loop_row1

	ld (hl), 0x76
	inc hl
	pop bc

	djnz loop_init
	#endasm
}


// got from https://www.z88dk.org/forum/viewtopic.php?t=11668 
int __FASTCALL__ zx81_saddr(int yx)
{
	#asm
	ld b, h
	ld c, l

	ld hl,(16396)	; D_FILE
	inc hl
	ld de, 33		; line size (might change later), this only works for 4k+ models

	ld a, b
	and a
	jr z, no_rows1

.loop_rows1
	add hl, de
	djnz loop_rows1

.no_rows1
	ld b, 0
	add hl, bc

	#endasm
}

// TODO FIX CRASH! todo with calling convension of stack use (possibly)
// study this more: https://github.com/z88dk/z88dk/wiki/The-Stack-Frame

int zx81_drawSpriteASM(uint16_t rowscols, uint16_t pos, uint16_t *spritemem) __smallc
{


    // on the zx81 each block is 2 "pixels" horizontally and 2 vertically pre encoded in the sprite memory
    //size of sprite in memory using bit pack is 16 * 16 = 256bits ==>>> 32bytes

    //hl = start of sprite memory
    //de = offset position in screen memory top left of sprite - no limit check done (yet)
    #asm
  
    ld hl,2
    add hl,sp              ; skip over return address on stack
    ld b,(hl)
    inc hl
    ld c,(hl)              ; bc = p
    inc hl
    ld d,(hl)
    inc hl
    ld e,(hl)
    push de
        inc hl
        ld d,(hl)
        inc hl
        ld e,(hl)
        
        push de
           pop hl
        
    pop de

.drawSprite
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
    #endasm
}


int __FASTCALL__ printOpeningScreen()
{
	#asm
	    call 0x0A2A   ; clear screen
	#endasm

    printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\n");
	printf("+++++++ zx81 spaceshooter+++++++\n\n");
    printf("    shoot everything!\n         avoiding the x\n\n");
	printf("++++++++++++++++++++++++++++++++\n");
	printf("            keys:\n       o=left p=right\n       q=up   a=down\n\n       press s to start\n\n\n");
    printf("          highscore\n              %d\n\n", highScore);
	printf("+++++ by a.pilkington 2024 ++++\n+++++ youtube: byteforever ++++\n+++++     version: 0.1     ++++");
}

int main()
{
	char control = '';
	uint16_t playerY = 23;
	uint16_t playerX = 15;
	uint16_t playerScreenPos = 0;
    uint16_t oldplayerScreenPos = 0;
    uint16_t score = 0;
	uint8_t restart = 0;
    uint8_t enemyChar = 61;
    uint8_t playerChar = 189;
    uint8_t mode = 0;
    uint8_t scoreSwitch = 0;
	uint8_t breakCountDown = 0;
	uint16_t level = 100;
	uint16_t delay = level;
	uint8_t bonus = 0;
	uint8_t bonusCountDown = 20;

    uint8_t  spritePos = 0;
    uint8_t  spriteRows = 8;
    uint8_t  spriteCols = 8;
// ok so using "goto", call the cops! but we have a mix of assembly anyway!
RESTART_LABEL:
    mode = 0;
    score = 0;
    scoreSwitch = 0;
    playerX = 15;
	playerY = 10;
    enemyChar = 61;
    playerChar = 189;
	breakCountDown = 0;
	level = 100;
	delay = level;
	bonus = 0;
	bonusCountDown = 20;
	printOpeningScreen();

    while (in_Inkey() != 'S')
	{
		// just hard loop waiting
	}

  	init_screen(0);

	while (1)
	{
#if 0
// for some reason this doesn't just delay for 2 frame cycles, perhaps the FRAMES variable is different
// in the z88dk zx81 runtime??
		#asm		
		ld b,2
waitForTVSync:	
		ld a,(0xf5a3)
		ld c,a
sync:
		ld a,(0xf5a3)
		cp c
		jr z,sync
		djnz waitForTVSync
		#endasm
#endif
		control = in_Inkey();

		switch (control)
		{
			case 'O' : if (playerX > 1) playerX -= 1; break;
			case 'P' : if (playerX < 31) playerX += 1; break;
			case 'Q' : if (playerY > 0) playerY -= 1; break;
			case 'A' : if (playerY < 23) playerY += 1; break;
			default: break;
		};

        while (delay-- > 0)
		{
		}
		delay = level;

        // update screen position based on player X Y 

        oldplayerScreenPos = playerScreenPos;
		playerScreenPos = zx81_saddr(combine(playerY,playerX));

		// we have to adjust the screen position based on Y position
		// bottom half needs nudging left above half nudge right
		if (bpeek(playerScreenPos) == enemyChar)
		{
			score = 0;
		#asm
		    ld b, 10
			ld c, 0
		    call 0x08F5    ; print at sets cursor position
		#endasm			
			printf("********* GAME OVER *********\n");
		#asm
		
		    ld b, 0xff 
loopDelay1:
				push b
				ld b, 0x8f
loopDelay2:
				djnz loopDelay2 
			    pop b
            djnz loopDelay1 
		#endasm	
			goto RESTART_LABEL;  // advised use of goto here is best way
		}
		if (bpeek(playerScreenPos) == 13) // check if got a dollar
		{
			score = score + 10;
            scoreSwitch = scoreSwitch + 1;
			if (score > highScore) 
            {
               highScore = score;
            }

            if (scoreSwitch >= 10) 
            {
			   if (level >= 0) level = level - 10;
			   if (level <= 0) bonus = 1;
			   breakCountDown = 30;
               scoreSwitch = 0;
               // every 200 we invert the player and enemy characters lol
               if (mode == 1)
               { 
                  enemyChar = 61;
                  playerChar = 189;
               } else
               {
                  enemyChar = 189;
                  playerChar = 61;
               }
               mode = 1 - mode; 
            }
		}

		bpoke (playerScreenPos, playerChar);

        zx81_drawSpriteASM(272, playerScreenPos, spriteMem);

        
		if (breakCountDown > 0)
		{
			breakCountDown--;
		} 
		else
		{
			if (bonus == 1) // just have $ no enemy
			{
				bonusCountDown--; 
				if (bonusCountDown <=0) 
				{
					bonusCountDown = 20;
					bonus = 0;
				} 
			}
		}
		
		#asm
		    xor a
			ld hl, 16418
			ld (hl), a
		    ld b, 23
			ld c, 0
		    call 0x08F5    ; print at sets cursor position
		#endasm
		printf("player pos %x", playerScreenPos);
		
	}
    
	return 1;
}
