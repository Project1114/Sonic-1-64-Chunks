; ---------------------------------------------------------------------------
; Subroutine to	make Sonic run around loops (GHZ/SLZ)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B	R O U T	I N E |||||||||||||||||||||||||||||||||||||||


Sonic_Loops:
	; The name's a misnomer: loops are no longer handled here, only the windtunnels. Loops are dealt with by pathswappers
	;	cmpi.b	#id_SLZ,(v_zone).w ; is level SLZ ?	; MJ: Commented out, we don't want SLZ having any rolling chunks =P
	;	beq.s	.isstarlight	; if yes, branch
		tst.b	(v_zone).w	; is level GHZ ?
		bne.w	.noloops	; if not, branch

;.isstarlight:
		move.w	obY(a0),d2		; MJ: Load Y position
		move.w	obX(a0),d3		; MJ: Load X position
		lea	(v_lvllayout).l,a1		; grab level layout
		moveq	#-1,d0				; make upper word of d0 $FFFF
		move.w	d2,d0				; make d0 the y position
		asr.w	#5,d0				; divide y by $10
		andi.w	#$7E,d0				; make it multiples of 4 (equivalent to dividing by $40 and multiplying by 4)
	;	move.w	(Layout_row_index_mask).w,d1
	;	bne.s	.addit
	;	andi.w	#$FF,d0
	;	bra.s	.skip

	;.addit:
	;	add.w	d1,d0

	;.loop:
	;	sub.w	d1,d0
	;	bpl.s	.loop
	;	add.w	d1,d0

	.skip:
		move.w	4(a1,d0.w),d0			; get the location of the section of chunks in the layout on the y level
		tst.w	d0
		bne.s	.valid
		move.w	#$108,d0

	.valid:
		move.w	d3,d1				; make d1 the x position
		lsr.w	#6,d1				; divide x by $40
		add.w	d1,d1				; double d1
		add.w	d1,d0				; go to x position in level layout
		movea.l	d0,a1				; set lower half of address
		move.w	(a1),d1				; get chunk id

		lea	STunnel_Chunks_End(pc),a2			; MJ: lead list of S-Tunnel chunks
		moveq	#(STunnel_Chunks_End-STunnel_Chunks)/2-1,d2	; MJ: get size of list

.loop:
		cmp.w	-(a2),d1	; MJ: is the chunk an S-Tunnel chunk?
		dbeq	d2,.loop	; MJ: check for each listed S-Tunnel chunk
		beq.w	Sonic_ChkRoll	; MJ: if so, branch

.noloops:
		rts	
; End of function Sonic_Loops

; ===========================================================================
STunnel_Chunks:		; MJ: list of S-Tunnel chunks
		dc.w	$2C,$2D,$2E
		dc.w	$39,$3A,$3B
		dc.w	$46,$47,$48
		dc.w	$62,$63,$64
		dc.w	$84,$85,$86
		dc.w	$A9,$AA
STunnel_Chunks_End:
