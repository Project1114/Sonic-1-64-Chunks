; ---------------------------------------------------------------------------
; Subroutine to	find which tile	the object is standing on

; input:
;	d2 = y-position of object's bottom edge
;	d3 = x-position of object

; output:
;	a1 = address within 128x128 mappings where object is standing
;	     (refers to a 16x16 tile number)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B	R O U T	I N E |||||||||||||||||||||||||||||||||||||||


FindNearestTile: 
		lea	(v_lvllayout).l,a1		; grab level layout
		moveq	#-1,d0				; make upper word of d0 $FFFF
		move.w	d2,d0				; make d0 the y position
		asr.w	#4,d0				; divide y by $10
		andi.w	#$FC,d0				; make it multiples of 4 (equivalent to dividing by $40 and multiplying by 4)
	;	move.w	(Layout_row_index_mask).w,d1
	;	bne.s	.addit
	;	andi.w	#$FF,d0
	;	bra.s	.skip

	.addit:
	;	add.w	d1,d0

	.loop:
	;	sub.w	d1,d0
	;	bpl.s	.loop
	;	add.w	d1,d0

	.skip:
		move.w	8(a1,d0.w),d0			; get the location of the section of chunks in the layout on the y level
		tst.w	d0
		bne.s	.valid
		move.w	#$108,d0

	.valid:

		move.w	d3,d1				; make d1 the x position
		lsr.w	#3,d1				; divide x by 4
		move.w	d1,d4				; store in d4
		lsr.w	#3,d1				; divide x by $10 ($40 overall)
		add.w	d1,d1				; double d1
		add.w	d1,d0				; go to x position in level layout
		moveq	#0,d1				; clear d1
		movea.l	d0,a1				; set lower half of address
		move.w	(a1),d1				; get chunk id
	;	add.w	d1,d1				; double it
	;	move.w	ChunkAddrArray(pc,d1.w),d1	; multiply by $80
		lsl.w	#5,d1				; multiply by $20
		move.w	d2,d0				; get y position
		lsr.w	d0				; divide by 2
		andi.w	#$18,d0				; loop after $20 and make multiples of 8
		add.w	d0,d1				; go to y block section within chunk
		andi.w	#6,d4				; and with 6
		add.w	d4,d1				; go to block within chunk
		movea.l	(v_chunkloc).w,a1		; get location of chunk data
		adda.l	d1,a1				; make that the output address
		rts
; End of function FindNearestTile
; ---------------------------------------------------------------------------
ChunkAddrArray:	dc.w     0,  $80, $100, $180, $200, $280, $300, $380, $400, $480, $500, $580, $600, $680, $700, $780
		dc.w  $800, $880, $900, $980, $A00, $A80, $B00, $B80, $C00, $C80, $D00, $D80, $E00, $E80, $F00, $F80
		dc.w $1000,$1080,$1100,$1180,$1200,$1280,$1300,$1380,$1400,$1480,$1500,$1580,$1600,$1680,$1700,$1780
		dc.w $1800,$1880,$1900,$1980,$1A00,$1A80,$1B00,$1B80,$1C00,$1C80,$1D00,$1D80,$1E00,$1E80,$1F00,$1F80
		dc.w $2000,$2080,$2100,$2180,$2200,$2280,$2300,$2380,$2400,$2480,$2500,$2580,$2600,$2680,$2700,$2780
		dc.w $2800,$2880,$2900,$2980,$2A00,$2A80,$2B00,$2B80,$2C00,$2C80,$2D00,$2D80,$2E00,$2E80,$2F00,$2F80
		dc.w $3000,$3080,$3100,$3180,$3200,$3280,$3300,$3380,$3400,$3480,$3500,$3580,$3600,$3680,$3700,$3780
		dc.w $3800,$3880,$3900,$3980,$3A00,$3A80,$3B00,$3B80,$3C00,$3C80,$3D00,$3D80,$3E00,$3E80,$3F00,$3F80
		dc.w $4000,$4080,$4100,$4180,$4200,$4280,$4300,$4380,$4400,$4480,$4500,$4580,$4600,$4680,$4700,$4780
		dc.w $4800,$4880,$4900,$4980,$4A00,$4A80,$4B00,$4B80,$4C00,$4C80,$4D00,$4D80,$4E00,$4E80,$4F00,$4F80
		dc.w $5000,$5080,$5100,$5180,$5200,$5280,$5300,$5380,$5400,$5480,$5500,$5580,$5600,$5680,$5700,$5780
		dc.w $5800,$5880,$5900,$5980,$5A00,$5A80,$5B00,$5B80,$5C00,$5C80,$5D00,$5D80,$5E00,$5E80,$5F00,$5F80
		dc.w $6000,$6080,$6100,$6180,$6200,$6280,$6300,$6380,$6400,$6480,$6500,$6580,$6600,$6680,$6700,$6780
		dc.w $6800,$6880,$6900,$6980,$6A00,$6A80,$6B00,$6B80,$6C00,$6C80,$6D00,$6D80,$6E00,$6E80,$6F00,$6F80
		dc.w $7000,$7080,$7100,$7180,$7200,$7280,$7300,$7380,$7400,$7480,$7500,$7580,$7600,$7680,$7700,$7780
		dc.w $7800,$7880,$7900,$7980,$7A00,$7A80,$7B00,$7B80,$7C00,$7C80,$7D00,$7D80,$7E00,$7E80,$7F00,$7F80
		even