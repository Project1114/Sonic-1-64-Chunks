; ===========================================================================
; ---------------------------------------------------------------------------
; MJ - New draw code - By MarkeyJester - Special thanks to Jorge
; ---------------------------------------------------------------------------
HBufferSize	=	$0080
VBufferSize	=	$0040

d3_LT_NegTen:		reg	d3
d4_LT_PosX:		reg	d4
d5_LT_PosY:		reg	d5
d6_LT_BlockAddrBackup:	reg	d6
d7_LT_BlockNum:		reg	d7
d7_LT_BlockRemain:	reg	d7
a0_LT_ChunkAddr:	reg	a0
a1_LT_BlockAddr:	reg	a1
a2_LT_TileMapLine1:	reg	a2
a3_LT_TileMapLine2:	reg	a3
a3_LT_Scanline:		reg	a3
a4_LT_LayoutAddr:	reg	a4

; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to	draw only the BG void tiles
; ---------------------------------------------------------------------------
LoadTilesAsYouMove_BGOnly:
		lea	(vdp_data_port).l,a6
		lea	4(a6),a5
		moveq	#$FFFFFFF0,d3_LT_NegTen			; prepare -10
		bra	BGH_Draw				; continue
; ===========================================================================
; ---------------------------------------------------------------------------
; Drawing the FG Tiles in void space
; ---------------------------------------------------------------------------
LoadTilesAsYouMove:
		lea	(vdp_data_port).l,a6
		lea	4(a6),a5
		lea	(v_lvllayout).l,a4_LT_LayoutAddr	; load FG layout space
		move.w	#vram_fg-$8000,d6				; prepare VRAM plane address
		moveq	#$FFFFFFF0,d3_LT_NegTen			; prepare -10
; ---------------------------------------------------------------------------
; FG Horizontal
; ---------------------------------------------------------------------------
		move.w	(v_screenposx).w,d4_LT_PosX			; load X position
		move.w	(v_screenposy).w,d5_LT_PosY			; load Y position
		add.w	d3_LT_NegTen,d4_LT_PosX			; move X back into void space
		move.w	d5_LT_PosY,d0				; copy Y pos to d0
		and.w	d3_LT_NegTen,d0				; keep in range
		add.w	d3_LT_NegTen,d5_LT_PosY			; move Y back into void space
		move.w	(vLastFGYPos).w,d1			; load last Y position
		and.w	d3_LT_NegTen,d1				; keep in range
		move.w	d0,(vLastFGYPos).w			; update new last Y position
		sub.w	d1,d0		
		beq	FGH_NoDraw				; if it hasn't moved, branch
		bmi	FGH_UpDraw				; if it's moved up, branch
		addi.w	#$00F0,d5_LT_PosY			; increase Y down into lower void space

FGH_UpDraw:
		moveq	#$00,d7_LT_BlockRemain			; clear d7
		move.b	(v_bg1_xblock).w,d7_LT_BlockRemain	; load number of horizontal blocks to draw
		bsr	HorizLoadBlocks				; draw horizontal blocks

FGH_NoDraw:
; ---------------------------------------------------------------------------
; FG Vertical
; ---------------------------------------------------------------------------
		move.w	(v_screenposx).w,d4_LT_PosX			; load X position
		move.w	(v_screenposy).w,d5_LT_PosY			; load Y position
		add.w	d3_LT_NegTen,d5_LT_PosY			; move Y back into void space
		move.w	d4_LT_PosX,d0				; copy X pos to d0
		and.w	d3_LT_NegTen,d0				; keep in range
		add.w	d3_LT_NegTen,d4_LT_PosX			; move X back into void space
		move.w	(vLastFGXPos).w,d1			; load last X position
		and.w	d3_LT_NegTen,d1				; keep in range
		move.w	d0,(vLastFGXPos).w			; update new last X position
		sub.w	d1,d0					; minus last from new
		beq	FGV_NoDraw				; if it hasn't moved, branch
		bmi	FGV_LeftDraw				; if it's moved right, branch
		addi.w	#$0150,d4_LT_PosX			; increase X down into right void space

FGV_LeftDraw:
		moveq	#$00,d7_LT_BlockRemain			; clear d7
		move.b	(v_bg1_yblock).w,d7_LT_BlockRemain	; load number of vertical blocks to draw
		bsr	VertiLoadBlocks				; draw vertical blocks

FGV_NoDraw:
; ---------------------------------------------------------------------------
; Drawing the BG Tiles in void space
; ---------------------------------------------------------------------------
BGH_Draw:
		lea	(v_lvllayout+2).l,a4_LT_LayoutAddr	; load BG layout space
		move.w	#vram_bg-$8000,d6				; prepare VRAM plane address
; ---------------------------------------------------------------------------
; BG Horizontal
; ---------------------------------------------------------------------------
		move.w	(v_hscrolltablebuffer+2).w,d4_LT_PosX		; load X position (From scroll buffer)
		neg.w	d4_LT_PosX				; reverse
		subi.w	#$0010,d4_LT_PosX			; move X back into void space
		move.w	(v_bgscreenposy).w,d5_LT_PosY			; load Y position
		move.w	d5_LT_PosY,d0				; copy Y pos to d0
		and.w	d3_LT_NegTen,d0				; keep in range
		add.w	d3_LT_NegTen,d5_LT_PosY			; move Y back into void space
		move.w	(vLastBGYPos).w,d1			; load last Y position
		and.w	d3_LT_NegTen,d1				; keep in range
		move.w	d0,(vLastBGYPos).w			; update new last Y position
		sub.w	d1,d0		
		beq	BGH_NoDraw				; if it hasn't moved, branch
		bmi	BGH_UpDraw				; if it's moved up, branch
		addi.w	#$00F0,d5_LT_PosY			; increase Y down into lower void space
		move.w	(v_hscrolltablebuffer+$3C2).w,d4_LT_PosX		; load X position (From scroll buffer)
		neg.w	d4_LT_PosX				; reverse
		add.w	d3_LT_NegTen,d4_LT_PosX			; move X back into void space

BGH_UpDraw:
		moveq	#$00,d7_LT_BlockRemain			; clear d7
		move.b	(v_bg2_xblock).w,d7_LT_BlockRemain	; load number of horizontal blocks to draw
		bsr	HorizLoadBlocks				; draw horizontal blocks

BGH_NoDraw:
; ---------------------------------------------------------------------------
; BG Vertical
; ---------------------------------------------------------------------------
		move.w	(v_bgscreenposx).w,d0			; load X position
		move.w	(v_bgscreenposy).w,d5_LT_PosY			; load Y position
		add.w	d3_LT_NegTen,d5_LT_PosY					; move Y back into void space
		move.w	(vLastBGXPos).w,d1			; load last X position
		move.w	d0,(vLastBGXPos).w			; update new last X position
		sub.w	d1,d0		
		beq	BGV_NoDraw				; if it hasn't moved, branch
		bmi	BGV_LeftDraw				; if it's moved right, branch
		move.w	#$0150,d3_LT_NegTen			; set void space side to right

BGV_LeftDraw:
		lea	(v_hscrolltablebuffer+2).w,a3_LT_Scanline		; load scroll address
		moveq	#$00,d7_LT_BlockRemain			; clear d7
		move.b	(v_bg2_yblock).w,d7_LT_BlockRemain	; load number of vertical blocks to draw
		bra	VertiLoadBlocksScroll			; draw vertical blocks using scroll

BGV_NoDraw:
		rts						; return
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to	draw the entire level tiles on screen
; ---------------------------------------------------------------------------
LoadTilesFromStart:
		lea	(v_lvllayout).l,a4_LT_LayoutAddr	; load FG layout space
		move.w	#vram_fg-$8000,d6				; prepare VRAM plane address
		move.w	(v_screenposx).w,d4_LT_PosX				; load X position
		move.w	(v_screenposy).w,d5_LT_PosY				; load Y position
		bsr	LoadFGTilesFromStart			; draw FG
		lea	(v_lvllayout+2).l,a4_LT_LayoutAddr	; load BG layout space
		move.w	#vram_bg-$8000,d6				; prepare VRAM plane address
		move.w	(v_bgscreenposx).w,d4_LT_PosX			; load BG X position
		move.w	(v_bgscreenposy).w,d5_LT_PosY			; load BG Y position

; ---------------------------------------------------------------------------
; Subroutine to	draw only the entire BG tiles
; ---------------------------------------------------------------------------
LoadBGTilesFromStart:
		lea	(vdp_data_port).l,a6
		lea	4(a6),a5
		moveq	#$FFFFFFF0,d3_LT_NegTen				; prepare -10
		add.w	d3_LT_NegTen,d4_LT_PosX					; move X and Y positions back into void space
		add.w	d3_LT_NegTen,d5_LT_PosY					; ''
		moveq	#$F,d7					; set number of horizontal lines to draw
		lea	(v_hscrolltablebuffer+2).w,a3_LT_Scanline		; load scroll address
		move.b	d5_LT_PosY,d0					; load Y position
		andi.w	#$000F,d0				; get only within the block position
		add.b	d0,d0					; multiply by 4 (size of scanline data)
		add.b	d0,d0					; ''
		move.w	(a3_LT_Scanline),d4_LT_PosX					; load first scanline position (for first block only)
		suba.w	d0,a3_LT_Scanline					; shift scroll buffer address to correct beginning scanline of blocks
		bra	DABG_FirstBlock

DABG_NextLine:
		move.w	(a3_LT_Scanline),d4_LT_PosX					; load scroll value

DABG_FirstBlock:
		lea	$40(a3_LT_Scanline),a3_LT_Scanline	; advance to next 10 pixel block space
		neg.w	d4_LT_PosX				; reverse scroll value
		add.w	d3_LT_NegTen,d4_LT_PosX			; advance to correct side of screen (left side)
		movem.w	d4_LT_PosX/d7/a3_LT_Scanline,-(sp)	; store X pos and counter
		moveq	#$1F,d7					; set repeat times (number of blocks) (20 blocks)
		bsr	HorizLoadBlocks				; draw horizontal blocks
		movem.w	(sp)+,d4_LT_PosX/d7/a3_LT_Scanline	; restore X pos and counter
		sub.w	d3_LT_NegTen,d5_LT_PosY			; increase Y pos down
		dbf	d7,DABG_NextLine			; repeat til all lines are done
		rts						; return
; ---------------------------------------------------------------------------
; Subroutine to	draw only the entire FG tiles
; ---------------------------------------------------------------------------
LoadFGTilesFromStart:
		lea	(vdp_data_port).l,a6
		lea	4(a6),a5
		moveq	#$FFFFFFF0,d3_LT_NegTen				; prepare -10
		add.w	d3_LT_NegTen,d4_LT_PosX					; move X and Y positions back into void space
		add.w	d3_LT_NegTen,d5_LT_PosY					; ''
		moveq	#$0F,d7					; set number of horizontal lines to draw

DAFG_NextLine:
		movem.w	d4_LT_PosX/d7,-(sp)				; store X pos and counter
		moveq	#$1F,d7					; set repeat times (number of blocks) (20 blocks)
		bsr	HorizLoadBlocks				; draw horizontal blocks
		movem.w	(sp)+,d4_LT_PosX/d7				; restore X pos and counter
		sub.w	d3_LT_NegTen,d5_LT_PosY					; increase Y pos down
		dbf	d7,DAFG_NextLine			; repeat til all lines are done
		rts						; return
; ===========================================================================
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to load tiles from blocks to a buffer space
; ---------------------------------------------------------------------------
HorizLoadBlocks:
		moveq	#$1E,d0					; prepare maximum number of blocks minus 1
		sub.w	d7_LT_BlockNum,d0			; subtract number of blocks to draw
		move.w	d6,-(sp)				; store VRAM address
		move.w	#v_16x16,d6				; prepare block address
		move.w	d0,-(sp)				; store remaining blocks counter
		move.w	d4_LT_PosX,d0				; load X position
		andi.w	#$01F0,d0				; keep in range
		lsr.w	#$02,d0					; divide by 4 (every 10 pixels is 4 bytes of a double tile)
		lea	(vTileBuffer).w,a2			; load beginning of tile buffer
		adda.w	d0,a2_LT_TileMapLine1			; advance to correct tile buffer address
		lea	$80(a2_LT_TileMapLine1),a3_LT_TileMapLine2	; ''

		move.l	#v_16x16,d6_LT_BlockAddrBackup	; prepare block address

HLB_NextBlock:
		movea.l	d6_LT_BlockAddrBackup,a1_LT_BlockAddr	; load block address
		moveq	#0,d1
		move.w	d4_LT_PosX,d1				; load X position
		lsr.w	#6,d1
		add.w	d1,d1

		move.w	d5_LT_PosY,d0	
		lsr.w	#4,d0
		andi.w	#$FC,d0
		addq.w	#8,d0
		move.w	(a4_LT_LayoutAddr,d0.w),d0

		tst.w	d0
		bne.s	.valid
		move.w	#$108,d0

	.valid:
		add.w	d0,d1

		moveq	#0,d0
		move.w	(a4_LT_LayoutAddr,d1.w),d0		; load chunk ID

		move.w	d4_LT_PosX,d1					; load X position
		lsr.w	#$03,d1					; divide by 8
		andi.w	#6,d1

	;	btst	#$E,d0
	;	beq.s	.NoChunkMirror
	;	eor.w	#2,d1

	.NoChunkMirror:
		move.w	d5_LT_PosY,d2				; load Y position
		lsr.w	#1,d2
		andi.w	#$18,d2
		or.w	d2,d1					; save Y onto X

	;	btst	#$F,d0
	;	beq.s	.NoChunkFlip
	;	eor.w	#4,d1

	.NoChunkFlip:
		move.w	d0,d2
		andi.w	#$3FFF,d0				; keep in range
		lsl.l	#5,d0
		or.w	d1,d0					; save to chunk address
		move.l	(v_chunkloc).w,a0_LT_ChunkAddr
		add.l	d0,a0_LT_ChunkAddr			; set chunk address
		move.w	(a0_LT_ChunkAddr),d0			; load block ID
		andi.w	#$03FF,d0				; clear flags
		lsl.w	#$03,d0					; multiply by 8
		adda.l	d0,a1_LT_BlockAddr			; advance to correct block address

		move.l	(a1_LT_BlockAddr)+,d0			; load top two tiles
		move.l	(a1_LT_BlockAddr)+,d1			; load bottom two tiles

		btst	#$02,(a0_LT_ChunkAddr)			; was the block mirrored?
		beq	.NoBlockMirror				; if not, branch
		eori.l	#$08000800,d0				; set mirror bits
		eori.l	#$08000800,d1				; ''
		swap	d0					; swap tiles over
		swap	d1					; ''

	.NoBlockMirror:
		btst	#3,(a0_LT_ChunkAddr)			; was the block flipped?
		beq	.NoBlockFlip				; if not, branch
		eori.l	#$10001000,d0				; set flip bits
		eori.l	#$10001000,d1				; ''
		exg	d0,d1					; swap tiles over

	.NoBlockFlip:
	;	btst	#$E,d2					; was the chunk mirrored?
	;	beq	.NoChunkMirror2				; if not, branch
	;	eori.l	#$08000800,d0				; set mirror bits
	;	eori.l	#$08000800,d1				; ''
	;	swap	d0					; swap tiles over
	;	swap	d1					; ''

	.NoChunkMirror2:
	;	btst	#$F,d2					; was the chunk flipped?
	;	beq	.NoChunkFlip2				; if not, branch
	;	eori.l	#$10001000,d0				; set flip bits
	;	eori.l	#$10001000,d1				; ''
	;	exg	d0,d1					; swap tiles over

	.NoChunkFlip2:
		move.l	d0,(a2_LT_TileMapLine1)+		; save top two tiles
		move.l	d1,(a3_LT_TileMapLine2)+		; save bottom two tiles
		sub.w	d3_LT_NegTen,d4_LT_PosX			; increase X position to next block
		move.w	d4_LT_PosX,d0				; copy to d0
		andi.w	#$01F0,d0				; keep in range of the plane size
		bne	HLB_NoEnd				; if it has not wrapped back to the beginning, branch
		lea	(vTileBuffer).w,a2_LT_TileMapLine1	; reload tile buffer from beginning
		lea	$80(a2_LT_TileMapLine1),a3_LT_TileMapLine2	; ''

HLB_NoEnd:
		dbf	d7_LT_BlockNum,HLB_NextBlock		; repeat til done
		move.w	(sp)+,d7				; load remaining blocks counter
		bmi	HBL_NoClearBuffer			; if there's none, branch
		moveq	#$00,d0					; clear d0

HBL_ClearBuffer:
		move.l	d0,(a2_LT_TileMapLine1)+		; clear buffer
		move.l	d0,(a3_LT_TileMapLine2)+		; ''
		dbf	d7,HBL_ClearBuffer			; repeat til done

HBL_NoClearBuffer:
		move.w	(sp)+,d6				; restore VRAM address
		move.l	#$977F0000,d0				; prepare DMA source value
		move.w	d5,d0
		andi.w	#$00F0,d0				; keep in range
		lsl.w	#$04,d0					; multiply by 10 (every 10 pixels is VRAM 0100+)
		or.w	d6,d0			; set VRAM write bit
		move.l	#((((((HBufferSize*$02)/$02)<<$08)&$FF0000)+(((HBufferSize*$02)/$02)&$FF))+$94009300),(a5) ; set DMA size
		move.l	#((((((vTileBuffer&$FFFFFF)/$02)<<$08)&$FF0000)+(((vTileBuffer&$FFFFFF)/$02)&$FF))+$96009500),(a5) ; set DMA Source
		move.l	d0,(a5)					; set DMA source/destination
		move.w	#$0083,(a5)				; set DMA destination
		rts						; return
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to load tiles from blocks to a buffer space
; ---------------------------------------------------------------------------

VertiLoadBlocks:
		move.w	d5_LT_PosY,d0				; load Y position
		andi.w	#$00F0,d0				; keep in range
		lsr.w	#$02,d0					; divide by 4 (every 10 pixels is 4 bytes of a double tile)
		lea	(vTileBuffer).w,a2_LT_TileMapLine1
		adda.w	d0,a2_LT_TileMapLine1			; advance to correct tile buffer address
		lea	$80(a2_LT_TileMapLine1),a3_LT_TileMapLine2	; ''
		move.w	d6,-(sp)				; store VRAM location
		move.l	#v_16x16,d6_LT_BlockAddrBackup			; prepare block address

VLB_NextBlock:
		movea.l	d6_LT_BlockAddrBackup,a1_LT_BlockAddr	; load block address
		moveq	#0,d1
		move.w	d4_LT_PosX,d1				; load X position
		lsr.w	#6,d1
		add.w	d1,d1

		move.w	d5_LT_PosY,d0	
		lsr.w	#4,d0
		andi.w	#$FC,d0
		move.w	8(a4_LT_LayoutAddr,d0.w),d0

		tst.w	d0
		bne.s	.valid
		move.w	#$108,d0

	.valid:
		add.w	d0,d1

		moveq	#0,d0
		move.w	(a4_LT_LayoutAddr,d1.w),d0		; load chunk ID

		move.w	d4_LT_PosX,d1					; load X position
		lsr.w	#$03,d1					; divide by 8
		andi.w	#6,d1

	;	btst	#$E,d0
	;	beq.s	.nomirror
	;	eor.w	#2,d1

	.nomirror:

		move.w	d5_LT_PosY,d2					; load Y position
		lsr.w	#1,d2
		andi.w	#$18,d2
		or.w	d2,d1					; save Y onto X

	;	btst	#$F,d0
	;	beq.s	.noflip
	;	eor.w	#4,d1

	.noflip:
		move.w	d0,d2
		andi.w	#$3FFF,d0				; keep in range
		lsl.l	#5,d0
		or.w	d1,d0					; save to chunk address
		move.l	(v_chunkloc).w,a0_LT_ChunkAddr
		add.l	d0,a0_LT_ChunkAddr					; set chunk address
		move.w	(a0_LT_ChunkAddr),d0					; load block ID
		andi.w	#$03FF,d0				; clear flags
		lsl.w	#$03,d0					; multiply by 8
		adda.l	d0,a1_LT_BlockAddr					; advance to correct block address

		move.w	(a1_LT_BlockAddr)+,d0				; load left tile 1
		move.w	(a1_LT_BlockAddr)+,d1				; load right tile 1
		swap	d0					; send left
		swap	d1					; ''
		move.w	(a1_LT_BlockAddr)+,d0				; load left tile 2
		move.w	(a1_LT_BlockAddr)+,d1				; load right tile 2

		btst	#$02,(a0_LT_ChunkAddr)			; was the block mirrored?
		beq	.NoBlockMirror				; if not, branch
		eori.l	#$08000800,d0				; set mirror bits
		eori.l	#$08000800,d1				; ''
		exg	d0,d1					; swap tiles over

	.NoBlockMirror:
		btst	#3,(a0_LT_ChunkAddr)			; was the block flipped?
		beq	.NoBlockFlip				; if not, branch
		eori.l	#$10001000,d0				; set flip bits
		eori.l	#$10001000,d1				; ''
		swap	d0					; swap tiles over
		swap	d1					; ''

	.NoBlockFlip:
	;	btst	#$E,d2					; was the chunk mirrored?
	;	beq	.NoChunkMirror2				; if not, branch
	;	eori.l	#$08000800,d0				; set mirror bits
	;	eori.l	#$08000800,d1				; ''
	;	exg	d0,d1					; swap tiles over

	.NoChunkMirror2:
	;	btst	#$F,d2					; was the chunk flipped?
	;	beq	.NoChunkFlip2				; if not, branch
	;	eori.l	#$10001000,d0				; set flip bits
	;	eori.l	#$10001000,d1				; ''
	;	swap	d0					; swap tiles over
	;	swap	d1					; ''

	.NoChunkFlip2:
		move.l	d0,(a2_LT_TileMapLine1)+		; save top two tiles
		move.l	d1,(a3_LT_TileMapLine2)+		; save bottom two tiles

		sub.w	d3_LT_NegTen,d5_LT_PosY			; increase Y position to next block
		move.w	d5_LT_PosY,d0				; copy to d0
		andi.w	#$00F0,d0				; keep in range of the plane size
		bne	VLB_NoEnd				; if it has not wrapped back to the beginning, branch
		lea	(vTileBuffer).w,a2_LT_TileMapLine1	; reload tile buffer from beginning			; ''
		lea	$80(a2_LT_TileMapLine1),a3_LT_TileMapLine2	; ''

VLB_NoEnd:
		dbf	d7,VLB_NextBlock			; repeat til done
		move.w	(sp)+,d6				; restore VRAM location
		move.l	#$977F0000,d0				; prepare DMA source value
		move.w	d4_LT_PosX,d0				; load X position
		andi.w	#$01F0,d0				; keep in range
		lsr.w	#$02,d0					; divide by 2 (every 10 pixels is VRAM 0002+)
		or.w	d6,d0					; set VRAM write bit
		move.w	#$8F80,(a5)				; set auto increment to 80 (next line)
		move.l	#(((((VBufferSize/$02)<<$08)&$FF0000)+((VBufferSize/$02)&$FF))+$94009300),(a5) ; set DMA size
		move.l	#((((((vTileBufferA&$FFFFFF)/$02)<<$08)&$FF0000)+(((vTileBufferA&$FFFFFF)/$02)&$FF))+$96009500),(a5) ; set DMA Source
		move.l	d0,(a5)					; set DMA source/destination
		move.w	#$0083,(a5)				; set DMA destination
		addq.w	#$02,d0					; advance to tile on right in VRAM address
		move.l	#(((((VBufferSize/$02)<<$08)&$FF0000)+((VBufferSize/$02)&$FF))+$94009300),(a5) ; set DMA size
		move.l	#((((((vTileBufferB&$FFFFFF)/$02)<<$08)&$FF0000)+(((vTileBufferB&$FFFFFF)/$02)&$FF))+$96009500),(a5) ; set DMA Source
		move.l	d0,(a5)					; set DMA source/destination
		move.w	#$0083,(a5)				; set DMA destination
		move.w	#$8F02,(a5)				; set auto increment back to 02 (word)
		rts						; return
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to load tiles from blocks to a buffer space
; ---------------------------------------------------------------------------
VertiLoadBlocksScroll:
		move.l	#$00800000,a2				; prepare a2 as line shift register value
		move.b	d5_LT_PosY,d0				; load Y position
		andi.w	#$000F,d0				; get only within the block position
		add.b	d0,d0					; multiply by 4 (size of scanline data)
		add.b	d0,d0					; ''
		move.w	(a3_LT_Scanline),d4_LT_PosX		; load first scanline position (for first block only)
		suba.w	d0,a3_LT_Scanline			; shift scroll buffer address to correct beginning scanline of blocks
		bra	VLBS_FirstBlock				; continue

VLBS_NextBlock:
		move.w	(a3_LT_Scanline),d4_LT_PosX		; load scroll value

VLBS_FirstBlock:
		lea	$40(a3_LT_Scanline),a3_LT_Scanline	; advance to next 10 pixel block space
		neg.w	d4_LT_PosX				; reverse scroll value
		add.w	d3_LT_NegTen,d4_LT_PosX			; advance to correct side of screen
		lea	(v_16x16).w,a1_LT_BlockAddr		; load block address
		moveq	#0,d1
		move.w	d4_LT_PosX,d1				; load X position
		lsr.w	#6,d1
		add.w	d1,d1

		move.w	d5_LT_PosY,d0	
		lsr.w	#4,d0
		andi.w	#$FC,d0
		move.w	8(a4_LT_LayoutAddr,d0.w),d0
		move.w d0,d2

		tst.w	d0
		bne.s	.valid
		move.w	#$108,d0

	.valid:
		add.w	d0,d1

		moveq	#0,d0
		move.w	(a4_LT_LayoutAddr,d1.w),d0		; load chunk ID

		move.w	d4_LT_PosX,d1					; load X position
		lsr.w	#$03,d1					; divide by 8
		andi.w	#6,d1

	;	btst	#$E,d0
	;	beq.s	.nomirror
	;	eor.w	#2,d1

	.nomirror:
		move.w	d5_LT_PosY,d2					; load Y position
		lsr.w	#1,d2
		andi.w	#$18,d2
		or.w	d2,d1					; save Y onto X

	;	btst	#$F,d0
	;	beq.s	.noflip
	;	eor.w	#4,d1

	.noflip:
	;	move.w	d0,d2
		andi.w	#$3FFF,d0				; keep in range
		lsl.l	#5,d0
		or.w	d1,d0					; save to chunk address
		move.l	(v_chunkloc).w,a0_LT_ChunkAddr
		add.l	d0,a0_LT_ChunkAddr			; set chunk address
		move.w	(a0_LT_ChunkAddr),d0			; load block ID
		andi.w	#$03FF,d0				; clear flags
		lsl.w	#$03,d0					; multiply by 8
		adda.l	d0,a1_LT_BlockAddr			; advance to correct block address

		move.w	d4_LT_PosX,d2				; load X position
		andi.w	#$01F0,d2				; keep within 200 pixels in multiples of 10
		lsr.w	#$02,d2					; divide by 4
		move.w	d5_LT_PosY,d1				; load Y position
		andi.w	#$00F0,d1				; keep within 100 pixels in multiples of 10
		lsl.w	#$04,d1					; multiply by 10
		or.w	d1,d2					; save Y onto X
		or.w	d6,d2					; save VRAM address
		swap	d2					; send left
		move.w	#$0003,d2

		move.l	(a1_LT_BlockAddr)+,d0			; load top two tiles
		move.l	(a1_LT_BlockAddr)+,d1			; load bottom two tiles
		btst	#$02,(a0_LT_ChunkAddr)			; was the block mirrored?
		beq	VLBS_NoMirror				; if not, branch
		eori.l	#$08000800,d0				; set mirror bits
		eori.l	#$08000800,d1				; ''
		swap	d0					; swap tiles over
		swap	d1					; ''

VLBS_NoMirror:
		btst	#$03,(a0_LT_ChunkAddr)			; was the block flipped?
		beq	VLBS_NoFlip				; if not, branch
		eori.l	#$10001000,d0				; set flip bits
		eori.l	#$10001000,d1				; ''
		exg	d0,d1					; swap tiles over

VLBS_NoFlip:
	;	btst	#$E,d2				; was the block mirrored?
	;	beq	.NoMirror				; if not, branch
	;	eori.l	#$08000800,d0				; set mirror bits
	;	eori.l	#$08000800,d1				; ''
	;	swap	d0					; swap tiles over
	;	swap	d1					; ''

	.NoMirror:
	;	btst	#$0F,d2				; was the block flipped?
	;	beq	.NoFlip				; if not, branch
	;	eori.l	#$10001000,d0				; set flip bits
	;	eori.l	#$10001000,d1				; ''
	;	exg	d0,d1					; swap tiles over

	.NoFlip:
		move.l	d2,(a5)
		move.l	d0,(a6)	
		add.l	a2,d2
		move.l	d2,(a5)					; save top two tiles
		move.l	d1,(a6)					; save top two tiles
		addi.w	#$0010,d5_LT_PosY			; increase Y position to next block
		dbf	d7,VLBS_NextBlock			; repeat til done
		rts						; return
; ===========================================================================