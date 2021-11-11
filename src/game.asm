.include "header.inc"
.include "constants.inc"

.segment "ZEROPAGE"
  world: .res 2

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
	  LDA #$00
	  STA OAMADDR

	  LDA #$02
	  STA OAMDMA
	  LDA $01
	  STA $2005
    lda #$00
	  STA $2005
    ;inc $01
	  RTI


.endproc
	
.import reset_handler

.export main
.proc main
  ;;;;;;;;;;;;;;;;;;;;;;;
  ; Variables
  ;;;;;;;;;;;;;;;;;;;;;;;
  lda #$00
  sta $01 ; scroll position



  ;;;;;;;;;;;;;;;;;;;;;;;
  ; prep to load pallets
  ;;;;;;;;;;;;;;;;;;;;;;;
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR

  ;;;;;;;;;;;;;;;;;;;;;;;
  ; Load Palettes
  ;;;;;;;;;;;;;;;;;;;;;;;
; load_palettes:
;   lda colors, x
;   sta PPUDATA
;   inx
;   cpx #$04
;   bne load_palettes

 

LoadPalettes:
    LDA colors, X
    STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
    INX
    CPX #$20
    BNE LoadPalettes    

  ;;;;;;;;;;;;;;;;;;;;;;;
  ; Load Sprites
  ;;;;;;;;;;;;;;;;;;;;;;;
  ldx #$00
load_spirtes:
  lda sprites, x
  sta $0200, x
  inx
  cpx #$04
  bne load_spirtes
  


  ldx #$20
  ldy #$00 ; position

  lda #<bg
  sta 3
  lda #>bg
  sta 4

; Background
page:
  tiles:
    LDA PPUSTATUS
    STX PPUADDR
    STY PPUADDR
    lda (3),y
    STa PPUDATA
    iny
    bne tiles

  inc 4
  inx
  cpx #$24
  bne page
  

; :
;   LDA PPUSTATUS
;   LDA $09 ; offset
;   STA PPUADDR
;   STY PPUADDR
;   lda (3),y
;   inx
;   STa PPUDATA
;   iny
;   cpy #$bf
;   bne :-

; colors!
; :
;   LDA PPUSTATUS
;   LDA #$23
;   STA PPUADDR
;   LDA #$f1
;   STA PPUADDR
;   lda (3),y
;   STA PPUDATA
;   iny
;   bne :-






vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  JMP forever

nextSprite:
  inc $07
  ldx $07
  cpx #$1e
  beq sub
  rts

  sub:
    ldx #$04
    stx $07
    rts  




.endproc

.segment "RODATA"
colors:
  .incbin "ccc.pal"
  ; .byte $22,$29,$1A,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F  ;background palette data
  ; .byte $22,$16,$27,$18,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17  ;sprite palette data

sprites:
.byte $70, $09, $00, $80

bg:
  .incbin "test.nam"


.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CHR"
.incbin "g.chr"

.segment "STARTUP"

