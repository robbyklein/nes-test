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
    inc $01
	  RTI


.endproc
	
.import reset_handler

.export main
.proc main
  ; prep to load pallets
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR

load_palettes:
  lda palettes, x
  sta PPUDATA
  inx
  cpx #$04
  bne load_palettes

  ; Load
  ldx #$00
load_spirtes:
  lda sprites, x
  sta $0200, x
  inx
  cpx #$04
  bne load_spirtes
  

    lda #$00
    sta $01 ; scroll

lda #$20
sta $09 ; page
lda #$00
sta $08; end
ldy #$00 ; position
page:
  tiles:
    LDA PPUSTATUS
    LDA $09 ; offset
    STA PPUADDR
    STY PPUADDR
    LDX #$02
    STX PPUDATA
    iny
    cpy $08
    bne tiles
  inc $09
  lda $09
  cmp #$23
  bne page


  lda #$c0
  sta $08  
:
  LDA PPUSTATUS
  LDA $09 ; offset
  STA PPUADDR
  STY PPUADDR
  LDX #$03
  STX PPUDATA
  iny
  cpy $08
  bne :-
  

ldy #$00
:
  LDA PPUSTATUS
  LDA #$24 ; offset
  STA PPUADDR
  STY PPUADDR
  LDX #$01
  STX PPUDATA
  iny
  cpy $08
  bne :-





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
palettes:
.byte $31, $16, $09, $0f

sprites:
.byte $70, $09, $00, $80


.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CHR"
.incbin "graphics2.chr"

.segment "STARTUP"

