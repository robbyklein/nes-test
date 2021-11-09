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
	  LDA #$00
	  STA $2005
	  STA $2005
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
  


  ; load background
;   ldx #$04
;   stx $07 ; spirte counter
;   ldy #$00
; load:
; 	LDA PPUSTATUS
; 	LDA #$20
; 	STA PPUADDR
; 	tya
;   STA PPUADDR
;   ldx $07
; 	STX PPUDATA
;   jsr nextSprite
;   inc $09
;   iny
;   bne load
  
lda #$20
sta $09 ; page

; Top row
ldy #$00
:
  LDA PPUSTATUS
  LDA $09 ; offset
  STA PPUADDR
  STY PPUADDR
  LDX #$01
  STX PPUDATA
  iny
  cpy #$40
  bne :-

; walls
ldy #$40
:
  ; left
  LDA PPUSTATUS
  LDA $09 ; offset
  STA PPUADDR
  STY PPUADDR
  LDX #$01
  STX PPUDATA

  ; right
  tya
  adc #$1e
  tay
  LDA PPUSTATUS
  LDA #$20 ; offset
  STA PPUADDR
  STY PPUADDR
  LDX #$01
  STX PPUDATA
  tya
  adc #$01
  tay
  cpy #$00
  jsr check_page
  bne :-


  

; floor
ldy #$80
:
  ; left
  LDA PPUSTATUS
  LDA #$23 ; offset
  STA PPUADDR
  STY PPUADDR
  LDX #$01
  STX PPUDATA
  iny
  cpy #$bf
  bne :-




;;;;;;;;;;;;;;;

  ; ldy #$5f
  ; LDA PPUSTATUS
  ; LDA #$20 ; offset
  ; STA PPUADDR
  ; STY PPUADDR
  ; LDX #$01
  ; STX PPUDATA

; :
;   LDA PPUSTATUS
;   LDA #$20 ; offset
;   STA PPUADDR
;   STY PPUADDR
;   LDX #$01
;   STX PPUDATA
;   cpy #$00
;   tya
;   adc #$20
;   tay
;   bne :-




; right
; ldy #$5f
; :
;   LDA PPUSTATUS
;   LDA #$20 ; offset
;   STA PPUADDR
;   STY PPUADDR
;   LDX #$01
;   STX PPUDATA
;   tya
;   adc #$1f
;   tay
;   cpy #$fe
;   bne :-

; :
;   LDA PPUSTATUS
;   LDA #$21 ; offset
;   STA PPUADDR
;   LDA $07 ; position
;   STA PPUADDR
;   LDX #$05
;   STX PPUDATA
;   inc $07
;   bne :-

; :
;   LDA PPUSTATUS
;   LDA #$22 ; offset
;   STA PPUADDR
;   LDA $07 ; position
;   STA PPUADDR
;   LDX #$06
;   STX PPUDATA
;   inc $07
;   bne :-

; :
;   LDA PPUSTATUS
;   LDA #$23 ; offset
;   STA PPUADDR
;   LDA $07 ; position
;   STA PPUADDR
;   LDX #$02
;   STX PPUDATA
;   inc $07
;   lda $07
;   cmp #$c0
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

check_page:
  cpy #$00
  beq sss
  rts

  sss:
    lda $09
    cmp #$23
    beq ss
    inc $09
    rts

  ss:
    rts


  rts

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

