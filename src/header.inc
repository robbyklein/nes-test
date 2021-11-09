.segment "HEADER"
  .byte "NES"
  .byte $1a
  .byte $02 ; 2 * 16 prg rom
  .byte $01 ; 8kb chr rom
  .byte %00000001 ; mapper and mirroring
  .byte $00
  .byte $00
  .byte $00
  .byte $00
  .byte $00, $00, $00, $00, $00 ; filler bytes