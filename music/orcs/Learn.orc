chn_k "Learn.detune", 3, 2, 0, 0, 1

;instr Mod
;	klfo lfo 8.2, 0.1
; chnset 0.5 + klfo, "Learn.detune"
;endin

;start("Mod")


instr Learn
  icps = p4
  kdetune chnget "Learn.detune"
  asaw1 vco2 1, icps
  asaw2 vco2 1, (1-kdetune) * icps + kdetune * icps * 1.03

  asig = (asaw1 + asaw2) /3
  aenv linsegr 0, 0.05, 1, 0.1, 0.7, 1, 0.1, 0.2,0
  aout = asig * aenv
  aout moogladder aout, icps*10, 0.8
  sbus_mix(p5, aout, aout)

endin
