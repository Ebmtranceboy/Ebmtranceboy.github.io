instr Decades
  icps = p4

  asaw vco2 0.2,icps
  aex moogvcf asaw, 16*icps, 0.6

  asaw8 vco2 0.2,icps*2.006
  aex8 moogvcf asaw8, 15*icps, 0.7

  kamp linsegr 1,0.4,0
  ain=aex+aex8
  
  sbus_mix(p5, kamp*ain, kamp*ain)
endin
