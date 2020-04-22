instr SnareDrum
  kamp0 linseg 0.6, 0.002, 1, 0.004, 0.4, 0.001, 1, 0.04, 1, 0.01, 0.5, 0.25, 0
  knoisamp linseg 0.5, 0.005, 0.5, 0.001, 0.1, 0.015, 0.1, 0.025, 0.2, 0.01, 0.5, 0.2, 0.5
  kfreq linseg 20, 0.01, 233, 0.5, 233
  kamp1 linseg 0,0.02,1,0.02,0
  atri vco2 1, kfreq, 12
  awhite random -1, 1
  kfcut linseg 20, 1, 20000
  anoise atone awhite, kfcut
  
  aout = kamp0 * (knoisamp * (anoise) + kamp1 * atri)
  sbus_mix(p5, aout, aout)
  
endin
