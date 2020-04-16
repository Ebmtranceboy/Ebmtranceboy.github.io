instr PsyKick
  a1 linseg 1.5, 0.2, 0.5
  a2 linseg 2, 0.06, 1
  a3 linseg 4, 0.01, 1
  a4 linseg 4, 0.002, 1
  alvl linsegr 0, 0.0002, 1, 0.03, 0.8, 0.08, 0.7, 0.5-0.1102, 0
  aout oscili alvl*0.5, a1*a2*a3*a4*44, -1
  sbus_mix(p5, aout, aout)
endin

