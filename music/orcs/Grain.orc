instr   Grain
  kamp        linsegr 0.02, 0.1, 0
  icps        =       p4
  kdens       =       icps/4
  kgdur       expsegr 0.1, 0.5, 0.06, 1, 0.01
  ifn         ftgen   0, 0, 1024, 10, 1, 0.5, 0.333, 0.25
  iwfn        ftgen   0, 0, 1024, 20, 2, 1; hanning

  aout        grain   kamp, icps, kdens, 0, 0, kgdur, ifn, iwfn, 1

  sbus_mix(p5, aout, aout)
endin

