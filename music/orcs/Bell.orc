instr Bell
  icps = p4
  ifm = icps * 1.618
  kenv expsegr 0.4, 15, 0.0001, 1, 0.0001
  kmod expon 10 * ifm, 15, 0.001
  amod oscil kmod, ifm, -1
  aout oscil kenv, icps + amod, -1

  sbus_mix(p5, aout, aout)

endin

