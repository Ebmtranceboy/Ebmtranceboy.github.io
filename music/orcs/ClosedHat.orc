instr ClosedHat
  icps = 1000
  aenv expon 1,0.2,0.0001
  a0 rand aenv
  aout atone a0,icps
  sbus_mix(p5, aout, aout)
endin
