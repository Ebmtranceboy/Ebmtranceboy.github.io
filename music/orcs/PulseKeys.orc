instr PulseKeys
  kcps = p4
  apulse fm_pulse kcps,1,0.25
  aenv expseg 1,0.8,0.001
  asig  =            0.04*apulse*aenv
  sbus_mix(p5, asig,asig)
endin
