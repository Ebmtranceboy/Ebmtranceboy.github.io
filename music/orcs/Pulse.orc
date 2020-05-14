instr Pulse
  kcps = p4
  apulse fm_pulse kcps,1,0.618
  aenv expseg 1,0.9,0.001
  asig  =            0.07*(apulse)*aenv
  sbus_mix(p5, asig,asig)
endin
