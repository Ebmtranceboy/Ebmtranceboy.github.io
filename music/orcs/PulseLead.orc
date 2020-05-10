instr PulseLead
  kcps = p4

  kbright expsegr 0.79,0.41,0.13,0.1,0.43
  ;kbright expsegr 1,0.1,0.2,0.1,0.43
  krat linseg 0.09,0.25,0.25
  alead pulseleadcore kcps,kbright,krat
  igain = 0.05
  sbus_mix(p5, igain*alead,igain*alead)
endin
