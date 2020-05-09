    
instr Naive
  icps = p4
  afond phasor icps
  ah3 phasor 3*icps
  ah5 phasor 5*icps
  asig1 table afond,-1,1
  asig3 table ah3,-1,1
  asig5 table ah5,-1,1
  aenv linsegr 0,abs(p3)/200,1,0.5,0
  aout = aenv*(asig1+asig3/3+asig5/5)/10

  sbus_mix(p5, aout, aout)
endin

