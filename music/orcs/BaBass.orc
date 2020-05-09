instr BaBass
  icps = p4/2
  
  aenv linseg 0, p3/40, 1, 39*p3/80, 1, 39*p3/80, 0
  asig vco2 1, icps/2, 20, 0.5, 0
  asig += vco2(0.25, 2*icps, 18, 0.7, 0.5)
  asig += vco2(0.0625, 4*icps, 18, 0.45, 0)
  asig += vco2(0.0125, 8*icps, 18, 0.45, 0)
  
  asig *= aenv

  kcutoff expseg 500, 0.01, 16000, 0.2, 500, 0.1, 400
  asig moogvcf asig, kcutoff, 0.7
  
  sbus_mix(p5, asig, asig)
endin
