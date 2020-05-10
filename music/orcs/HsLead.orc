instr HsLead
  kmaster = p4
  kslave loopseg 1,0,0,1,5,2.3,8,1
  kbright expseg 1.5,0.5,0.01
  aout hs_sinebuzz kmaster, kslave, kbright
  asaw integ aout
  asaw dckill asaw
  again linsegr 0.5,0.01,0
  sbus_mix(p5, again*kbright*asaw,again*kbright*asaw)
endin
