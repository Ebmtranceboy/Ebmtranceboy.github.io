
gi_snare_raw ftgen 0,0,1024,7,0,256,-1,512,1,256,0
gi_snare_fn ftgen 0,0,1024,30,gi_snare_raw,1,10

instr Snare
  icps = 85
  arnd_L random -1, 1
  arnd_L tone arnd_L, 6000
  arndenv expseg 1,    0.007,1,   0.01,0.2, 0.05,0.2, 0.01,0.5, 0.5,0.5
  arndlfo lfo 0.15, icps*1.5
  aenvlfo expseg 0.00001, 0.005, 0.00001, 0.01, 1, 0.05, 1, 0.01, 00001, 0, 0.00001
  amp = arndenv + aenvlfo * arndlfo
  
  aenv expseg 1, 0.05,1, 0.9, 0.0001, 0,0.0001
  
  alfenv expseg 0.01,  0.001,0.4, 0.05, 0.4, 0.12, 0.001, 0, 0.001
  asine oscili alfenv*(1-amp),icps,gi_snare_fn
  aleft = (arnd_L*amp+asine)*aenv
  
  out aleft * p4
  
endin
