instr Logan
  awet linseg 0.93,5,0.03
  ;awet =0.76
  icps = p4

  imaster = 2*icps

  aphm phasor imaster

  aphm12 phasor imaster/2
  aphm2 phasor 2*imaster
  aenv12 = 2*aphm12-aphm ; pulse
  aenv2 = 2*aphm-aphm2
  alpha = 2-awet ; 1 <= alpha <= 2

  aphmod = 0.5-(aphm-0.5)
  afunc mirror alpha*aphmod,0,1
  aramp = alpha*((2-alpha)*aphmod - afunc)/(alpha-1)/2 ; ramp : [(0,0),(1/alpha,-1),(1,0)] (mode 1) or  [(0,0),(1-1/alpha,-1),(1,0)] (mode -1)
  asaw = -aramp*(1/alpha-0.5)                     ; saw  : [(0,1),(1/alpha,-1),(1,1)]  (mode 1) or  [(0,1),(1-1/alpha,-1),(1,1)]  (mode -1)

  afunc4 limit (1+1/(1-awet))*aphm,0,1
  apick mirror 2*afunc4,0,1
  abuzine = apick*(0.5-(1-awet)/(2-awet))

  ;aone table3 aphm + (irez==-1?asaw: (irez==-2?apulse: (irez==-3?abuzz: (irez==-4?apulsaw:abuzine))))+0.25,isine,1,0,1
  axone = aphm + abuzine

  au mirror 2*axone-0.5,-0.5,0.5
  aone = au*(4*au^2-3)

  ;atwo table3 aphm +(ishape==1?asaw:(ishape==2?apulse:(ishape==3?abuzz:(ishape==4?apulsaw:abuzine))))+0.25,isine,1,0,1
  axtwo = aphm + asaw

  av mirror 2*axtwo-0.5,-0.5,0.5
  atwo = av*(4*av^2-3)

  aout = (1-aenv12)*aone + aenv12*atwo
  ;aout=aone
  al = aout
  ar = aout
  again linsegr 0.06,0.5,0
  
  sbus_mix(p5, al*again, ar*again)
endin
