instr FMKeys
  icps = p4
  aphm phasor icps

  index1=3+log(icps)/10000
  aph1 phasor (index1*icps)
  again1 linsegr 0,0.0006,1.4,2,0.2,5,0
  amod1 tablei aph1,-1,1,0,1
  amod1 = (amod1+1)/2

  index2=1+log(icps)/10000
  aph2 phasor (index2*icps)
  again2 linsegr 0,0.02,1,5,0
  amod2 tablei aph2,-1,1,0,1
  amod2 = (amod2+1)/2

  aout tablei aphm+again1*amod1+again2*amod2,-1,1,0,1
  adeclick linsegr 0.6,0.2,0.6,0.2,0
  aout = aout*adeclick
  alp tone aout,300
  ahp atone aout,300
  aout = (10*alp+7*ahp)/40
  sbus_mix(p5, aout,aout)
endin