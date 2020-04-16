gi_Guitar_full ftgen 0,0,8192,13,0.5,1,0,1,0,1/3,0,1/5,0,1/7,0,1/9,0,1/11,0,1/13,0,1/15,0,1/17,0,1/19,0,1/21,0,1/23,0,1/25,0,1/27
gi_Guitar_dist ftgen 0,0,513,13,1,1,0,100,0,-33,0,20,0,-14.2,0,11.1,0,-9.09,0,7.69,0,-6.67,0,5.88,0,-5.26 ; quasi square wave transfer function

instr Guitar
  icps = p4
  iwet=0.76
  ifond=0.56
  asig0 oscil 1,icps/2.001,-1
  afull0 table asig0,gi_Guitar_full,1,0.5
  adist0 table afull0,gi_Guitar_dist,1,0.5

  asig1 oscil 1,icps,-1
  afull1 table asig1,gi_Guitar_full,1,0.5
  adist1 table afull1,gi_Guitar_dist,1,0.5

  afull=ifond*afull0+(1-ifond)*afull1
  adist=ifond*adist0+(1-ifond)*adist1

  igain=0.3
  aenv linsegr 0,0.001,igain,0.3,0
  aout=aenv*(iwet*adist+(1-iwet)*afull)
  kfc linsegr 8*icps,1,4*icps,5,2*icps,1,50
  aout moogladder aout,kfc,0.8

  sbus_mix(p5, aout, aout)
endin
