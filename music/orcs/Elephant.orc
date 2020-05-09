
instr Elephant
  icps = p4

  asaw vco2 0.3,icps
  asaw2 vco2 0.3,icps*1.0128496383
  asaw3 vco2 0.3,icps*0.9827973694
  aosc poscil 1,21,-1
  aout = (asaw+asaw2+asaw3)*(1+abs(aosc))*0.3
  sbus_mix(p5, aout, aout)
endin
