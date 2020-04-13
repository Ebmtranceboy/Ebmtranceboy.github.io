instr Agonoize
  icps = p4
  kdf lfo icps/100,25
  kcps =icps+kdf
  icoef=1.007
  iepsilon=0.005
  ivel=0.3
  igain = 0.5

  kplay linseg 0.0001,0.001,0.01,0.3,0.1
  kplay0 linseg 0.0000001,0.15,0.04
  asaw0 vco2 0.25,kcps/(icoef+iepsilon)/icoef,4,kplay0
  asaw1 vco2 0.25,kcps/icoef,4,kplay0
  asaw2 vco2 0.25,kcps,4,kplay0
  asaw3 vco2 0.25,kcps*(icoef-iepsilon),4,kplay0
  asaw4 vco2 0.25,kcps*(icoef+iepsilon)*(icoef-iepsilon),4,kplay0
  asaw=asaw1+(asaw2+asaw3+asaw4)*10*(0.1-kplay);+asaw0

  abody linsegr 0,0.002,1,0.01,1,0.1,0
  arnd random -ivel,ivel
  aattack linseg 1,0.005,0
  asig=igain*(arnd*aattack+asaw*abody)
  apadl=asig
  apadr delay apadl,0.01

  sbus_mix(p5,apadl, apadr)
endin
