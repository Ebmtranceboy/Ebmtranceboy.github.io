instr Spine
  icps = p4

  icoef=1.005
  iepsilon=0.002
  ivel=0.3
  igain = 0.2

  asaw0 vco2 0.25,icps/(icoef+iepsilon)/icoef
  asaw1 vco2 0.25,icps/icoef
  asaw2 vco2 0.25,icps
  asaw3 vco2 0.25,icps*(icoef-iepsilon)
  asaw4 vco2 0.25,icps*(icoef+iepsilon)*(icoef-iepsilon)
  asaw=asaw1+asaw2+asaw3+asaw4;+asaw0

  abody linsegr 0,0.01,1,0.01,1,0.1,0
  arnd random -ivel,ivel
  aattack linseg 1,0.005,0
  apad=igain*(arnd*aattack+asaw*abody)

  sbus_mix(p5, apad, apad)


endin

