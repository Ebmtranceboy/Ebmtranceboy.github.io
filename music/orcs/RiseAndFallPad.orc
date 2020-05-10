instr RiseAndFallPad
  icps = p4
  asaw1 vco2 0.3, icps
  asaw2 vco2 0.3, icps * cent(3)
  kenv linsegr 0,0.2,1,0.05,0.7,0.5,0
  kenvf linsegr 0,0.05,icps,0.5,icps*30,5,icps,0.05,0
  afilt moogvcf (asaw1+asaw2),kenvf,0.1

  aout = kenv * afilt
  sbus_mix(p5, aout, aout)

endin
