instr ResonantPad
  icps = p4
  asaw vco2 0.3, icps
  kfq lfo 0.01, 2
  atri vco2 0.3, icps * cent(5) * (1+kfq), 4, 0.5

  kenv linsegr 1,0.05,0.7,0.5,0
  kenvf linsegr icps,1, icps*6,2,icps*2,5,0

  afilt moogladder (asaw+atri), kenvf, 0.75

  aout = kenv * afilt

  sbus_mix(p5, aout, aout)

endin
