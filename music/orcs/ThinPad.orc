instr ThinPad
  icps = p4
  kpwm lfo 0.3, 0.8 
  apuls vco2 0.6, icps, 2, 0.5+kpwm
  kenv linsegr 0, 0.1, 1, 0.02, 0.6, 1, 0
  kenvf lfo 0.2, 0.1
  afilt moogladder apuls, (1+kenvf)*icps*20,0.4

  aout = kenv * afilt

  sbus_mix(p5, aout, aout)

endin
