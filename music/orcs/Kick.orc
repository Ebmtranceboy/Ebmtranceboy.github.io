instr Kick
  arnd_L random -1, 1
  arnd_R random -1, 1
  arndenv expseg 1, 0.02, 0.06, 0.02, 0.01, 0.12, 0.0001, 0.12, 0.00001

  alf expseg 600, 0.02,112, 0.14,50, 0.11,20, 0,20
  alfenv expseg 0.00001, 0.005,1, 0.16,1, 0.5,0.00001, 0, 0.00001
  asine oscili (1.0-arndenv)*alfenv, alf, -1

  aleft = arnd_L*arndenv+asine
  aright = arnd_R*arndenv+asine

  sbus_mix(p5, aleft, aright)
endin
