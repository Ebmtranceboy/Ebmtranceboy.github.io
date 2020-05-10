instr GhostPad
  icps = p4
  ifm = p4/2 + 0.5
  aenv linsegr 0, 0.3, 0.9, 0.4, 0.9, 0.05, 0
  amd =1;linseg 1, 0.2, 0.5, 0.1, 0.5

  amod oscil 11*ifm*amd, ifm, -1
  aleft oscil aenv/3, icps + amod, -1
  aright oscil aenv/3, icps - amod, -1

  sbus_mix(p5, aleft, aright)

endin

