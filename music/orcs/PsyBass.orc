instr PsyBass
  icps = p4
  alvl expseg 1, 0.05, 0.5, 0.45, 0.00003
  iamp = 0.1
  a1 vco iamp * alvl, icps, 1, 0.5, -1
  a2 vco iamp * alvl, icps * 1.01, 1, 0.5, -1
  a3 vco iamp * alvl, icps * 0.99, 1, 0.5, -1
  asig sum a1, a2, a3
  acut linseg 1, 0.03, 0.1
  afilt moogvcf2 asig, 5000 * acut, 0.4
  aleft, aright freeverb afilt, afilt, 0.8, 0.35

  sbus_mix(p5, afilt + 0.04 * aleft, afilt + 0.04 * aright)

endin
