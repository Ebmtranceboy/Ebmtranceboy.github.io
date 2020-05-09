instr PhaseAlignedFormant
  icps = p4
  kamp linsegr 0.01, 0.3, 0
  kformant linseg 2*icps, 1, 6*icps
  kbw linseg 1000, 0.33, 100, 0.33, 500, 0.33, 100

  kn = int(kformant/icps)
  ka = (kformant - kn * icps) / icps
  kg = exp(-icps / kbw)
  aphs phasor icps / 2
  a1 tablei 2 * aphs * kn, -1, 1, 0.25, 1
  a2 tablei 2 * aphs * (kn + 1), -1, 1, 0.25, 1
  asin tablei aphs, -1, 1, 0, 1
  amod = 1 / (1 + (2 * sqrt(kg) * asin / (1 - kg))^2)
  kscl = (1 + kg) / (1 - kg)
  acar = ka * a2 + (1 - ka) * a1
  aout = kamp * kscl * amod * acar

  sbus_mix(p5, aout, aout)
endin

