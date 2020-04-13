instr BandlimitedSum
  icps = p4
  kamp linsegr 0.005, 0.2, 0
  kmod = 4 * icps
  ka linseg 0.89, 3, 0.3
  kndx = 1.5

  aphw phasor icps
  apht phasor kmod

  a1 tablei aphw, -1, 1
  a2 tablei aphw - apht, -1, 1, 0, 1
  a3 tablei aphw + (kndx+1) * apht, -1, 1, 0, 1
  a4 tablei aphw + kndx * apht, -1, 1, 0, 1
  acos tablei apht, -1, 1, 0, 1
  kpw pow ka, kndx+1
  ksq = ka * ka
  aden = 1 - 2*ka*acos +ksq
  asig = (a1 - ka*a2 - kpw * (a3 - ka*a4))/aden
  ; sum(aˆk sin(w+kb), k, 0, n) = [sin(w) - a sin(w-b) - aˆ(n+1) (sin(w+(n+1)b) - a sin(w+nb))] / (1 + aˆ2 - 2a cos(b))  

  knorm = sqrt((1-ksq)/(1-kpw*kpw))
  aout = knorm * kamp * asig

  sbus_mix(p5,aout, aout)
endin
