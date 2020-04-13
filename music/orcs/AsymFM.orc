instr AsymFM
  icps = p4
  kamp linsegr 0.4,0.3,0
  kmod = 4 * icps
  knx = 1.5
  kR linseg 1.5, 1, 0.5 ;symmetry control: < 1 -> spectral peak < icps, > 1 -> above 

  imx = 50
  iexp ftgen 0, 0, 131072, "exp", 0, -imx, 1
  kndx = knx * (kR + 1/kR) * 0.5
  kndx2 = knx * (kR - 1/kR) * 0.5
  afm oscili kndx/(2 * $M_PI), kmod, -1
  aph phasor icps
  afc tablei aph + afm, -1, 1, 0, 1
  amod oscili kndx2, kmod, -1, 0.25
  aexp tablei -(amod-abs(kndx2))/imx, iexp, 1

  aout = kamp * aexp * afc
  ; sum(rˆn J(k) sin(w_c + n w_m), n, - oo, + oo) = exp(k(r-1/r)cos(w_m)/2) sin(w_c + k(r+1/r)sin(w_m))
  sbus_mix(p5,aout,aout)
endin
