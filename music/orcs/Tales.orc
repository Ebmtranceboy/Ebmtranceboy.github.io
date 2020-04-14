instr Tales
  icps = p4
  araw vco2 1,icps
  araw0 vco2 1,icps/2
  adeclick linsegr 1,0.05,0
  kcutoff linseg 2000,1,500
  irez = 0.4
  asig moogvcf araw, kcutoff, irez
  asig0 moogvcf araw0, kcutoff,irez
  adel delay asig,30
  aout = adeclick*(asig0+adel)
  sbus_mix(p5, aout, aout)
endin
