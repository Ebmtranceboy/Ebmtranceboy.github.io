instr Resonance1

  imaster = p4
  kwet linseg 0.95,3,0.5
  khs = 1 / (1-kwet)
  kslave = khs * imaster
  anosync init 0
  aphm,areset syncphasor imaster,anosync
  aphs, asyncout syncphasor kslave, areset
  asine table aphs,-1,1,0.75,1
  asig = (1-aphm) * (1+asine)
  again linsegr 0,0.02,0.2,0.5,0
  aout = again * asig

  sbus_mix(p5, aout, aout)
endin

