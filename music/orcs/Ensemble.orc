
chn_k "Ensemble.ktrl", 3, 2, 0.2, 0, 1

instr Ensemble
  ktrl chnget "Ensemble.ktrl"
  kcps = cpspch(p4)
  kwet0 linseg 0,0.233,1,1.6,0.0096,0.229,0
  ilfo = 0.04
  klfo lfo ilfo,5

  kwet = (kwet0*(1-2*ilfo)+klfo+ilfo)*ktrl*1.5
  asig1 pm_sine kcps,kwet
  asig2 pm_sine kcps*1.01,kwet
  asig = (asig1+asig2)/2
  kgain linsegr 0,0.02,0.05,0.16,0
  igain=3
  aout = igain*kgain*asig
  
  sbus_mix(p5, aout, aout)

endin

