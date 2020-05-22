instr StretchDrum
  ; p3 ~ 0.5
  kamp0 linseg 0.6, 0.002*p3/0.5, 1, 0.004*p3/0.5, 0.4, 0.001*p3/0.5, 1, 0.04*p3/0.5, 1, 0.01*p3/0.5, 0.5, 0.25*p3/0.5, 0
  knoisamp linseg 0.5, 0.005*p3/0.5, 0.5, 0.001*p3/0.5, 0.1, 0.015*p3/0.5, 0.1, 0.025*p3/0.5, 0.2, 0.01*p3/0.5, 0.5, 0.2*p3/0.5, 0.5
  kfreq linseg 20, 0.01*p3/0.5, 233, 0.5*p3/0.5, 233
  kamp1 linseg 0,0.02*p3/0.5,1,0.02*p3/0.5,0
  atri vco2 1, kfreq, 12
  awhite random -1, 1
  kfcut linseg 20, 1*p3/0.5, 20000
  anoise atone awhite, kfcut
  
  aout = kamp0 * (knoisamp * (anoise) + kamp1 * atri)
  
   aout delay aout, 0.2
 
  sbus_mix(p5, aout, aout)
  
endin