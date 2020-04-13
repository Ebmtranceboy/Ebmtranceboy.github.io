gi_Vampyr_exci ftgen 0,0,1024,-7,0.66346157, 152.6153869629, 0.875, 34.4615325928, 0, 329.8461914062, 0, 388.923034668, 0, 118.1538696289, 0

instr Vampyr
  icps = p4

  kexcindx loopseg 0,0,0,  0,0, 1,1024
  kexcigain table kexcindx, gi_Vampyr_exci

  ;kexenv expsegr 0.95, 0.1, 0.5, 0.05, 0.05
  kexenv expseg 0.999,0.5,0.2
  aex      vco2 0.1540000141, icps, 2, kexenv 
  ain = aex*kexcigain

  ka1 = 0.9687818289
  ktype = 0

  knoclick linsegr 1,0.1,0
  ka0 = (1 - ka1)/2

  isampsfrac = sr/icps
  isamps = int(isampsfrac)
  ifrac = isampsfrac-isamps

  aendb init 0

  adump delayr (isamps+1)/sr
  abegp deltap 0
  aendp0 deltapn isamps
  aendp1 deltapn (isamps+1)
  aendp = (1-ifrac) * aendp0 + ifrac*aendp1
  a1 delay1 aendp
  a2 delay1 a1
      delayw (aendb+ain)

  adumb delayr (isamps+1)/sr
  abegb deltap 0
  aendb1 deltapn isamps
  aendb0 deltapn (isamps-1)
  aendb = (1-ifrac) * aendb0 + ifrac*aendb1
  if (ktype == 0) then
     amix = ka0 * (aendp + a2) + ka1 * a1
  endif
  if (ktype == 1) then
    amix = ka0 * (aendp - a2) 
  endif
  ;if (ktype == 2) then
  ;  if (kblend &lt; ka1) then
  ;   amix = ka0 * (aendp + a2) + ka1 * a1
  ;  else
  ;   amix = -(ka0 * (aendp + a2) + ka1 * a1)
  ;  endif
  ;endif
       delayw amix*0.5;ka0*(aendp+(ktype==0?1:-1)*a2)+(ktype==0?ka1:0)*a1;(a1+aendp)/2


  aleft = knoclick*(abegp + aendb)
  aright = knoclick*(abegb + aendp)
  sbus_mix(p5, aleft, aright)
endin

