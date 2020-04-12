instr Enough
  ; DX9 mode 3
  ; 
  ;      _
  ; 3  |  |
  ; |  4  |
  ; 2  |--
  ;  \/
  ;  1
  ;  |
  ;
  icps = p4

  adx3 init 1
  adx2 linseg 9,0.1,6,0.5,2
  adxr linseg 1,2,0
  adx1 linseg 2,2,0

  ic3 = 20
  ic2 = 4
  icr = 9
  ir = 0.0001
  ic1 = 3

  kph3 init 0
  kph2 init 0
  kmodr init 0
  kphr init 0
  kph1 init 0
  kph init 0

  asig = 0
  knsamp = 0
  while knsamp < ksmps do

      kdx1 vaget knsamp, adx1
      kdx2 vaget knsamp, adx2
      kdx3 vaget knsamp, adx3
      kdxr vaget knsamp, adxr

      kph3 += ic3*icps/sr
      kmod3 tab (kph3+1)%1,-1,1

      kph2 += ic2*icps*(1+kdx3*kmod3)/sr
      kmod2 tab (kph2+1)%1,-1,1

      kfreqr = icr *icps * (1 + ir*kmodr)
      kphr += kfreqr/sr
      kmodr tab (kphr+1)%1,-1,1

      kph1 += ic1*icps*(1+kdx2*kmod2+kdxr*kmodr)/sr
      kmod1 tab (kph1+1)%1,-1,1

      kfreq = icps*(1+kdx1*kmod1)
      kph += kfreq/sr
      ksig tab (kph+1)%1,-1,1

      vaset ksig, knsamp, asig
      knsamp += 1
      od

  aenv linsegr 1,0.2,0
  aout=aenv*asig/2

  ;out aout
  pan_verb_mix(aout, 0.5, 0.99)
endin
