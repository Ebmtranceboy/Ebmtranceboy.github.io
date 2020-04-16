instr DxKeys
; DX9 mode 1
; 
;   _
;  |  |
;  4  |
;  |--
;  3
;  | 
;  2
;  |
;  1
;  |
;
  icps = p4

  adxr linseg 1,7,0
  adx3 linseg 2,2,0
  adx2 linseg 5,0.2,4,0.2,2
  adx1 linseg 10,0.5,6,0.5,2

  icr = 2
  ir = 2
  ic3 = 49
  ic2 = 3
  ic1 = 2

  kmodr init 0
  kphr init 0
  kph3 init 0
  kph2 init 0
  kph1 init 0
  kph init 0

  asig = 0
  knsamp = 0
  while knsamp < ksmps do
      kdxr vaget knsamp, adxr
      kdx1 vaget knsamp, adx1
      kdx2 vaget knsamp, adx2
      kdx3 vaget knsamp, adx3

      kfreqr = icr *icps * (1 + ir*kmodr)
      kphr += kfreqr/sr
      kmodr tab (kphr+1)%1,-1,1

      kph3 += ic3*icps*(1+kdxr*kmodr)/sr
      kmod3 tab (kph3+1)%1,-1,1

      kph2 += ic2*icps*(1+kdx3*kmod3)/sr
      kmod2 tab (kph2+1)%1,-1,1

      kph1 += ic1*icps*(1+kdx2*kmod2)/sr
      kmod1 tab (kph1+1)%1,-1,1

      kfreq = icps*(1+kdx1*kmod1)
      kph +=  kfreq/sr
      ksig tab (kph+1)%1,-1,1

      vaset ksig, knsamp, asig
      knsamp += 1
      od

  aenv linsegr 1,0.2,0
  aout=aenv*asig/7

  sbus_mix(p5, aout, aout)
endin
