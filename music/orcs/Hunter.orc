instr Hunter
; DX9 mode 2
;
;     _
;   |  |
;   4  |
; 3 |--
;  \/
;  2
;  |
;  1
;  |
;

  icps = p4

  adx3 linseg 12,2,0
  adxr linseg 1,7,0
  adx2 linseg 3,0.2,0,0.2,2
  adx1 linseg 10,0.1,6,0.5,2
  ic3 = 20
  icr = 9
  ir = 0.005
  ic2 = 3
  ic1 = 2

  kph3 init 0
  kmodr init 0
  kphr init 0
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

      kph3 += ic3*icps/sr
      kmod3 tab (kph3+1)%1,-1,1

      kfreqr = icr *icps * (1 + ir*kmodr)
      kphr += kfreqr/sr
      kmodr tab (kphr+1)%1,-1,1

      kph2 += ic2*icps*(1+kdx3*kmod3+kdxr*kmodr)/sr
      kmod2 tab (kph2+1)%1,-1,1

      kph1 += ic1*icps*(1+kdx2*kmod2)/sr
      kmod1 tab (kph1+1)%1,-1,1

      kfreq = icps*(1+kdx1*kmod1)
      kph += kfreq/sr
      ksig tab (kph+1)%1,-1,1

      vaset ksig, knsamp, asig
      knsamp += 1
      od

  aenv linsegr 0,0.03,1,0.2,0
  aout=aenv*asig*0.3

  sbus_mix(p5, aout, aout)
endin
