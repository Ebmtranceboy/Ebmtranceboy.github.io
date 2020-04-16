instr Dummy
; DX9 mode 6
;      _
;     |  |
;     4  |
;     |--
;    _|_
;  /  |  \
; 1   2  3
; |   |  |
; | _ |_ |
; 

  icps = p4*5

  adxr init 0.6
  adx1 linseg 1,1,0
  adx2 linseg 1,0.5,0,0.5,1
  adx3 linseg 2,0.1,0,0.5,2

  icr = 1
  ir = 3
  ic1 = 2
  ic2 = 1
  ic3 = 1

  kmodr init 0
  kphr init 0
  kph1 init 0
  kph2 init 0
  kph3 init 0
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

      kph1 += ic1*icps*(1+kdxr*kmodr)/sr
      kmod1 tab (kph1+1)%1,-1,1

      kph2 += ic2*icps*(1+kdxr*kmodr)/sr
      kmod2 tab (kph2+1)%1,-1,1

      kph3 += ic3*icps*(1+kdxr*kmodr)/sr
      kmod3 tab (kph3+1)%1,-1,1

      kfreq = icps*(1+kdx1*kmod1+kdx2*kmod2+kdx3*kmod3)
      kph += kfreq/sr
      ksig tab (kph+1)%1,-1,1

      vaset ksig, knsamp, asig
      knsamp += 1
      od

  aenv linsegr 1,0.2,0
  aout=aenv*asig/20

  sbus_mix(p5, aout, aout)
endin
