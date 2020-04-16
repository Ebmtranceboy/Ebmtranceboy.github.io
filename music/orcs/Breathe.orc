instr Breathe
; DX9 mode 8
;            _
;           |  |
; 1  2 3 4  |
; |  |  |   |_|
; | _|_ |_ |
; 
  icps = p4

  adx1 linseg 18,2,0
  adx2 linseg 12,0.5,6,0.5,1
  adx3 linseg 5,0.1,5,0.5,2
  adxr linseg 1,1,0

  ic1 = 2
  ic2 = 3
  ic3 = 6
  icr = 25
  ir = 0.15

  kph1 init 0
  kph2 init 0
  kph3 init 0
  kmodr init 0
  kphr init 0
  kph init 0

  asig = 0
  knsamp = 0
  while knsamp < ksmps do
      kdx1 vaget knsamp, adx1
      kdx2 vaget knsamp, adx2
      kdx3 vaget knsamp, adx3
      kdxr vaget knsamp, adxr

      kph1 += ic1*icps/sr
      kmod1 tab (kph1+1)%1,-1,1

      kph2 += ic2*icps/sr
      kmod2 tab (kph2+1)%1,-1,1

      kph3 += ic3*icps/sr
      kmod3 tab (kph3+1)%1,-1,1

      kfreqr = icr *icps * (1 + ir*kmodr)
      kphr += kfreqr/sr
      kmodr tab (kphr+1)%1,-1,1

      kfreq = icps*(1+kdx1*kmod1+kdx2*kmod2+kdx3*kmod3+kdxr*kmodr)
      kph += kfreq/sr
      ksig tab (kph+1)%1,-1,1

      vaset ksig, knsamp, asig
      knsamp += 1
      od

  aenv linsegr 1,0.2,0
  aout=aenv*asig/20

  sbus_mix(p5, aout, aout)
endin
