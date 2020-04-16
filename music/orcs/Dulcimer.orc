instr Dulcimer
  icps = p4

  adx1 linseg 18,2,0
  adx2 linseg 12,0.5,3,0.5,6
  adx3 linseg 5,0.1,5,0.5,2

  ic1 = 5
  ic2 = 3
  ic3 = 7

  kph1 init 0
  kph2 init 0
  kph3 init 0
  kph init 0

  asig = 0
  knsamp = 0
  while knsamp < ksmps do
      kdx1 vaget knsamp, adx1
      kdx2 vaget knsamp, adx2
      kdx3 vaget knsamp, adx3

      kph1 += ic1*icps/sr
      kmod1 table (kph1+1)%1,-1,1,0,1

      kph2 += ic2*icps/sr
      kmod2 table (kph2+1)%1,-1,1,0,1

      kph3 += ic3*icps/sr
      kmod3 table (kph3+1)%1,-1,1,0,1


      kfreq=icps*(1+kdx1*kmod1+kdx2*kmod2+kdx3*kmod3)
      kph += kfreq/sr
      ksig table (kph+1)%1,-1,1,0,1

      vaset ksig, knsamp, asig
      knsamp += 1
      od

  aenv linsegr 1,0.2,0
  aout=aenv*asig/8

  sbus_mix(p5, aout, aout)
endin
