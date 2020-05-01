sr=44100
ksmps=32
0dbfs=1
nchnls=2

gi_Hocico_exciter ftgen 0,0,1024,-7,0.31,94,0.67,94,0,15,0.77,5,0.38,98,0.25,74,0.53,79,0.42,49,0.15,10,0,167,0.46,128,0.06,211,0
gi_Hocico_pitchshifter_triangle ftgen 0,0,513,20,3 

instr Hocico
  icps = p4

  kmaster = 0.1
  kamp  =    kmaster * 5
  kexciter_gain = 0.82

  knh = int (sr / 2 / icps)
  aex buzz 1.5, icps, knh, -1

  kexcindx linseg 0, 0.4 * log(icps) / 3, ftlen(gi_Hocico_exciter)
  kexcigain table kexcindx, gi_Hocico_exciter

  ain = kexciter_gain * aex * kexcigain

  ka1 = 0.70
  ka0 = (1 - ka1) / 2

  anoclick linsegr 1,0.3,0

  isampsfrac = sr/icps
  isamps = int(isampsfrac)
  ifrac = isampsfrac-isamps
  ilen = isamps + 2
  kbufb[] init ilen
  kidx init 0
  kbufp[] init ilen
  kn init 0

  k1 init 0
  kendb init 0
  kendp init 0

  awgl = 0
  awgr = 0

  ksmp = 0
  while ksmp < ksmps do
      kn = (kidx + 0) % ilen
      kbegp = kbufp[kn]
      kn = (kidx + ilen - isamps) % ilen
      kendp0 = kbufp[kn]
      kn = (kidx + ilen - isamps - 1) % ilen
      kendp1 = kbufp[kn]
      k2 = k1
      k1 = kendp
      kendp = (1-ifrac) * kendp0 + ifrac * kendp1
      kin vaget ksmp, ain
      kn = kidx % ilen
      kbufp[kn] = kendb + kin

      kn = (kidx + 0) % ilen
      kbegb = kbufb[kn]
      kn = (kidx + ilen - isamps + 1) % ilen
      kendb0 = kbufb[kn]
      kn = (kidx + ilen - isamps) % ilen
      kendb1 = kbufb[kn]
      kendb = (1-ifrac) * kendb0 + ifrac * kendb1
      kmix = ka0 * (kendp + k2) + ka1 * k1
      kn = (kidx) % ilen
      kbufb[kn] = kmix

      knoclick vaget ksmp, anoclick
      vaset knoclick * (kbegp + kendb), ksmp, awgl
      vaset knoclick * (kbegb + kendp), ksmp, awgr

      kidx += 1
      kidx = kidx % ilen

      ksmp += 1
      od

  asig = (awgl + awgr) / 2
  imaxdel = 0.1
  idur = int(sr * imaxdel)
  ibasehz = cpsoct(8) ; USE MIDDLE C AS BASIS

  insemis = -12
  ifract = insemis / 12          ; FRACTION OF AN OCTAVE
  inewhz = cpsoct(8 + ifract)    ; REL FREQ OF NEW PITCH
  iratio = inewhz / ibasehz      ; RATIO NEW HZ TO OLD
  idelrate = (iratio - 1) / imaxdel

  avdeln phasor -idelrate       ; 1 to 0
  avdel2n phasor -idelrate, 0.5 ; 1/2 offset
  afaden tablei avdeln, gi_Hocico_pitchshifter_triangle, 1, 0, 1
  afade2n tablei avdel2n, gi_Hocico_pitchshifter_triangle, 1, 0, 1
  kbufn[] init idur
  kidxn init 0
  knn init 0
  asigneg = 0

  ksmpn = 0
  while ksmpn < ksmps do
      kvdeln vaget ksmpn, avdeln
      kvdel2n vaget ksmpn, avdel2n
      knn = int(kidxn +idur- kvdeln * imaxdel * sr) % idur
      ktap1n = kbufn[knn]
      knn = int(kidxn + idur-kvdel2n * imaxdel * sr) % idur
      ktap2n = kbufn[knn]
      kfaden vaget ksmpn, afaden
      kfade2n vaget ksmpn, afade2n
      kmixn = ktap1n * kfaden + ktap2n * kfade2n
      ksig vaget ksmpn, asig
      knn = kidxn % idur
      kbufn[knn] = ksig

      vaset kmixn, ksmpn, asigneg
      kidxn += 1
      kidxn = kidxn % idur
      ksmpn += 1
      od

  insemis = 12
  ifract = insemis / 12          ; FRACTION OF AN OCTAVE
  inewhz = cpsoct(8 + ifract)    ; REL FREQ OF NEW PITCH
  iratio = inewhz / ibasehz      ; RATIO NEW HZ TO OLD
  idelrate = (iratio - 1) / imaxdel

  avdelp phasor -idelrate       ; 1 to 0
  avdel2p phasor -idelrate, 0.5 ; 1/2 offset
  afadep tablei avdelp, gi_Hocico_pitchshifter_triangle, 1, 0, 1
  afade2p tablei avdel2p, gi_Hocico_pitchshifter_triangle, 1, 0, 1
  kbuf[] init idur
  kidxp init 0
  knp init 0
  asigpos = 0

  ksmpp = 0
  while ksmpp < ksmps do
      kvdelp vaget ksmpp, avdelp
      kvdel2p vaget ksmpp, avdel2p
      knp = int(kidxp + idur -kvdelp * imaxdel * sr) % idur
      ktap1p = kbuf[knp]
      knp = int(kidxp + idur -kvdel2p * imaxdel * sr) % idur
      ktap2p = kbuf[knp]
      kfadep vaget ksmpp, afadep
      kfade2p vaget ksmpp, afade2p
      kmixp = ktap1p * kfadep + ktap2p * kfade2p
      ksig vaget ksmpp, asig
      knp = kidxp % idur
      kbuf[knp] = ksig

      vaset kmixp, ksmpp, asigpos
      kidxp += 1
      kidxp = kidxp % idur
      ksmpp += 1
      od

  aleft = kamp * (awgl + asigneg + asigpos)
  aright = kamp * (awgr + asigneg + asigpos)
  
  sbus_mix(p5, aleft, aright)
endin

instr Logan
  awet linseg 0.93,5,0.03
  ;awet =0.76
  icps = p4

  imaster = 2*icps

  aphm phasor imaster

  aphm12 phasor imaster/2
  aphm2 phasor 2*imaster
  aenv12 = 2*aphm12-aphm ; pulse
  aenv2 = 2*aphm-aphm2
  alpha = 2-awet ; 1 <= alpha <= 2

  aphmod = 0.5-(aphm-0.5)
  afunc mirror alpha*aphmod,0,1
  aramp = alpha*((2-alpha)*aphmod - afunc)/(alpha-1)/2 ; ramp : [(0,0),(1/alpha,-1),(1,0)] (mode 1) or  [(0,0),(1-1/alpha,-1),(1,0)] (mode -1)
  asaw = -aramp*(1/alpha-0.5)                     ; saw  : [(0,1),(1/alpha,-1),(1,1)]  (mode 1) or  [(0,1),(1-1/alpha,-1),(1,1)]  (mode -1)

  afunc4 limit (1+1/(1-awet))*aphm,0,1
  apick mirror 2*afunc4,0,1
  abuzine = apick*(0.5-(1-awet)/(2-awet))

  ;aone table3 aphm + (irez==-1?asaw: (irez==-2?apulse: (irez==-3?abuzz: (irez==-4?apulsaw:abuzine))))+0.25,isine,1,0,1
  axone = aphm + abuzine

  au mirror 2*axone-0.5,-0.5,0.5
  aone = au*(4*au^2-3)

  ;atwo table3 aphm +(ishape==1?asaw:(ishape==2?apulse:(ishape==3?abuzz:(ishape==4?apulsaw:abuzine))))+0.25,isine,1,0,1
  axtwo = aphm + asaw

  av mirror 2*axtwo-0.5,-0.5,0.5
  atwo = av*(4*av^2-3)

  aout = (1-aenv12)*aone + aenv12*atwo
  ;aout=aone
  al = aout
  ar = aout
  again linsegr 0.06,0.5,0
  
  sbus_mix(p5, al*again, ar*again)
endin

instr SnareDrum
  kamp0 linseg 0.6, 0.002, 1, 0.004, 0.4, 0.001, 1, 0.04, 1, 0.01, 0.5, 0.25, 0
  knoisamp linseg 0.5, 0.005, 0.5, 0.001, 0.1, 0.015, 0.1, 0.025, 0.2, 0.01, 0.5, 0.2, 0.5
  kfreq linseg 20, 0.01, 233, 0.5, 233
  kamp1 linseg 0,0.02,1,0.02,0
  atri vco2 1, kfreq, 12
  awhite random -1, 1
  kfcut linseg 20, 1, 20000
  anoise atone awhite, kfcut
  
  aout = kamp0 * (knoisamp * (anoise) + kamp1 * atri)
  sbus_mix(p5, aout, aout)
  
endin

instr Kick
  arnd_L random -1, 1
  arnd_R random -1, 1
  arndenv expseg 1, 0.02, 0.06, 0.02, 0.01, 0.12, 0.0001, 0.12, 0.00001

  alf expseg 600, 0.02,112, 0.14,50, 0.11,20, 0,20
  alfenv expseg 0.00001, 0.005,1, 0.16,1, 0.5,0.00001, 0, 0.00001
  asine oscili (1.0-arndenv)*alfenv, alf, -1

  aleft = arnd_L*arndenv+asine
  aright = arnd_R*arndenv+asine

  sbus_mix(p5, aleft, aright)
endin

instr Decades
  icps = p4

  asaw vco2 0.2,icps
  aex moogvcf asaw, 16*icps, 0.6

  asaw8 vco2 0.2,icps*2.006
  aex8 moogvcf asaw8, 15*icps, 0.7

  kamp linsegr 1,0.4,0
  ain=aex+aex8
  
  sbus_mix(p5, kamp*ain, kamp*ain)
endin

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
  aout = aenv*asig/2

  sbus_mix(p5, aout, aout)
endin

instr Mixer
  al, ar  sbus_read 1
  al, ar reverbsc al, ar, 0.8, 2000
  out(al, ar)
  sbus_clear(1)

  al, ar  sbus_read 2
  al, ar reverbsc al, ar, 0.95, 3000
  out(al*0.7, ar*0.7)
  sbus_clear(2)
  
  al, ar  sbus_read 3
  al, ar reverbsc al, ar, 0.75, 2000
  out(al/2, ar/2)
  sbus_clear(3)

  al, ar  sbus_read 4
  al, ar reverbsc al, ar, 0.95, 10000
  aenv = 1/4+oscil:a(1/7,0.125)
  out(al*aenv, ar*aenv)
  sbus_clear(4)

  adelL	init 0
  adelR init 0
  
  al, ar  sbus_read 5
  
  adelL delay al + adelL * 0.8, 0.5
  adelR delay ar + adelR * 0.81, 0.5
  
  al, ar reverbsc adelR, adelR, 0.8, 20000
  out(al/2, ar/2)
  sbus_clear(5)

endin



gitrig = 0

instr P1
  
  if gitrig == 0 then
		gk_clock_tick = 0
    	gitrig = 1
  endif
 
  Sinstrs[] fillarray "Hocico" , "Logan", "SnareDrum", "Kick", "Decades", "Enough"
  ioct_melody = 5
  ivoxsteps[] fillarray 8, 8, 1, 12, 16, 6
  itrigs[] fillarray bits(array(0,1,1,0,1,0)),
                     bits(array(1,1,1,0,1,0)),
  					 bits(array(1,1,1,1,1,0)),
  					 bits(array(1,1,1,1,1,1))
 
  ivoxes[] fillarray 0, 24, 1, p3*61, cpsmidinn(36+0), 1,
                     0, 40,0,0,0,0,
                     0, 24, 1, p3*61, cpsmidinn(24+10), 1,
                     0, 40,0,0,0,0,
                     0, 24, 1, p3*61, cpsmidinn(36+7), 1,
  					 0, 40,0,0,0,0,
  					 0, 24, 1, p3*61, cpsmidinn(36+8), 1,
					 0, 40,0,0,0,0,
  \
    				2, 8, 1, p3*3, 0, 1,
    				2, 8, 2, p3*3, 0, 1,
    				2, 8, 0, p3*3, 0, 1,
    				2, 8, 130, p3*3, 0, 1,
    				2, 8, 1, p3*3, 0, 1,
    				2, 8, 130, p3*3, 0, 1,
    				2, 8, 0, p3*3, 0, 1,
    				2, 8, 130, p3*3, 0, 1,
  \
    				3, 16, 2, p3*2, 0, 3,
  \
                \;    4, 256,0,0,0,0,
  					4, 24, 1, p3*32, cpsmidinn(72+2), 4,
  					4, 8,0,0,0,0,
                    4, 24, 1, p3*24, cpsmidinn(72+0), 4,
                    4, 8, 1, p3*8, cpsmidinn(60+10), 4,
                    4, 24, 1, p3*64, cpsmidinn(60+5), 4,
                    4, 40,0,0,0,0,
                    4, 24, 1, p3*32, cpsmidinn(72+2), 4,
                    4, 8,0,0,0,0,
                    4, 24, 1, p3*24, cpsmidinn(60+11), 4,
                    4, 8, 1, p3*8, cpsmidinn(60+7), 4,
                    4, 24, 1, p3*64, cpsmidinn(72+0), 4,
                    4, 40,0,0,0,0,
 \
    				5, 8, 2, p3, cpsmidinn(12*ioct_melody+2),5,
    				5, 14, 2, p3, cpsmidinn(12*ioct_melody+7),5,
    				5, 2, 2, p3, cpsmidinn(12*ioct_melody+3),5,
    				5, 10, 2, p3, cpsmidinn(12*ioct_melody+7),5,
    					5, 10, 2, p3, cpsmidinn(12*ioct_melody+8),5,
    					5, 2, 2, p3, cpsmidinn(12*ioct_melody+14),5,
    					5, 8, 2, p3, cpsmidinn(12*ioct_melody+7),5,
    					5, 6, 2, p3, cpsmidinn(12*ioct_melody+15),5,
    				5, 14, 2, p3, cpsmidinn(12*ioct_melody+12),5,
    				5, 4, 2, p3, cpsmidinn(12*ioct_melody+7),5,
    				5, 10, 2, p3, cpsmidinn(12*ioct_melody+3),5,
    				5, 2, 2, p3, cpsmidinn(12*ioct_melody+5),5,
    					5, 14, 2, p3, cpsmidinn(12*ioct_melody+8),5,
    					5, 10, 2, p3, cpsmidinn(12*ioct_melody+8),5,
    					5, 4, 2, p3, cpsmidinn(12*ioct_melody+7),5,
    					5, 6, 2, p3, cpsmidinn(12*ioct_melody+3),5,
  \
    				 1, 4, 7, p3, cpsmidinn(arpmidi(5,2,array(0,3,2,7))), 2,
                     1, 4, 11, p3, cpsmidinn(arpmidi(5,2,array(3,0,2,7))), 2,
                     1, 4, 13, p3, cpsmidinn(arpmidi(5,2,array(3,0,7,2))), 2,
                     1, 4, 14, p3, cpsmidinn(arpmidi(5,2,array(3,7,0,2))), 2,
                     1, 4, 13, p3, cpsmidinn(arpmidi(5,2,array(2,0,7,3))), 2,
                     1, 4, 11, p3, cpsmidinn(arpmidi(5,2,array(0,2,7,3))), 2
                    
  score(Sinstrs, ivoxsteps, ivoxes, itrigs)
endin

