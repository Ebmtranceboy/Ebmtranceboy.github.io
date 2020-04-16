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
