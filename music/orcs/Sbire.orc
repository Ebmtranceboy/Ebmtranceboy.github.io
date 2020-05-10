
gi_Sbire_exciter ftgen 0,0,1024,-7,0.31,94,0.67,94,0,15,0.77,5,0.38,98,0.25,74,0.53,79,0.42,49,0.15,10,0,167,0.46,128,0.06,211,0

instr Sbire
  icps = p4

  kmaster = 0.1
  kamp  =    kmaster * 5
  kexciter_gain = 0.4

  knh = int (sr / 2 / icps)
  aex buzz 1.5, icps, knh, -1

  kexcindx linseg 0, 0.4 * log(icps) / 3, ftlen(gi_Sbire_exciter)
  kexcigain table kexcindx, gi_Sbire_exciter

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
  asigneg = 2*asig^2-1
  asigpos = 8*asig^4-8*asig^2+1
  aleft = kamp * (awgl + asigneg + asigpos)
  aright = kamp * (awgr + asigneg + asigpos)

  sbus_mix(p5, aleft, aright)

endin

