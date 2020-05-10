instr Violin
  icps = p4

  aenv expseg 0.01, 0.05, 1, 2, 0.01
  iwet = 0.5

  iraw ftgen 0, 0, 512, 7, 1, 512, -1
  isaw ftgen 0, 0, 512, 30, iraw, 1, iwet * sr / icps
  asig poscil 1, icps, isaw
  ain = aenv * asig

  ka1 = 0.5

  knoclick linsegr 1, 0.3, 0
  ka0 = (1 - ka1) / 2

  isampsfrac = sr/icps
  isamps = int(isampsfrac)
  ifrac = isampsfrac - isamps

  ilen = isamps + 2
  kbufferb[] init ilen
  kidx init 0
  kbufferp[] init ilen

  abegb = 0
  aendb = 0
  abegp = 0
  aendp = 0

  k1 init 0
  kendb init 0
  kendp init 0
  kn init 0

  ksmp = 0
  while ksmp < ksmps do
      kn = (kidx + 0) % ilen
      kbegp = kbufferp[kn]
      kn = (kidx + ilen - isamps) % ilen
      kendp0 = kbufferp[kn]
      kn = (kidx + ilen - isamps - 1) % ilen
      kendp1 = kbufferp[kn]
      k2 = k1
      k1 = kendp
      kendp = (1-ifrac) * kendp0 + ifrac * kendp1
      kin vaget ksmp, ain
      kn = kidx % ilen
      kbufferp[kn] = kendb + kin
      
      kn = (kidx + 0) % ilen
      kbegb = kbufferb[kn]
      kn = (kidx + ilen - isamps + 1) % ilen
      kendb1 = kbufferb[kn]
      kn = (kidx + ilen - isamps) % ilen
      kendb0 = kbufferb[kn]
      kendb = (1-ifrac) * kendb0 + ifrac * kendb1
      kmix = ka0 * (kendp + k2) + ka1 * k1
      kn = kidx % ilen
      kbufferb[kn] = kmix

      vaset kbegp, ksmp, abegp
      vaset kendp, ksmp, aendp
      vaset kbegb, ksmp, abegb
      vaset kendb, ksmp, aendb

      kidx += 1
      kidx = kidx % ilen
      ksmp += 1
      od

  aleft = knoclick * (abegp + aendb)
  aright = knoclick * (abegb + aendp)

  igain = 0.005

  sbus_mix(p5, igain * aleft, igain * aright)

endin

