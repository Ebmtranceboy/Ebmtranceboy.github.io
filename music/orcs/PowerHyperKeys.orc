instr PowerHyperKeys
  icps0 = p4
  icps1 = 1.01 * icps0
  alfo0 lfo	 0.05, 1.5
  alfo1 lfo	 0.05, 1.51
  arat0 = 0.25 + alfo0
  arat1 = 0.75 + alfo1

  ibright = 1

  aph0 phasor icps0
  at0 = aph0 - 0.5

  icoef0 = ibright * sr / icps0
  iscale0 = tanh(icoef0 * 0.5)
  aheaviside0 = tanh(icoef0 * at0)

  asaw0 = aheaviside0 - 2 * iscale0 * at0
  aparab integ asaw0

  aph1 phasor icps1
  at1 = aph1 - 0.5

  icoef1 = ibright * sr / icps1
  iscale1 = tanh(icoef1 * 0.5)
  aheaviside1 = tanh(icoef1 * at1)

  asaw1 = aheaviside1 - 2 * iscale1 * at1

  ilen = int(0.5 * sr)
  kbuf0[] init ilen
  kbuf1[] init ilen
  kidx init 0
  kn init 0

  adel0 = 0
  adel1 = 0
  ksmp = 0
  while ksmp < ksmps do
      krat0 vaget ksmp, arat0
      kn = int(kidx + ilen - sr * krat0 / icps0) % ilen
      vaset kbuf0[kn], ksmp, adel0
      kparab vaget ksmp, aparab
      kbuf0[kidx] = kparab

      krat1 vaget ksmp, arat1
      kn = int(kidx + ilen - sr * krat1 / icps1) % ilen
      vaset kbuf1[kn], ksmp, adel1
      ksaw1 vaget ksmp, asaw1
      kbuf1[kidx] = ksaw1

      kidx += 1
      kidx = kidx % ilen
      ksmp += 1
      od

  aramp = (adel0 - aparab) / (sr * arat0 * (1 - arat0) / icps0)
  apulse = asaw1 - adel1

  igain = 0.1
  adeclick linsegr 1,0.01,0
  aout = igain * adeclick * (0.7 * aramp + 0.3 * apulse)

  sbus_mix(p5, aout, aout)

endin
