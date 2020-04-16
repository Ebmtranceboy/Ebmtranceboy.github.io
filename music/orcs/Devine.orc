gi_Devine_p2 = 4096
gi_Devine_table ftgen 0,0,gi_Devine_p2,2,0

instr Devine
  ip2 = gi_Devine_p2
  ip32 = ip2/32
  itab[] init ip2
  idx  = 0
  until idx == ip32 do
      itab[idx] = -idx/ip32
      idx += 1
  od
  until idx == 2*ip32 do
      itab[idx] = -1+(idx-ip32)/ip32
      idx += 1
  od
  until idx == 3*ip32 do
      itab[idx] = (idx-2*ip32)/ip32
      idx += 1
  od
  until idx == 4*ip32 do
      itab[idx] = 1-(idx-3*ip32)/ip32
      idx += 1
  od
  until idx == 5*ip32 do
      itab[idx] = -(idx-4*ip32)/ip32
      idx += 1
  od
  until idx == 6*ip32 do
      itab[idx] = -1+(idx-5*ip32)/ip32
      idx += 1
  od
  until idx == 26*ip32 do
      itab[idx]=sin(2*$M_PI*(idx-6*ip32)/20/ip32)
      idx += 1
  od
  until idx == 27*ip32 do
      itab[idx] = (idx-26*ip32)/ip32
      idx += 1
  od
  until idx == 28*ip32 do
      itab[idx] = 1-(idx-27*ip32)/ip32
      idx += 1
  od
  until idx == 29*ip32 do
      itab[idx] = -(idx-28*ip32)/ip32
      idx += 1
  od
  until idx == 30*ip32 do
      itab[idx] = -1+(idx-29*ip32)/ip32
      idx += 1
  od
  until idx == 31*ip32 do
      itab[idx] = (idx-30*ip32)/ip32
      idx += 1
  od
  until idx == ip2 do
      itab[idx] = 1-(idx-31*ip32)/ip32
      idx += 1
  od
  copya2ftab itab,gi_Devine_table

  icps = p4
  krawr lfo 0.09,1.5,0.5
  klfo lfo krawr,50

  kcps = icps*(1+klfo)
  aph = 0
  kph init 0
  ksmp = 0
  while ksmp < ksmps do
      kph += kcps/sr + 1
      kph = kph % 1
      vaset kph, ksmp, aph
      ksmp += 1
      od

  asig table aph,gi_Devine_table,1,0,1
  aenv linsegr 1,0.2,0
  aout=aenv*asig/7

  sbus_mix(p5, aout, aout)
endin

