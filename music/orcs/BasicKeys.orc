
gi_BasicKeys_exciter ftgen 0,0,8192,7,1,2047,1,1,-1,6143,-1

instr BasicKeys
  icps =        p4
  imaj =        0.4
  kamp linsegr  imaj, p3, imaj, 0.5, 0
  aenv expseg   1,0.8,0.001

  aval poscil   1,icps,gi_BasicKeys_exciter

  asig  =       kamp*aval*aenv

  sbus_mix(p5, asig, asig)
endin
