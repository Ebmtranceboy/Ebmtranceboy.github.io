instr Pad1
  icps = p4
  asaw saw icps, 0.6
  achorus chorus asaw, 0.5,0.01*log(icps)/7.6
  aright delay achorus,0.1
  kgain linsegr 0,0.05,0,0.5,0.5,0.5,0
  sbus_mix(p5, kgain*achorus, kgain*aright)
endin