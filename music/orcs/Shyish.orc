instr Shyish
  icps = p4
  klfo1 lfo 0.002,0.2
  klfo2 lfo 0.002,0.03
  klfo4 lfo 0.002,0.05
  ;.97:1
  ;.96:P5
  ;.95:M3
  ;.94:1
  ;.93:7
  ;.92:P5
  ;.91:P4
  ;.90:M3
  ;.89:m3
  ;.875:1
  ;.85:m7
  ;.83:P5
  ;.81:M3
  ;a1 casio_rez1 icps*1.01,0.791+klfo2
  a2 casio_rez1 icps*2.001,0.8752+klfo1
  a4 casio_rez1 icps*4.01,0.884+klfo2
  a1 casio_rez1 icps*1.01,0.875+klfo2
  aout = a2/7+a4/29+a1
  igain=0.06
  kgain linsegr 0,0.02,igain,0.5,0
  sbus_mix(p5, kgain*aout,kgain*aout)
endin