gi_OpenHat_square ftgen 0,0,32,7,-1,16,-1,0,1,16,1

instr OpenHat
  kblnd linseg 0.5,0.2,0.2
  istrh = 0.0025

  asig init 1

  adel0 delay1 asig
  adel delay1 adel0

  ap rand 0.5

  ;f1 0 32 7 -1 16 -1 0 1 16 1
  asgnp table ap + 1 - kblnd,gi_OpenHat_square,1

  asig = asgnp*(istrh*adel+(1-istrh)*asig)

  sbus_mix(p5, asig, asig)

endin

