sr=44100
ksmps=32
0dbfs=1
nchnls=2

massign 0, 1; assign all incoming midi to instr 1 

chn_k "fadeout", 3, 2, 1, 0, 1

instr Mod
  kamp linseg 1,80,1,50,0
  chnset kamp, "fadeout"
endin 

schedule("Mod",  ksmps / sr, -1)


instr Master
  al, ar sbus_read 0
  kamp chnget "fadeout"
  outs al*kamp, ar*kamp
  sbus_clear 0
endin

schedule("Master",  ksmps / sr, -1)

instr Mixer
al, ar sbus_read 1
     adl init 0
    adl delay al + 0.2 * adl, 60/i(gk_tempo)/2
  
    al, ar reverbsc adl, ar, 0.85, 5000
    sbus_mix 0, al*1.5, ar*1.5
  sbus_clear 1

  al, ar sbus_read 2
    al, ar reverbsc al, ar, 0.65, 5000
    sbus_mix 0,  al*4, ar*4
  sbus_clear 2

  al, ar sbus_read 3
    sbus_mix 0,  al/8, ar/8
  sbus_clear 3

  al, ar sbus_read 4
  al, ar reverbsc al, ar, 0.3, 10000
      sbus_mix 0,  al*1.5, ar*1.5
  sbus_clear 4

  al, ar sbus_read 5
    al, ar reverbsc al, ar, 0.9, 9000
    sbus_mix 0,  al*0.5, ar*0.5
  sbus_clear 5

  al, ar sbus_read 6
    al, ar reverbsc al, ar, 0.9, 9000
    sbus_mix 0,  al, ar
  sbus_clear 6
endin 

schedule("Mixer",  ksmps / sr, -1)

instr 1
  
  xtratim 0.5
  
  krel init 0
  krel release 

  kmp init 0

  if krel == 1 kgoto fadeout
  kgoto attack
   
  kmp linseg 1, 0.5, 1, 0.3, 0

  fadeout:
    kmp linseg 1, 0.3, 0
    kgoto done
    
  attack:
    kmp linseg 1, 0.5 ,1
  
  done:
    
  inote notnum
  aout vco2 0.05, cpsmidinn(inote)
  aout *= kmp
  al = aout
  ar = aout
  al, ar reverbsc al, ar, 0.9, 10000
  
  outs al*2, ar*2
endin 
 
gi_snare_raw ftgen 0,0,1024,7,0,256,-1,512,1,256,0
gi_snare_fn ftgen 0,0,1024,30,gi_snare_raw,1,10

instr Snare
  icps = 85
  arnd_L random -1, 1
  arnd_L tone arnd_L, 6000
  arnd_R random -1, 1
  arnd_R tone arnd_R, 6000
  arndenv expseg 1,    0.007,1,   0.01,0.2, 0.05,0.2, 0.01,0.5, 0.5,0.5
  arndlfo lfo 0.15, icps*1.5
  aenvlfo expseg 0.00001, 0.005, 0.00001, 0.01, 1, 0.05, 1, 0.01, 00001, 0, 0.00001
  amp = arndenv + aenvlfo * arndlfo
  
  aenv expseg 1, 0.05,1, 0.9, 0.0001, 0,0.0001
  
  alfenv expseg 0.01,  0.001,0.4, 0.05, 0.4, 0.12, 0.001, 0, 0.001
  asine oscili alfenv*(1-amp),icps,gi_snare_fn
  aleft = (arnd_L*amp+asine)*aenv
  aright = (arnd_R*amp+asine*alfenv)*aenv
  
    aleft delay aleft, 0.195
  aright delay aright, 0.195


  sbus_mix(p5, aleft, aright)
  
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
  
  aleft delay aleft, 0.05
  aright delay aright, 0.05

  sbus_mix(p5, aleft, aright)
endin

instr 2
  kmp linseg 1, p3*0.9, 1, p3*0.1, 0
  inote = p4
  aout vco2 0.05, cpsmidinn(inote)
  aout *= kmp
  al = aout
  ar = aout
  
  sbus_mix p5, al*2, ar*2
endin

gk_clock_tick init -1
  
gifonds[] fillarray 14,14,15,12,14,14,14,14,
                   14,14,15,12,14,14,14,14,
                   10,10,7,7,14,14,14,14,
                   13,13,8,8,12,12,12,12
gimel[] fillarray  17, 24, 17, 21, 17, 22, 21, 17,
                   17, 24, 17, 21, 17, 24, 21, 17,
  				   17, 24, 17, 21, 17, 22, 21, 17,
                   17, 24, 17, 21, 17, 24, 21, 17,
  				   17, 24, 17, 22, 17, 24, 17, 22,
    			   17, 24, 17, 21, 17, 24, 17, 21,
  				   17, 24, 17, 22, 15, 24, 15, 22,
    			   15, 24, 15, 22, 17, 24, 19, 21

gibass[] fillarray 2,2, 5,3,2,2,2,2,
                    2,2,7,3,2,2,2,2,
                    3,3,5,7,2,2,2,2,
                    10,10,8,7,0,0,0,0
  
gitreble1[] fillarray 7, 5, 9, 2, 5, 7, 9, 5, 7,  5,9,2 ,10, 5,3, 0
gitreble2[] fillarray 9, 10,5 ,9 ,9, 10,12,9, 9, 10,5,9  ,5,12,5, 7
gitreble3[] fillarray 14,12,14,14,14,12,14,14,14,12,14,12,13,8,19,15
  
ginfra[] fillarray 2,2,2,0
  
instr P1
  ibeat = p4 + 256
  
  if ibeat%4 == 0 && (ibeat < 512  || ibeat >= 768) then
  	schedule(2, 0, p3*1.5, 12*4 + gifonds[int(ibeat/8)%32], 1)
  endif 

  if ibeat%4 == 0 && ibeat >= 256 + 96 then
  	schedule("Kick", 0, p3*4, 0, 3)
  endif 
  
  if ibeat%64 == 3 && ibeat >= 256 + 96 then
  	schedule("Kick", 0, p3*2, 0, 3)
  endif 
  
  if ibeat%64 == 34 && ibeat >= 256 + 96 then
  	schedule("Kick", 0, p3*2, 0, 3)
  endif 
  
  if (ibeat+2)%16 == 14 && ibeat >= 256 + 128 then
  	schedule("Snare", 0, p3*4, 0, 4)
  endif 
  
  if (ibeat+2)%16 == 12 && ibeat >= 256 + 128 then
  	schedule("Snare", 0, p3*4, 0, 4)
  endif 
  
  if (ibeat+2)%16 == 4 && ibeat >= 256 + 128 then
  	schedule("Snare", 0, p3*4, 0, 4)
  endif 
  
  if (ibeat+2)%4 == 0 && ((ibeat >= 256 + 192 && ibeat < 512) || (ibeat >= 512 + 192)) then
  	schedule(2, 0, p3*1.5, 12*4 + gimel[int((ibeat+2)/4-1)%64], 1)
  endif

  if ibeat%2 == 0 && ibeat >= 256 + 64 then
  	schedule(2, 0, p3*0.9, 12*(3 + int(ibeat/2)%2) + gibass[int(ibeat/8)%32], 2)
  endif 
  
  if ibeat% 16 == 0 && ibeat >= 512 && ibeat < 512 + 192 then
    ;schedule(2, 0, p3*18, 12*7+gitreble1[int(ibeat/16)%16],5)
    schedule(2, 0, p3*18, 12*6+gitreble2[int(ibeat/16)%16],5)
    ;schedule(2, 0, p3*18, 12*6+gitreble3[int(ibeat/16)%16],5)
  endif
 
  if ibeat% 16 == 0 && ibeat >= 512 + 64 && ibeat < 512 + 192 then
    schedule(2, 0, p3*18, 12*6+gitreble1[int(ibeat/16)%16],5)
    ;schedule(2, 0, p3*18, 12*6+gitreble3[int(ibeat/16)%16],5)
  endif

    if ibeat% 64 == 32 && ibeat < 320 then
    schedule(2, 0, p3*18, 12*2+ginfra[int(ibeat/64)%4],6)
  endif

endin
