;gi_Distar_tcheb ftgen 0,0,513,13,1,1,-300,100,-50,-33,25,20,-16.7,-14.2,12.5,11.1,-10,-9.09,8.333,7.69,-7.14,-6.67,6.25,5.88,-5.55,-5.26,5 ; quasi sawtooth transfer function
gi_Distar_tcheb ftgen 0,0,8192,13,0.5,1,0.4,1,.7,.8,.3,.1,.8,.9,1,1
;gi_Distar_tcheb ftgen 0,0,8192,13,0.5,1,-0.73,1,-1/2,1/3,-1/4,1/5,-1/6,1/7,-1/8,1/9,-1/10,1/11,-1/12,1/13,-1/14,1/15,-1/16,1/17,-1/18,1/19,-1/20,1/21,-1/22,1/23,-1/24,1/25,-1/26,1/27

;gi_Distar_phaser_rev_half_sine ftgen 0,0,16384,9,.5,-1,0 

instr Distar
  icps = p4
  iamp=0.7
  arnd random -1,0
  atk linseg 0,0.001,iamp,1/icps,iamp,0.0001,0
  ain=arnd*atk/10
  ka1 = 0.3532166
  ktype = 0

  knoclick linsegr 1,0.3,0
  ka0 = (1 - ka1)/2
  ;kblend 	rand	 0.5, 0.5, 0, 0.5

  isampsfrac = sr/icps
  isamps = int(isampsfrac)
  ifrac = 0;isampsfrac-isamps

  aendb init 0

  adump delayr (isamps+1)/sr
  abegp deltap 0
  aendp0 deltapn isamps
  aendp1 deltapn (isamps+1)
  aendp = (1-ifrac) * aendp0 + ifrac*aendp1
  a1 delay1 aendp
  a2 delay1 a1
      delayw (aendb+ain)

  adumb delayr (isamps+1)/sr
  abegb deltap 0
  aendb1 deltapn isamps
  aendb0 deltapn (isamps-1)
  aendb = (1-ifrac) * aendb0 + ifrac*aendb1
  if (ktype == 0) then
     amix = ka0 * (aendp + a2) + ka1 * a1
  endif
  ;if (ktype == 1) then
  ;  amix = ka0 * (aendp - a2) 
  ;endif
  ;if (ktype == 2) then
  ;  if (kblend &lt; ka1) then
  ;   amix = ka0 * (aendp + a2) + ka1 * a1
  ;  else
  ;   amix = -(ka0 * (aendp + a2) + ka1 * a1)
  ;  endif
  ;endif
       delayw amix;ka0*(aendp+(ktype==0?1:-1)*a2)+(ktype==0?ka1:0)*a1;(a1+aendp)/2


  asigl = knoclick*(abegp + aendb)
  asigr = knoclick*(abegb + aendp)

  adistl table asigl,gi_Distar_tcheb,1,0.5
  adistr table asigr,gi_Distar_tcheb,1,0.5

  sbus_mix(p5, adistl, adistr)
endin
