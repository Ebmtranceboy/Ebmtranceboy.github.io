sr=44100
ksmps=32
0dbfs=1
nchnls=2

opcode dckill,a,a
  ain xin
  setksmps 1
  iN = 1001
  anum init 0
  asumy init 0

  iesp = (iN+1)/2
  ivar = (iN^2-1)/12

  adel delay ain, iN/sr

  anum = anum + iesp*adel + (iN-iesp)*ain - asumy
  asumy = asumy - adel + ain

  am = anum / iN / ivar
  ap = asumy/iN - am*iesp

  xout ain-(am*iN+ap)
endop


opcode saw,a,kk
  kcps, kbright xin
  setksmps 2
  kN = int (kbright*sr/(2*kcps))
  abuzz buzz	 1,kcps,kN,-1
  asawdc integ abuzz
  ;asaw dcblock asawdc, 0.999
  asaw dckill asawdc

  ;adeclick linsegr 0,0.04,0,0.01,1,0.01,0
  xout asaw
endop

opcode chorus,a,aki
  ain, kwet, idpth xin
  setksmps 1
  ;usage example:
  ;kwet linseg 0,0.5,0.7
  ;achorus chorus asaw, kwet, 0.005


  ilevl		=		.3		; Output level
  imax		=		0.156	; Maximum LFO rate
  imin		=		0.006	; Minimum LFO rate
  idelay		=		idpth * 2 + .01;imax;(idpth *2) + .01

  idiff		=		imax-imin

  ain             =               ain * ilevl
  i01             =               rnd(idiff)
  i02             =               rnd(idiff)
  i03             =               rnd(idiff)
  i04             =               rnd(idiff)
  i05             =               rnd(idiff)
  i06             =               rnd(idiff)
  alfo01          oscil           idpth, i01 + imin, -1
  alfo02          oscil           idpth, i02 + imin, -1, .08
  alfo03          oscil           idpth, i03 + imin, -1, .17
  alfo04          oscil           idpth, i04 + imin, -1, .25
  alfo05          oscil           idpth, i05 + imin, -1, .33
  alfo06          oscil           idpth, i06 + imin, -1, .42
  atemp           delayr          idelay * 2
  a01             deltapi         idelay + alfo01
  a02             deltapi         idelay + alfo02
  a03             deltapi         idelay + alfo03
  a04             deltapi         idelay + alfo04
  a05             deltapi         idelay + alfo05
  a06             deltapi         idelay + alfo06
                  delayw          ain
  achorus        sum		a01, a02, a03, a04, a05, a06

  aout		=		ain * (1-kwet) + achorus * kwet
  xout aout
endop

opcode fm_saw,a,kk 
  kcps,kbright xin
  setksmps 1

  kw = kcps/sr ; normalized freq
  kscale = kbright*6.5*(0.5-kw)^4
  kdc = 0.376-0.752*kw
  knorm = 1 - 2*kw

  asaw init 0
  asaw1 init 0

  kph phasor kcps

  asin tablei kph+asaw*kscale,-1,1,0,1
  asaw = (asaw+asin)/2
  aout = 2.5*asaw-1.5*asaw1
  asaw1 = asaw

  adeclick linsegr 1,0.01,0
  xout adeclick*knorm*(aout+kdc)
endop

opcode fm_pulse,a,KKK 
  kcps, kbright, krat xin
  setksmps	 2

  asaw fm_saw kcps,kbright

  adummy delayr 0.5
  adel deltap krat/kcps
  delayw asaw

  apulse = asaw - adel
  adeclick linsegr 0,0.01,1, 0.01,0
  xout adeclick*apulse
endop


instr Kick
  arnd_L random -1, 1
  arnd_R random -1, 1
  arndenv expseg 1, 0.02, 0.06, 0.02, 0.01, 0.12, 0.0001, 0.12, 0.00001

  alf expseg 600, 0.02,112, 0.14,50, 0.11,20, 0,20
  alfenv expseg 0.00001, 0.005,1, 0.16,1, 0.5,0.00001, 0, 0.00001
  asine oscili (1.0-arndenv)*alfenv, alf, -1

  aleft = arnd_L*arndenv+asine
  aright = arnd_R*arndenv+asine

  sbus_mix(p5, aleft, aright)
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

  sbus_mix(p5, aleft, aright)
  
endin

instr PsyBass
  icps = p4
  alvl expseg 1, 0.05, 0.5, 0.45, 0.00003
  iamp = 0.1
  a1 vco iamp * alvl, icps, 1, 0.5, -1
  a2 vco iamp * alvl, icps * 1.01, 1, 0.5, -1
  a3 vco iamp * alvl, icps * 0.99, 1, 0.5, -1
  asig sum a1, a2, a3
  acut linseg 1, 0.03, 0.1
  afilt moogvcf2 asig, 5000 * acut, 0.4
  aleft, aright freeverb afilt, afilt, 0.8, 0.35

  sbus_mix(p5, afilt + 0.04 * aleft, afilt + 0.04 * aright)

endin


instr Elephant
  icps = p4

  asaw vco2 0.3,icps
  asaw2 vco2 0.3,icps*1.0128496383
  asaw3 vco2 0.3,icps*0.9827973694
  aosc poscil 1,21,-1
  aout = (asaw+asaw2+asaw3)*(1+abs(aosc))*0.3
  sbus_mix(p5, aout, aout)
endin

instr Pad1
  icps = p4
  asaw saw icps, 0.6
  achorus chorus asaw, 0.5,0.01*log(icps)/7.6
  aright delay achorus,0.1
  kgain linsegr 0,0.05,0,0.5,0.5,0.5,0
  sbus_mix(p5, kgain*achorus, kgain*aright)
endin

instr Mixer
  al, ar  sbus_read 1
  alr, arr reverbsc al,ar,0.7,9000
  out(al+0.7*alr,ar+0.7*arr)
  sbus_clear(1)

  al, ar  sbus_read 2
  out(al/4,ar/4)
  sbus_clear(2)

  adel init 0
  al, ar  sbus_read 3
  adel delay al + 0.4*adel, i(gk_tempo) / 120 / 2
  al += adel
  ar += adel
  out(al*2,ar*2)
  sbus_clear(3)

  al, ar  sbus_read 4
   alr, arr reverbsc al,ar,0.7,9000
  out((al+0.7*alr)/6,(ar+0.7*arr)/6)
  sbus_clear(4)

  al, ar  sbus_read 5
  al, ar reverbsc al,ar, 0.9, 8000
  out(al/6,ar/6)
  sbus_clear(5)

endin

gk_tempo init 135

gitri ftgen  0, 0, 1025, 20,  3

chn_k "truc", 3, 2, 0, -1, 1
instr Mod
  klfo lfo 1, 0.02
  chnset klfo, "truc"
endin

schedule("Mod", ksmps / sr,-1) ; start("Mod")

instr Pulse
  kcps = p4
  apulse fm_pulse kcps,1,0.618
  ktruc chnget "truc"
  adel delay apulse, (i(ktruc)/2+1)/p4
  apulse += adel
  aenv expseg 1,0.9,0.001
  asig  =            0.07*apulse*aenv
  asig  moogvcf asig, expseg(16000,0.2,400),(0.4+i(ktruc)/4)
  sbus_mix(p5, asig,asig)
endin

schedule("Mixer", ksmps / sr,-1) ; start("Mixer")
;reset_clock()


gSinstrs[] fillarray "Pulse", "Kick", "Snare", "PsyBass", "Elephant", "Pad1"

gisteps[] fillarray 1,2,2,2,2,1,1,1,1,1,1,4,4,4,1
gitrigs[] fillarray bits(array(1,0,1,1,1,0,0,0,1,1,0,1,0,0,0)),
					bits(array(1,0,1,1,1,0,0,1,1,0,0,0,0,0,0)),
					bits(array(1,0,1,1,1,0,1,1,1,0,0,0,0,0,0)),
					bits(array(1,1,1,0,0,1,1,1,1,0,0,1,0,0,0)),
                    bits(array(1,1,0,1,0,1,1,1,1,1,0,0,1,0,0)),
                    bits(array(1,1,1,0,1,1,1,1,1,1,0,1,0,1,0)),
					bits(array(1,1,0,1,1,1,1,1,1,1,0,0,1,1,0)),
					bits(array(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)),
					bits(array(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
                   
					
gk_clock_tick init 0

instr P1
  
;   if gk_clock_tick >= 208 then
;     gk_clock_tick = 192
;   endif

  gidur = p3*0.7
  ioct1 = 4
  ioct2 = 5

  givoxes[] fillarray 0,128,0,0,0,0,
  \					  
   					  0, 16, bits(array(1,1,1,1,0,0,1,1,0,0,1,1,1,1,1,1)),gidur, cpsmidinn((ioct1-1)*12 + 5), 1,
                      0, 16, bits(array(1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0)), gidur, cpsmidinn((ioct1-1)*12 + 5), 1,
  \
                      0, 16, bits(array(0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0)), gidur, cpsmidinn(ioct1*12 + 5), 1,
                      0, 16, bits(array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1)), gidur, cpsmidinn(ioct1*12 + 5), 1,
  \
                      0, 16, bits(array(0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0)), gidur, cpsmidinn(ioct1*12 + 6), 1,
                      0, 16, bits(array(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)), gidur, cpsmidinn(ioct1*12 + 6), 1,
  \
                      0, 16, bits(array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), gidur, cpsmidinn(ioct1*12 + 10), 1,
                      0, 16, bits(array(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)), gidur, cpsmidinn(ioct1*12 + 10), 1,
  \                   
  					  2, 16, bits(array(0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0)), p3*4, 0, 2,
  \                   
  					  1, 16, bits(array(1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0)), p3*2, 0, 2,
  \                   
  					  3, 16, bits(array(0,1,1,0,1,0,0,0,0,1,1,0,0,1,0,0)), p3, cpsmidinn(2*12 + 5), 3,
  \                   
  					  3, 16, bits(array(0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0)), p3*4, cpsmidinn(4*12 + 5), 3,
  \                   
  					  4, 16, bits(array(0,0,1,0,0,1,0,0,1,1,0,0,0,0,1,0)), p3/1.2, cpsmidinn(5*12 + 5), 4,
  \                   
  					  4, 16, bits(array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)), p3/1.5, cpsmidinn(5*12 + 6), 4,
  \                   
  					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn(ioct2*12     + 5), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2+1)*12 + 0), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn(ioct2*12     + 10), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2+1)*12 + 6), 5,
  \                   
  					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2+1)*12 + 0), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2+1)*12 + 3), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2+1)*12 + 5), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2)*12 + 8), 5,
 \                   
  					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2-1)*12 + 9), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn(ioct2*12     + 6), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2+1)*12 + 0), 5,
 					  5, 32, bits(array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), p3*32, cpsmidinn((ioct2+1)*12 + 3), 5,
  \                   
  					  4, 16, bits(array(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)), p3*3, cpsmidinn(6*12 + 0), 4

  score(gSinstrs,gisteps,givoxes,gitrigs)
  
endin


