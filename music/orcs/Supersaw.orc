
        instr Supersaw
  ;A supersaw is available on some synthesizers as an extra waveform,
  ;besides the familiar sine, triangle, square and saw. A supersaw is
  ;typically seven saws that are slightly detuned. I found out that you also
  ;get a nice sound if you replace a saw with a square, so I made this
  ;instrument that lets you choose between them. They are called pulse
  ;in the interface because you can select the pulse width (does not
  ;apply if saw is selected).

  ;I did not add presets because it usually sounds good, unless the
  ;lo pass knob is too low, and because it does not have that much
  ;diversity (but a nice instrument anyway). It sounds best around
  ;pitch 7.00 to 9.06, especially in chords.

  ;Usage:
  ;p4: pitch or frequency

  ;Example:
  ;i1 0 2 8.00 
  ;i1 0 . 8.02 
  ;i1 0 . 8.04

  ;ctrls
  ;-----
  ;kvol_adjust
  ;kdiff1 ;kdiff2 ;kdiff3
  ;ktype1 ;ktype2 ;ktype3 ;ktype4
  ;kpw1 ;kpw2 ;kpw3 ;kpw4
  ;kamp1 ;kamp2 ;kamp3 ;kamp4
  ;kcomp1 ;kcomp2_3 ;kcomp4_5 ;kcomp6_7
  ;kattack ;kdecay ;ksustain ;krelease
  ;klo_pass
  ;kPanpos

  ivol_adjust = -12.46191
  kdiff1 init 0.19435695
  kdiff2 init 0.2819123
  kdiff3 init 0.73613167
  itype1 = 2 ;0:Saw 2:pulse
  kpw1 init 0.05411869
  kamp1 init 0.04845392
  itype2 = 0 ;0:Saw 2:pulse
  kpw2 init 0.6060633
  kamp2 init 0.9652921
  itype3 = 0 ;0:Saw 2:pulse
  kpw3 init 0.1303014
  kamp3 init 0.3946069
  itype4 = 2 ;0:Saw 2:pulse
  kpw4 init 0.36728287
  kamp4 init 0.5888107
  kcomp1  init  1
  kcomp2_3  init  1
  kcomp4_5  init  1
  kcomp6_7  init  1
  kattack init 0.0025528558
  kdecay init 6.013136
  ksustain init 0.2885465
  krelease init 0.9659428
  klo_pass init 20.6647
  kPanpos init 0

  ifreq	= p4 

  iamp = ampdb(ivol_adjust)

  asig = 0

  ; my experiments show that it sounds a lot better when
  ; components with opposite frequencies have the same
  ; wave type and pulse width,
  ; and that the phases should all be the same
  if (kcomp1!=0) then
	  asig1 vco2 iamp*kamp1, ifreq, itype1, kpw1
	  asig = asig+asig1
  endif

  if (kcomp2_3!=0) then
	  asig2 vco2 iamp*kamp2, ifreq+kdiff1, itype2, kpw2
	  asig3 vco2 iamp*kamp2, ifreq-kdiff1, itype2, kpw2
	  asig = asig+asig2+asig3
  endif

  if (kcomp4_5!=0) then
	  asig4 vco2 iamp*kamp3, ifreq+kdiff2, itype3, kpw3
	  asig5 vco2 iamp*kamp3, ifreq-kdiff2, itype3, kpw3
	  asig = asig+asig4+asig5
  endif

  if (kcomp6_7!=0) then
	  asig6 vco2 iamp*kamp4, ifreq+kdiff3, itype4, kpw4
	  asig7 vco2 iamp*kamp4, ifreq-kdiff3, itype4, kpw4
	  asig = asig+asig6+asig7
  endif

  icutoff = ifreq*i(klo_pass)
  if (icutoff<sr/2) then
	  asig butterlp asig, icutoff
  endif

  iatk=i(kattack)
  idec=i(kdecay)
  isus=i(ksustain)
  irel=i(krelease)
  ifrac = iatk+idec+irel
  if ( ifrac > 1 ) then
	  iatk = iatk / ifrac
	  idec = idec / ifrac
	  irel = irel / ifrac
  endif

  kenv adsr iatk, idec, isus, irel
  asig = asig*kenv


  kPanpos = kPanpos/2

  kPanpos	= kPanpos * 3.14159265359 * .5
  kLeft     	= sqrt(2) / 2 * (cos(kPanpos) - sin(kPanpos)) 
  kRight    	= sqrt(2) / 2 * (cos(kPanpos) + sin(kPanpos))

  sbus_mix(p5, asig*kLeft, asig*kRight)

endin
