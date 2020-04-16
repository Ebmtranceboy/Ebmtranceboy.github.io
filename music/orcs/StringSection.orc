instr StringSection
  icps = p4
  awet linseg 0.99,0.5,0.985 ; 0 <= wet <= 1
  icoef=1.005    ; close to 1
  iepsilon=0.02  ; close to 0
  igain = 0.4    ; 1/n where n is the polyphony
  iattack = 0.1
  irelease = 0.07

  aph0 phasor icps/(icoef+iepsilon)/icoef
  aph1 phasor icps/icoef
  aph2 phasor icps
  aph3 phasor icps*(icoef-iepsilon)
  aph4 phasor icps*(icoef+iepsilon)*(icoef-iepsilon)

  alpha = 2-awet 
  abeta = alpha/(alpha-1)/2
  agamma=(1/alpha-0.5)

  amod0 = 1-aph0
  af0 mirror alpha*amod0,0,1
  av0 mirror 2*(aph0-(abeta*(awet*amod0-af0))*agamma)-0.5,-0.5,0.5
  a0 = av0*(4*av0^2-3)

  amod1 = 1-aph1
  af1 mirror alpha*amod1,0,1
  av1 mirror 2*(aph1-(abeta*(awet*amod1-af1))*agamma)-0.5,-0.5,0.5
  a1 = av1*(4*av1^2-3)

  amod2 = 1-aph2
  af2 mirror alpha*amod2,0,1
  av2 mirror 2*(aph2-(abeta*(awet*amod2-af2))*agamma)-0.5,-0.5,0.5
  a2 = av2*(4*av2^2-3)

  amod3 = 1-aph3
  af3 mirror alpha*amod3,0,1
  av3 mirror 2*(aph3-(abeta*(awet*amod3-af3))*agamma)-0.5,-0.5,0.5
  a3 = av3*(4*av3^2-3)

  amod4 = 1-aph4
  af4 mirror alpha*amod4,0,1
  av4 mirror 2*(aph4-(abeta*(awet*amod4-af4))*agamma)-0.5,-0.5,0.5
  a4 = av4*(4*av4^2-3)

  kgain linsegr 0,iattack,igain,irelease,0
  asig = igain*kgain*(a0+a1+a2+a3+a4)

  sbus_mix(p5, asig, asig)
endin
