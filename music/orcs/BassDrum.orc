instr BassDrum
  irez = 2
  ishape = 0
  imode = -1


  awet linseg 0.99,0.3,0.5
  amaster = cpspch(p4)/2

  anull init 0
  aphm, ametrom syncphasor amaster, anull
  aphs, adummy syncphasor amaster/(1-awet), ametrom
  axh = aphs+0.5

  aw mirror 2*axh-0.5,-0.5,0.5
  ahsine= aw*(4*aw^2-3)

  atri mirror 2*aphm,0,1
  arez = 1-(irez==1?1-aphm:(irez==2?atri:1-aphm+atri/2))*(ahsine+1) ; resonnance waves I,II, or III

  aout = arez
  al = aout
  ar = aout

  again linsegr 0.3,0.5,0,0.1,0
  out again*al,again*ar
endin
