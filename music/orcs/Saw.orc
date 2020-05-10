instr Saw
  iamp = 0.5
  iwet = 0.7
  
  apuls = abs(fm_pulse( p4, iwet, 0.01))
  adel delay apuls, 0.25/p4
  apuls += adel
  adel delay adel, 0.25/p4
  apuls += adel
  
  aout =  apuls - 0.5
  aenv = linseg(0,0.01,iamp*2,p3-0.02,iamp*2,0.01,0)
  aout = aenv*aout
  aout moogvcf aout, expseg(16000,0.15,400),0.3
  
  sbus_mix(p5,aout,aout)
endin