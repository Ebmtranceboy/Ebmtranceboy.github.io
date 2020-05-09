instr Snobby
  icps = p4
  ieta = -29.1
  iTe = 1/sr
  amod1 linseg 1,1,0.5
  amod2 linseg 1,1,0

  ia = 1
  ib = -2*ia*ieta

  aout = 0
  kmpuls init 1/3
  ky11 init 0
  ky1 init 0
  ky12 init 0
  ky2 init 1
  ky13 init 0
  ky init 0
  ksmp = 0
  while ksmp < ksmps do
      icps1 = icps / 2
      iw01 = 2*$M_PI*icps1
      ic1 = ia * (iw01^2+ieta^2)
      ky21 = ky11
      ky11 = ky1
      ky1 = (2*iTe^2*(icps1*kmpuls)+(4*ia-2*ic1*iTe^2)*ky11+(ib*iTe-2*ia)*ky21)/(2*ia+ib*iTe)

      kmpuls = 0

      kmod1 vaget ksmp, amod1
      icps2 = icps * 2
      iw02 = 2*$M_PI*icps2
      ic2 = ia * (iw02^2+ieta^2)
      ky22 = ky12
      ky12 = ky2
      ky2 = (2*iTe^2*(icps2*kmod1*ky1)+(4*ia-2*ic2*iTe^2)*ky12+(ib*iTe-2*ia)*ky22)/(2*ia+ib*iTe)

      kmod2 vaget ksmp, amod2
      icps3 = icps
      iw03 = 2*$M_PI*icps3
      ic3 = ia * (iw03^2+ieta^2)
      ky23 = ky13
      ky13 = ky
      ky = (2*iTe^2*(icps3*kmod2*ky2)+(4*ia-2*ic3*iTe^2)*ky13+(ib*iTe-2*ia)*ky23)/(2*ia+ib*iTe)
      vaset ky*icps/2, ksmp, aout
      ksmp += 1
      od

sbus_mix(p5, aout, aout)

endin

