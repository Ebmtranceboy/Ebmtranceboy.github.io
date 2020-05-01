opcode arpmidi,i,iik[]
  ioct_start, ioct_spread, karps[] xin
  
  inarps = lenarray(karps)
  iflip = ioct_spread * inarps - 1
  itick = int(now_tick()) % (2*iflip)
  if (itick > iflip) then
    itick = 2*iflip - itick
  endif
  ioct = int(itick/inarps)
  im = itick % inarps
  xout 12*(ioct+ioct_start) + i(karps,im)
endop