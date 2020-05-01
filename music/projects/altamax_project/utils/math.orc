
opcode bits,i,k[]
  kbits[] xin 
  indx =  lenarray(kbits) - 1
  inb = 0
  loop:
    if indx <= -1 goto done
  	inb = inb * 2 + i(kbits,indx)
  	indx -= 1
  	goto loop
  done:
  	xout inb
endop

opcode nthbit,i,ii
  ip,inth xin 
  
  i0 = ip % 2
  if (inth == 0) goto done
  
  loop:
  	ip = (ip - i0) / 2
  	i0 = ip % 2
  	inth -= 1
  if (inth != 0) goto loop

  done:
  	xout i0 
endop 
