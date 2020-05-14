opcode elem,i,i[]iiii
  iarr[], ioffset, icols, icol, irow xin 
  xout iarr[(ioffset + irow) * icols + icol]
endop  

opcode cue3,i,i[]iiiiii
  idurs[], ioffset, icols,icol,istart,im,is xin
  iy = elem(idurs,ioffset,icols,icol,istart)
  
  if (im<is+iy) then
    iout = istart
  else 
    iout = cue3(idurs,ioffset,icols,icol,istart+1,im,is+iy)
  endif
  xout iout
endop

opcode cue2,i,i[]iiiii
  idurs[], ioffset, icols,icol,isum,im xin 
  
  if (im >= isum) then
    iout = cue2(idurs, ioffset, icols,icol, isum, im-isum)
  else
    iout = cue3(idurs,ioffset,icols,icol, 0, im, 0)
  endif
  xout iout
endop 

opcode sum,i,i[]iiii
  iarr[],ioffset,irows,icols,icol xin 
  idx = -1
  isum = 0
  
  loop:
  	idx += 1
  	isum += elem(iarr, ioffset, icols, icol, idx)
  if idx < irows - 1 goto loop

  xout isum
endop

opcode cue1,i,i[]iiiii
  idurs[], ioffset, irows, icols,icol, idelay xin 
  isum = sum(idurs,ioffset,irows,icols,icol)
  xout cue2(idurs, ioffset, icols,icol, isum, (now_tick()-1-idelay+isum)%isum)
endop 

opcode cue0,i,i[]iiii
  idurs[],ioffset, irows, icols,icol xin 
  xout cue1(idurs,ioffset, irows, icols,icol, 0)
endop

opcode cue,ii,i[]iiiii
  ivoxes[],ioffset, irows, icols,irep,ibit xin 
  
  if irows == 1 then
    indx = 0
    irep0 = elem(ivoxes,ioffset,icols,irep,0)
    istem = 1 + (now_tick()-1  + irep0) % irep0
  else
 
    indx = cue0(ivoxes,ioffset, irows, icols,irep)
    istem = -1
    loop:
      istem += 1
      if cue1(ivoxes,ioffset, irows, icols,irep, istem) == indx goto loop
   endif 
    
   if istem > 0 then
     ibit = nthbit(elem(ivoxes,ioffset,icols,ibit,indx),istem-1)
   else
	ibit = 0
   endif
        
  xout indx, ibit
 endop
 