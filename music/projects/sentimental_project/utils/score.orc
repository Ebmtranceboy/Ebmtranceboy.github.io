opcode score,0,S[]i[]i[]i[]j
  Sinstrs[], ivoxsteps[], ivoxes[], itriggers[], iparameters xin 
    
  if iparameters < 0 then
    iparameters = 6
  endif 
  
  iacc = 0
  itref = -1
  
  it = -1
  loop_trigger:
  	it += 1
  	ischeme = itriggers[it]
  
  	imaxlen = 0

    iv = -1
    ioffset = 0
    loop_vox0:
      iv += 1
      isteps = abs(ivoxsteps[iv]) 
  	  iplay = nthbit(ischeme, iv)
  	  
  	  if iplay == 1 then
        
        isumreps = sum(ivoxes,ioffset,isteps,iparameters,1)
        if imaxlen < isumreps then
          imaxlen = isumreps
        endif 
        
      endif 
      ioffset += isteps
  	  if iv<lenarray(ivoxsteps)-1 goto loop_vox0

    if iacc <= now_tick() && now_tick() < iacc + imaxlen then
      itref = it
    endif
    iacc += imaxlen
    if itref == -1 && it < lenarray(itriggers) - 1 goto loop_trigger
      
  if itref < 0 goto done
  ischeme = itriggers[itref]

  iv = -1
  ioffref = -1
  ioffset = 0
  loop_vox1:
  	 iv += 1
     iplay = nthbit(ischeme, iv)
     isteps = ivoxsteps[iv]
     if iplay == 1 then
       indx, istem = cue(ivoxes,ioffset,isteps,iparameters,1,2)
       if istem == 1 then
         schedule(Sinstrs[elem(ivoxes, ioffset, iparameters, 0, indx)], 
           0, 
           elem(ivoxes, ioffset, iparameters, 3, indx), 
           elem(ivoxes, ioffset, iparameters, 4, indx), 
           elem(ivoxes, ioffset, iparameters, 5, indx))
         endif 
     endif
     ioffset += isteps
    if iv<lenarray(ivoxsteps)-1 goto loop_vox1
  	done:
endop
  