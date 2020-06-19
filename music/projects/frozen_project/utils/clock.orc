
gk_tempo init 140
gk_clock_internal init 0
gk_clock_tick init 0

instr Clock ;; our clock  
  ;; tick at 1/16th note
  kfreq = (4 * gk_tempo) / 60     ;; frequency of 16th note
  kdur = 1 / kfreq                ;; duration of 16th note in seconds 
  kstep16th = kfreq / kr          ;; step size in 16th notes per buffer
  gk_clock_internal += kstep16th  ;; advance 16th note clock

  // checks if next buffer will be one where clock will
  // trigger.  If so, then schedule event for time 0 
  // which will get processed next buffer. 
  if(gk_clock_internal + kstep16th >= 1.0 ) then
    gk_clock_internal -= 1.0 
    gk_clock_tick += 1 
    event("i", "Perform", 0, kdur, gk_clock_tick)
  endif
endin

instr Perform
  ibeat = p4
  schedule("P1", 0, p3, ibeat) 
endin

schedule("Clock", ksmps / sr, -1)

