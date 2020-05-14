/** Returns value of now beat time
   (Code used from Thorin Kerr's LivecodeLib.csd) */
opcode now, i, 0
  xout i(gk_now)
endop

opcode now_tick, i, 0
  xout i(gk_clock_tick)
endop
