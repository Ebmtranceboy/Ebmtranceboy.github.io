instr       VoxFM
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ; Simple FM instrument.
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  khz         =           p4
  kamplitude  =           10
  ktrackpady  linseg 0, 1, 0.002
  kcarrier    =           1 + 10 * ktrackpady
  ktrackpadx  linseg 0, 1, 0.003
  kmodulator  =           1 + 10 * ktrackpadx
  iattack     =           0.03
  irelease    =           0.1
  isustain    =           1
              ; Intensity sidebands.
  intensity   = 0.1
  kindex      line        intensity * 20, 2, 0.25
  asignal     foscili     kamplitude, khz, kcarrier, kmodulator, kindex, -1
  adamping    linsegr     0, iattack, 1, isustain, 1, irelease, 0
  asignal     =           asignal * adamping
  sbus_mix(p5, asignal, asignal)
endin
