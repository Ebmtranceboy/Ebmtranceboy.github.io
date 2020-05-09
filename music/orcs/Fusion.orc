gi_fusion_saw ftgen 0,0,8192,7,1,2048,-1,0,1,2048,-1,0,1,2048,-1,0,1,2048,-1
gi_fusion_wn ftgen 0,0,8192,20,2,1

chn_k "Fusion.grains", 3, 1, 5, 1, 20

;instr Mod
;	klfo lfo 8.2, 0.1
; chnset int(abs(klfo*20)), "Fusion.grains"
;endin

;start("Mod")

instr Fusion
  imaj_grains chnget "Fusion.grains"
  
  ifn ftgen 0,0,8192,-30,gi_fusion_saw,1,(imaj_grains+10)*80

  icps = p4  			; perceived frequency 
  iovrlp = 2 + imaj_grains 			; 1 + max simultaneous grains
  kgdur expseg 0.03,p3,0.01 		; grain duration
  kgamp expseg 0.9,p3,0.02 		; grain amplitude

  iperiod = sr/icps
  ksnd[] init iovrlp
  ksize init 0
  kcpt init iperiod+1 ; start first grain ASAP

  aout = 0

  knsamp = 0
  while knsamp < ksmps do

    if kcpt > iperiod then ; new grain generation

      ; is there a free slot ?
      kfound = 0
      kndx = 0
      while kndx < ksize && kfound == 0 do
        if ksnd[kndx] >= 1 then
        kfound = 1
        else
        kndx += 1
        endif
      od
    
      if ksize < lenarray(ksnd)-1  then
        if kndx < ksize then                    ; slot found
      ksnd[kndx] = (kcpt-iperiod)/iperiod
        elseif kndx == ksize then               ; create new slot
      ksize += 1
      ;Print ksnd,ksize
      ksnd[kndx] = (kcpt-iperiod)/iperiod
        endif
      endif

      kcpt -= iperiod   	; reinit metronom
    endif

    kcpt += 1

    ksamp = 0		; harvest time
    kgrain = 0
    while kgrain < ksize do
      if ksnd[kgrain] < 1 then
        kval table ksnd[kgrain],ifn,1,0
        kwin table ksnd[kgrain],gi_fusion_wn,1,0
        ksamp += kval*kwin*kgamp
        ksnd[kgrain] = ksnd[kgrain] + 1/sr/kgdur
      endif
      kgrain += 1
    od
    vaset ksamp, knsamp, aout
    knsamp += 1
  od

  kfc expseg  ((imaj_grains-1)%8+1)*1000,p3, ((imaj_grains+1-1)%8+1)*1000
  krez expseg imaj_grains/20,p3,(imaj_grains+1)/20
  aout moogladder aout,kfc,krez
  sbus_mix(p5, aout, aout)
endin
