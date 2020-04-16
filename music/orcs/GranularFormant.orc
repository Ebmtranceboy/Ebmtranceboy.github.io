;instr Mod
;	gklfo lfo 8.2,0.1
;	xchnset("GranularFormant.ktrl",12.9)
;endin

;start("Mod")

gi_GranularFormant_fn ftgen 0,0,256,2,0
gi_GranularFormant_wfn ftgen 0,0,256,20,6,1,1

instr GranularFormant
    ktrl = xchan:k("GranularFormant.ktrl",1.0)
    isize = 256

    kfreq lfo 0.25,0.02
    kfreq += 0.5
    kdx = 0
    while kdx<isize do
        kval = sin(2*$M_PI*kfreq*kdx/isize)
        tablew kval/10,kdx,gi_GranularFormant_fn
        kdx+=1
        od
    
    icps = p4
    kfmd = 5*ktrl
    kgdur = 0.2
    iovrlp = 50
    kfn = gi_GranularFormant_fn
    aSig grain2 icps,kfmd,kgdur,iovrlp,kfn,gi_GranularFormant_wfn 
    aEnv linsegr 0,0.01,1,0.5,0
    sbus_mix(p5, aSig/4*aEnv, aSig/4*aEnv)
endin

