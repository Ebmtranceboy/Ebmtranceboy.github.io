sr=44100
ksmps=1
nchnls=2
0dbfs=1

;gihand fiopen "unisonMelody", 0
ga_bluemix_1_0	init	0
ga_bluemix_1_1	init	0
ga_bluemix_2_0	init	0
ga_bluemix_2_1	init	0
ga_bluemix_3_0	init	0
ga_bluemix_3_1	init	0
ga_bluemix_4_0	init	0
ga_bluemix_4_1	init	0
ga_bluemix_5_0	init	0
ga_bluemix_5_1	init	0
ga_bluemix_6_0	init	0
ga_bluemix_6_1	init	0
ga_bluesub_Master_0	init	0
ga_bluesub_Master_1	init	0


gk_blue_auto0 init 5085
gk_blue_auto1 init 0.6
gk_blue_auto2 init 43.488037
gk_blue_auto3 init 0.7924239
gk_blue_auto4 init 0
gk_blue_auto5 init 0.4071353
gk_blue_auto6 init 0.41953278
gk_blue_auto7 init 0.3427653
gk_blue_auto8 init 0
gk_blue_auto9 init 0.4071353
gk_blue_auto10 init 0.41953278
gk_blue_auto11 init 0.84447163
gk_blue_auto12 init -16
gk_blue_auto13 init -23.3
gk_blue_auto14 init -24.2
gk_blue_auto15 init 0.4071353
gk_blue_auto16 init 0.41953278
gk_blue_auto17 init 0.84447163
gk_blue_auto18 init -1.7
gk_blue_auto19 init 0.4071353
gk_blue_auto20 init 0.41953278
gk_blue_auto21 init 0.3427653
gk_blue_auto22 init 0.7
gk_blue_auto23 init 5.3


gi_tales_trisaw ftgen 0,0,1024,7,1,5,-0.6,246,0.3,5,-0.3,251,0.6,5,-1,512,1



	opcode wavesolve,aa,aikk

ain, icps,ka1,ktype xin
setksmps 1

knoclick linsegr 1,0.3,0
ka0 = (1 - ka1)/2
;kblend 	rand	 0.5, 0.5, 0, 0.5

isampsfrac = sr/icps
isamps = int(isampsfrac)
ifrac = isampsfrac-isamps

aendb init 0

adump delayr (isamps+1)/sr
abegp deltap 0
aendp0 deltapn isamps
aendp1 deltapn (isamps+1)
aendp = (1-ifrac) * aendp0 + ifrac*aendp1
a1 delay1 aendp
a2 delay1 a1
    delayw (aendb+ain)

adumb delayr (isamps+1)/sr
abegb deltap 0
aendb1 deltapn isamps
aendb0 deltapn (isamps-1)
aendb = (1-ifrac) * aendb0 + ifrac*aendb1
if (ktype == 0) then
   amix = ka0 * (aendp + a2) + ka1 * a1
endif
;if (ktype == 1) then
;  amix = ka0 * (aendp - a2) 
;endif
;if (ktype == 2) then
;  if (kblend < ka1) then
;   amix = ka0 * (aendp + a2) + ka1 * a1
;  else
;   amix = -(ka0 * (aendp + a2) + ka1 * a1)
;  endif
;endif
     delayw amix;ka0*(aendp+(ktype==0?1:-1)*a2)+(ktype==0?ka1:0)*a1;(a1+aendp)/2


aleft = knoclick*(abegp + aendb)
aright = knoclick*(abegb + aendp)
xout aleft,aright

	endop
	opcode blueEffect0,aa,aa ; Tempo-Sync Stereo Delay

ain1,ain2	xin
ibpm = 130.0
inl = 0.75

ifb = 0.5
idel = ( 1/(ibpm/60) * inl )

aleft delayr idel
aright delayr idel

if ( 1 == 0 ) then
	ado1 = aleft + ain1
	ado2 = aright + ain2
else
	ado1 = aright + ( .5 * (ain1 + ain2) )
	ado2 = aleft
endif

delayw ado1 * ifb
delayw ado2 * ifb

if ( 1 == 1 ) then
	aout1 = aright
	aout2 = aleft
else
	aout1 = aleft
	aout2 = aright
endif

aout1 = ( aout1 * ( 1 - 0.5 )  ) * 2
aout2 = ( aout2 * 0.5 ) * 2

aout1 = (ain1 * 0.5) + (aout1 * (1 - 0.5))
aout2 = (ain2 * 0.5) + (aout2 * (1 - 0.5))
xout	aout1,aout2


	endop
	opcode blueEffect1,aa,aa ; Freeverbw

ain1,ain2	xin
setksmps 1
denorm ain1,ain2
arev1,arev2 freeverb ain1,ain2,gk_blue_auto7,gk_blue_auto5,sr,1
aout1 = (arev1*gk_blue_auto6)+(ain1*(1-gk_blue_auto6))
aout2 = (arev2*gk_blue_auto6)+(ain2*(1-gk_blue_auto6))

xout	aout1,aout2


	endop
	opcode blueEffect2,aa,aa ; Freeverbw

ain1,ain2	xin
setksmps 1
denorm ain1,ain2
arev1,arev2 freeverb ain1,ain2,gk_blue_auto11,gk_blue_auto9,sr,1
aout1 = (arev1*gk_blue_auto10)+(ain1*(1-gk_blue_auto10))
aout2 = (arev2*gk_blue_auto10)+(ain2*(1-gk_blue_auto10))

xout	aout1,aout2


	endop
	opcode blueEffect3,aa,aa ; Freeverbw

ain1,ain2	xin
setksmps 1
denorm ain1,ain2
arev1,arev2 freeverb ain1,ain2,gk_blue_auto17,gk_blue_auto15,sr,1
aout1 = (arev1*gk_blue_auto16)+(ain1*(1-gk_blue_auto16))
aout2 = (arev2*gk_blue_auto16)+(ain2*(1-gk_blue_auto16))

xout	aout1,aout2


	endop


	instr 2	;vampyr
icps = cpspch(p4)
;foutir gihand,0,1,int(p4),frac(p4)*100


imaster = p5/127
kamp linseg imaster,6,imaster,0.1,0

kexenv expseg 0.9999,0.2,0.2
ain      vco2 0.1540000141, icps, 2, kexenv 

ka1 = 0.99687818289
ktype = 1

aleft,aright wavesolve ain, icps, ka1, ktype
 ; aleft = ain
;aright = ain
;aleft moogladder aleft, gk_blue_auto0, gk_blue_auto1
;aright moogladder aright, gk_blue_auto0*1.01, gk_blue_auto1
ga_bluemix_1_0 +=  kamp*aleft
ga_bluemix_1_1 += kamp*aright

	endin

	instr 3	;tales
icps = cpspch(p4)
;foutir gihand,0,1,int(p4),frac(p4)*100

iglide_on=0
idel=30

if(iglide_on==1) then 
    ipcps=(p3<0?icps:cpspch(p6))
    ip3=(p3<0?0.5:p3)
    iglide=0.07
    kmaster linseg ipcps,iglide,icps,ip3-iglide,icps
else 
    kmaster init icps
endif
aph phasor kmaster
aph0 phasor kmaster/2
araw table aph,gi_tales_trisaw,1,0,1
araw0 table aph0,gi_tales_trisaw,1,0,1
adeclick linsegr 1,0.05,0
kcutoff min gk_blue_auto2*icps,10000
;asig moogladder araw, kcutoff, gk_blue_auto3
;asig0 moogladder araw0, kcutoff, gk_blue_auto3
asig = araw
asig0 = araw0
adel delay asig,idel
aout = adeclick*(asig0+adel)
ga_bluemix_2_0 +=  aout
ga_bluemix_2_1 += aout

	endin

	instr 4	;Kick
seed 2015103
arnd_L random -1, 1
arnd_R random -1, 1
arndenv expseg 1, 0.02,0.06, 0.02,0.01, 0.12,0.00001, 0.12,0.00001

alf expseg 600, 0.02,112, 0.14,50, 0.11,20, 0,20
alfenv expseg 0.00001, 0.005,1, 0.16,1, 0.5,0.00001, 0,0.00001
asine oscili (1-arndenv)*alfenv, alf, -1

ga_bluemix_3_0 +=  arnd_L * arndenv + asine
ga_bluemix_3_1 +=  arnd_R * arndenv + asine

	endin

	instr 5	;SnareDrum

  kamp0 linseg 0.6, 0.002, 1, 0.004, 0.4, 0.001, 1, 0.04, 1, 0.01, 0.5, 0.25, 0
  knoisamp linseg 0.5, 0.005, 0.5, 0.001, 0.1, 0.015, 0.1, 0.025, 0.2, 0.01, 0.5, 0.2, 0.5
  kfreq linseg 20, 0.01, 233, 0.5, 233
  kamp1 linseg 0,0.02,1,0.02,0
  atri vco2 1, kfreq, 12
  awhite random -1, 1
  kfcut linseg 20, 1, 20000
  anoise atone awhite, kfcut
  
  aout = kamp0 * (knoisamp * (anoise) + kamp1 * atri)
 
afilt lpf18	 aout, 19000, 1.1, 1
;aout2 lpf18	 a2, 19000, 1.1, 2

ga_bluemix_4_0 +=  aout+afilt
ga_bluemix_4_1 +=  aout

	endin

	instr 6	;spine
icps = cpspch(p4)
;foutir gihand,0,1,int(p4),frac(p4)*100

icoef=1.005
iepsilon=0.002
ivel=0.3
igain = 0.2

asaw0 vco2 0.25,icps/(icoef+iepsilon)/icoef
asaw1 vco2 0.25,icps/icoef
asaw2 vco2 0.25,icps
asaw3 vco2 0.25,icps*(icoef-iepsilon)
asaw4 vco2 0.25,icps*(icoef+iepsilon)*(icoef-iepsilon)
asaw=asaw1+asaw2+asaw3+asaw4;+asaw0

abody linsegr 0,0.01,1,0.01,1,0.1,0
arnd random -ivel,ivel
aattack linseg 1,0.005,0
apad=igain*(arnd*aattack+asaw*abody)

ga_bluemix_5_0 +=  apad
ga_bluemix_5_1 += apad

	endin

	instr 7	;vampyr_lead
icps = cpspch(p4)
;foutir gihand,0,1,int(p4),frac(p4)*100


imaster = p5/127
kamp linseg imaster,6,imaster,0.1,0

i1=0.25
i2=2
i3=0.2
i4=0.1
kexenv expseg 0.9999,i1,0.5,i2,0.2,i3,0.1,i4,0.1
;kexenv expseg 0.999,0.6,0.52,1.2,0.929
;kcps linseg 30,0.5,5
;kexenv lfo 0.2,kcps
ain      vco2 0.1540000141, icps, 2, kexenv;(1+kexenv)*kex 


ka1 = 0.999687818289
ktype = 1

aleft,aright wavesolve ain, icps, ka1, ktype
aleft moogladder aleft, 4560, 0.6
aright moogladder aright, 4560, 0.6
ga_bluemix_6_0 +=  kamp*aleft
ga_bluemix_6_1 += kamp*aright

	endin

	instr BlueMixer	;Blue Mixer Instrument
ga_bluemix_1_0, ga_bluemix_1_1	blueEffect0	ga_bluemix_1_0, ga_bluemix_1_1
ga_bluemix_1_0, ga_bluemix_1_1	blueEffect1	ga_bluemix_1_0, ga_bluemix_1_1
ktempdb = ampdb(gk_blue_auto8)
ga_bluemix_1_0 *= ktempdb
ga_bluemix_1_1 *= ktempdb
ga_bluesub_Master_0	+=	ga_bluemix_1_0
ga_bluesub_Master_1	+=	ga_bluemix_1_1
ga_bluemix_2_0, ga_bluemix_2_1	blueEffect2	ga_bluemix_2_0, ga_bluemix_2_1
ktempdb = ampdb(gk_blue_auto12)
ga_bluemix_2_0 *= ktempdb
ga_bluemix_2_1 *= ktempdb
ga_bluesub_Master_0	+=	ga_bluemix_2_0
ga_bluesub_Master_1	+=	ga_bluemix_2_1
ktempdb = ampdb(gk_blue_auto13)
ga_bluemix_3_0 *= ktempdb
ga_bluemix_3_1 *= ktempdb
ga_bluesub_Master_0	+=	ga_bluemix_3_0
ga_bluesub_Master_1	+=	ga_bluemix_3_1
ktempdb = ampdb(gk_blue_auto14)
ga_bluemix_4_0 *= ktempdb
ga_bluemix_4_1 *= ktempdb
ga_bluesub_Master_0	+=	ga_bluemix_4_0
ga_bluesub_Master_1	+=	ga_bluemix_4_1
ga_bluemix_5_0, ga_bluemix_5_1	blueEffect3	ga_bluemix_5_0, ga_bluemix_5_1
ktempdb = ampdb(gk_blue_auto18)
ga_bluemix_5_0 *= ktempdb
ga_bluemix_5_1 *= ktempdb
ga_bluesub_Master_0	+=	ga_bluemix_5_0
ga_bluesub_Master_1	+=	ga_bluemix_5_1
ga_bluemix_6_0, ga_bluemix_6_1	blueEffect0	ga_bluemix_6_0, ga_bluemix_6_1
ktempdb = ampdb(gk_blue_auto22)
ga_bluemix_6_0 *= ktempdb
ga_bluemix_6_1 *= ktempdb
ga_bluesub_Master_0	+=	ga_bluemix_6_0
ga_bluesub_Master_1	+=	ga_bluemix_6_1
ktempdb = ampdb(gk_blue_auto23)
ga_bluesub_Master_0 *= ktempdb
ga_bluesub_Master_1 *= ktempdb
outc ga_bluesub_Master_0, ga_bluesub_Master_1
ga_bluemix_1_0 = 0
ga_bluemix_1_1 = 0
ga_bluemix_2_0 = 0
ga_bluemix_2_1 = 0
ga_bluemix_3_0 = 0
ga_bluemix_3_1 = 0
ga_bluemix_4_0 = 0
ga_bluemix_4_1 = 0
ga_bluemix_5_0 = 0
ga_bluemix_5_1 = 0
ga_bluemix_6_0 = 0
ga_bluemix_6_1 = 0
ga_bluesub_Master_0 = 0
ga_bluesub_Master_1 = 0

	endin

