pro test_ioniz
;Testing John Raymond's approach to finding the EM change in the solar corona
;Written by KAK, March 18, 2014
path='/home/kkozarev/Desktop/IONIZATION_RAYMOND/'
restore,path+'ionization_times.idl'
event=load_events_info(label='110511_01')
label=event.label
date=event.date
regs=['01','02','03','04','05','06','07','08']
DEVICE, DECOMPOSED=0
!P.color=0
!P.background=255
!P.thick=3
!X.thick=3
!x.style=1
!x.title='Time [s]'
!Y.thick=3
!y.style=1
!P.charsize=3
!P.charthick=4
!P.position=[0.15,0.12,0.95,0.9]
!p.font=1
fname='aschdem_'+event.date+'_'+event.label+'_teem_normalized_series_r'

;start with one region
restore,event.aschdempath+'50_px/'+fname+'01'+'.sav'
EM=10^emiss
time=(tm-tm[0])*86400.
dEMdt=deriv(time,EM)

plot,time,EM,ytitle='EM',title='EM vs. time, R1'
write_png,'em_time.png',tvrd(/true)

plot,time,demdt,ytitle='dEM/dt',title='dEM/dt vs. time, R1'
write_png,'demdt_time.png',tvrd(/true)


;Load the radial regions DEM results
inpath=event.aschdempath
radregs=['06','03','07','08']
for rr=0,n_elements(radregs)-1 do begin
   restore, 'aschdem_'+date+'_'+label+'_teem_normalized_series_r'+radregs[rr]+'.sav'
   stop
endfor

end 





pro test_ioniz_bak
;Testing John Raymond's approach to finding the EM change in the solar corona
;Written by KAK, March 18, 2014

  ;An array holding the temperatures
  temp=(dindgen(501)*5)/501.+4
  
  ions=['fe_11','fe_12']
  ;rates=dblarr(n_elements(ions))
  res=0.D
  for io=0,n_elements(ions)-1 do begin
     rates=ioniz_rate(ions[io],10^temp)
     res+=(1./rates)
  endfor
  rates211=1./res
  
  ions=['fe_11','fe_12','fe_13','fe_14']
  res=0.D
;rates=dblarr(n_elements(ions))
  for io=0,n_elements(ions)-1 do begin
     rates=ioniz_rate(ions[io],10^temp)
     res+=(1./rates)
  endfor
  rates335=1./res
stop
end