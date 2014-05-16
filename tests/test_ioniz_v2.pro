pro test_ioniz_v2
;Testing John Raymond's approach to finding the EM change in the solar corona
;Written by KAK, March 18, 2014
;path='/home/kkozarev/Desktop/IONIZATION_RAYMOND/'
;restore,path+'ionization_times.idl'
  wdef,0,800
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
  ;fname='aschdem_'+event.date+'_'+event.label+'_teem_normalized_series_r'
  
;Load the radial regions DEM results
inpath=event.aschdempath

restore, inpath+'dem_'+date+'_'+label+'_teem_map_subrois.sav'

nregs=n_elements(roi_radheight)
ntimes=n_elements(times)
roitime=anytim(times)
roitime=roitime-roitime[0]
beta=fltarr(ntimes)
radregs=[5,2,6,7]
radrad=reform(roi_radheight[radregs])
nradregs=n_elements(radregs)
for tt=0,ntimes-1 do begin
   for rr=0,nregs-1 do begin
      emiss=reform(emdata[rr,tt,0:npix[rr]-1])
      if rr eq 0 and tt eq 0 then emarr=dblarr(nregs,ntimes)
      avgemiss=avg(10^emiss)
      emarr[rr,tt]=avgemiss
   endfor
   
;Do the fitting of the radial regions with mpfit
   fit_model = 'p[0] + (x)*p[1]'
   em=reform(emarr[radregs,tt])
   logem=alog(reform(emarr[radregs,tt]))
   lograd=alog(radrad)
   a_dist = size(em, /n_elements)
   h_error = replicate(1., a_dist)
   res = mpfitexpr(fit_model, lograd, logem, h_error, [alog(em[0]), -2.], perror=perror, /quiet)
   fit=exp(res[0])*radrad^res[1]
   beta[tt]=res[1]
   plot,radrad,em,psym=2,color=0,symsize=4,ystyle=0,xstyle=0,/ylog,$
        ytitle='EM', xtitle='Radial Distance, R!DS!N'
   oplot,radrad,fit,color=0

   ;wait,0.2
endfor


;This is the beta value to use.
avgbeta=avg(beta[0:9])
print,avgbeta

;Then, calculate alpha
alpha=0.5*(abs(avgbeta)+1)
print,alpha
stop

;f(alpha)
falpha=0.75

;The pre-event density
em=dblarr(nregs)
for rr=0,nregs-1 do em[rr]=avg(emarr[rr,0:9]) 
n0=sqrt(em/(roi_radheight*falpha))

;===================================
;Next, calculate the density jump, X
wav='193'
shockfile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed.sav'
restore,shockfile
;Calculate the shock radii
RSUN=ind_arr[0].RSUN_REF/(1000.0) ;Solar radius in km.
KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
fit=reform(rad_data.fitparams[0,*].front)
shocktime=(rad_data.time-rad_data.time[0])*3600.
shockrad=(fit[0]+fit[1]*shocktime+0.5*fit[2]*shocktime^2)/RSUN
shockrad*=event.geomcorfactor
shockrad=(shockrad-1.)*RSUN

;Find the overlap in time between the DEM calculations and the
;kinematics measurements
maxid=max(where(max(shocktime)-roitime ge 0.))
roitime=roitime[0:maxid]
shockids=intarr(maxid+1)
for tt=0,maxid do begin
   shockids[tt]=where(fix(shocktime - roitime[tt]) eq 0)
endfor
shocktime=shocktime[shockids]
;Calculate the shock speeds
vshock=rad_data.fitparams[1].front+shocktime*rad_data.fitparams[2].front

;Calculate the heights of the EM-measurement regions
roirad=(roi_radheight-1.)*RSUN


;Calculate the quantity dt/ds=sqrt((Rs^2-Rs*h))/(Vs*(2*Rs-h))
dtds=dblarr(nregs,n_elements(shockids))
shockrad=shockrad[shockids]
for rr=0,nregs-1 do dtds[rr,*]=(sqrt(shockrad^2-shockrad*roirad[rr]))/(vshock*(2*shockrad-roirad[rr]))


;Calculate dEM/dt
alldemdt=dblarr(nregs,maxid+1)
for rr=0,nregs-1 do begin
   EM=emarr[rr,0:maxid]
   dEMdt=deriv(roitime,EM)
   alldemdt[rr,*]=reform(demdt)
   
   plot,roitime,EM,ytitle='EM',title='EM vs. time, R'+strtrim(string(rr+1),2)
   write_png,'em_time.png',tvrd(/true)
   
   plot,roitime,demdt,ytitle='dEM/dt',title='dEM/dt vs. time, R'+strtrim(string(rr+1),2)
   write_png,'demdt_time.png',tvrd(/true)
endfor

;Finally, calculate the shock jump for all regions as a function of
;time
shockjump=dblarr(nregs,maxid+1)
for rr=0,nregs-1 do begin
   shockjump[rr,*]=sqrt(1.+(1/(n0[rr])^2)*alldemdt[rr,*]*dtds[rr,*])
   
endfor
print,minmax(shockjump)
stop

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
