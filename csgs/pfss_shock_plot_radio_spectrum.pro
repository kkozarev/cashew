pro test_pfss_shock_plot_radio_spectrum
;Test procedure pfss_shock_radio_spectrum
one=1
  if one eq 1 then begin
     labels=['110511_01','131212_01','130517_01','130423_01','120915_01','120526_01',$
             '120424_01','110607_01','110211_02','110125_01']
     labels=['131212_01']
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        pfss_shock_plot_radio_spectrum, event,/hires
     endfor
  endif
end


function cordens,rad
;Calculate the radial density spectrum form Mancuso & Avetta (2008)
dens=(2.99/rad^16 + 1.55/rad^6 + 2.1/rad^4 +0.08/rad^2.5) * 1.0e8 ; in cm^-3
return,dens
end


pro pfss_shock_plot_radio_spectrum, event,lores=lores,hires=hires

;PURPOSE:
;A procedure to plot an artificial type II radio spectrum based on
;the PFSS/shock model and AIA observations.
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;       event - an event structure 
;
;KEYWORDS:
;       hires - use a high resolution PFSS model
;       lores - use a low resolution PFSS model (default)
;OUTPUTS:
;
; 
;DEPENDENCIES:
;transform_volume, sym
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/21/2014
;
  set_plot,'x'
  close,/all
  evnum=event.label
  datapath=event.savepath
  mfhcpath=event.mfhcpath

;1. Load the results from the shock modeling
  ;Find a file to load with the latest results of applying the CSGS model
  ;csgsfile=find_latest_file(event.mfhcpath+'csgs_results_*')
  csgsfile=file_search(event.mfhcpath+'csgs_results_'+event.date+'_'+event.label+'_lores.sav')
  if keyword_set(hires) then csgsfile=file_search(event.mfhcpath+'csgs_results_'+event.date+'_'+event.label+'_hires.sav')
  
  if csgsfile[0] eq '' then begin
     print,'The file to load is not properly set or does not exist. Quitting.'
     return
  endif  
if csgsfile eq '' then begin
   print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif
  print,''
  print,'Loading data...'
  ;Load the CSGS model results
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
   
  dt=time[1:nsteps-1]-time[0:nsteps-2]
  strtime=subindex[*].date_obs
  tm=anytim2jd(strtime)
  tm=1.D*tm.int+tm.frac
  xrng=[min(tm),max(tm)]
;!!DEBUG!!

  ;allcrossAngles, dt,nsteps
  ;time=findgen(nsteps)*dt
  angles=findgen(91)
 
  
  loadct,0,/silent
  !P.font=-1
  !P.position=[0.14,0.14,0.8,0.92]
  wdef,0,1000,800
  thlet='!4' + String("110B) + '!X'
  ;thlet='!9'+String("161B)+'!X'
  tit='1/cos('+thlet+'!DBN!N)!U2!N'

   ;work out the plotting min/max of the frequency axis:
  minfreq=100000.
  maxfreq=-100000.
  for sstep=0,nsteps-2 do begin
     ncrosses=allcrosses[sstep]
     rmag=reform(sqrt(crossPoints[sstep,0:ncrosses-1].rpx^2+$
                      crossPoints[sstep,0:ncrosses-1].rpy^2+$
                      crossPoints[sstep,0:ncrosses-1].rpz^2))*event.geomcorfactor
     dens=cordens(rmag)
     freq=8898.0*sqrt(dens)/1.0e6 ;Frequency in MHz
     if min(freq) lt minfreq then minfreq=min(freq)
     if max(freq) gt maxfreq then maxfreq=max(freq)
  endfor
  

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  plot,tm,angles,yrange=[minfreq-1,maxfreq+1],xrange=xrng,$
       xstyle=1,ystyle=1,background=255,XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
       color=0,/nodata,charsize=3,charthick=2,xticks=6,$
       title=tit,xtitle='Time of '+event.date,ytitle='Frequency [MHz]'
;'sin('+thlet+'!DBN!N)'
  loadct,13,/silent
  
 
;  maxf=0.0
;  minf=1.0e20
  minf=10.0
  maxf=100.
  
  for sstep=0,nsteps-2 do begin
     ncrosses=allcrosses[sstep]
     thb=crossPoints[sstep,0:ncrosses-1].thbn
     invcosthb=(1./(cos(!PI/180.*thb))^2)
     minf=10.0
     maxf=100.
     ;stop
     rmag=reform(sqrt(crossPoints[sstep,0:ncrosses-1].rpx^2+$
                      crossPoints[sstep,0:ncrosses-1].rpy^2+$
                      crossPoints[sstep,0:ncrosses-1].rpz^2))*event.geomcorfactor
     dens=cordens(rmag)
     freq=8898.0*sqrt(dens)/1.0e6 ;Frequency in MHz
     sorted_freq=freq[sort(freq)]
     sorted_invcosthb=invcosthb[sort(freq)]
     ;vshock=radiusmoments[sstep,1]
     ;shockInt=(vshock/100.0)^16*exp((100.0-vshock)/100.0)^2.2
     ;stop
     xmin=tm[sstep]
     xmax=tm[sstep+1]
     for i=0,ncrosses-2 do begin
        ymin=sorted_freq[i]
        ymax=sorted_freq[i+1];+1.0 ;Bin in 1 MHz frequency bins
        polyfill,[xmin,xmax,xmax,xmin],$
                 [ymin,ymin,ymax,ymax],$
                 color=254.0*((sorted_invcosthb[i]-minf)/(maxf-minf)),/data
;                 color=254.0*((thb[i]-min(thb))/(90.0-min(thb))),/data


;color=254.0*(sin(thb[i]*!PI/180.)),/data
;                  color=254.0*((thb[i]-0.0)/(90.0-0.0)),/data
                 ;color=254.0*((thb[i]-0.0)/(90.0-0.0)),/data
        
        ;plots,time[sstep],f[i],color=254.0*(sin(thb[i]*!PI/180.)),$
        ;      psym=sym(1),symsize=2,/data
     endfor
  endfor
  
  ;MIN=0.0,MAX=90.0
  fcolorbar, MIN=minf,MAX=maxf,Divisions=4, $
             Color=0,VERTICAL=1,RIGHT=1, TITLE=tit,$
             CHARSIZE=3,charthick=2, format='(f6.1)',Position=[0.88, 0.4, 0.9, 0.8]
;'sin('+thlet+'!DBN!N)'

  tvlct,rr,gg,bb,/get
  im=tvrd(true=1)
  write_png,datapath+event.date+'_'+event.label+'_pfss_shock_spectrogram.png',im,rr,gg,bb
  
  ;stop
;3 Get the radial distance of the crossing points for every time step
  
  
;4. Make a histogram plot of time on the X-axis versus
; f=C*sqrt(density) on the Y-axis. The color coding will be ThetaBn,
; which I will assume for now is directly proportional to the intensity (it
; should also depend on the shock speed).






end
