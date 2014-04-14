pro pfss_shock_plot_radio_spectrum;,datapath,fname
;PURPOSE:
;A procedure to plot an artificial type II radio spectrum based on the PFSS/shock model and AIA observations.
;CATEGORY:
;
;
;INPUTS:
;
;KEYWORDS:
;
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;fcolorbar
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;


  evnum='113'
  datapath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'+evnum+'/'
  fname='aia_shock_pfss_r2.0.sav'
  

;1. Load the results from the shock modeling
  restore, datapath+fname
  ;allcrossAngles, dt,nsteps
  time=findgen(nsteps)*dt
  angles=findgen(91)
  
  
  loadct,0,/silent
  !P.font=-1
  !P.position=[0.18,0.14,0.8,0.92]
  wdef,0,900,800
  thlet='!4' + String("110B) + '!X'
  tit=thlet+'!DBN!N'
  plot,time,angles,yrange=[40,180],xrange=[0,dt*(nsteps)],$
       xstyle=1,ystyle=1,background=255,$
       color=0,/nodata,charsize=3,charthick=2,$
       title=tit,xtitle='Time [sec]',ytitle='Frequency [MHz]'
;'sin('+thlet+'!DBN!N)'
  loadct,8,/silent
  
  
  maxf=0.0
  minf=1.0e20
  for sstep=0,nsteps-1 do begin
     ncrosses=allcrosses[sstep]
     thb=allcrossangles[sstep,0:ncrosses-1]
     cross_points=reform(allcrosspoints[sstep,*,0:ncrosses-1])
     rmag=reform(sqrt(cross_points[0,*]^2+$
                      cross_points[1,*]^2+$
                      cross_points[2,*]^2))/subindex[0].r_sun+1.0
     dens=aia_coronal_density(rmag,/ma08)
     f=8898.0*sqrt(dens)/1.0e6 ;Frequency in MHz
     vshock=radiusmoments[sstep,1]
     shockInt=(vshock/100.0)^16*exp((100.0-vshock)/100.0)^2.2
     
     xmin=time[sstep]
     if sstep eq nsteps-1 then xmax=time[sstep]+dt else xmax=time[sstep+1]
     for i=0,ncrosses-2 do begin
        ymin=f[i]
        ymax=f[i]+1.0 ;Bin in 1 MHz frequency bins
        polyfill,[xmin,xmax,xmax,xmin],$
                 [ymin,ymin,ymax,ymax],$
                 color=254.0*((thb[i]-0.0)/(90.0-0.0)),/data
;color=254.0*(sin(thb[i]*!PI/180.)),/data
;                  color=254.0*((thb[i]-0.0)/(90.0-0.0)),/data
                 ;color=254.0*((thb[i]-0.0)/(90.0-0.0)),/data
        
        ;plots,time[sstep],f[i],color=254.0*(sin(thb[i]*!PI/180.)),$
        ;      psym=sym(1),symsize=2,/data
     endfor
  endfor
  
  fcolorbar, MIN=0.0,MAX=90.0,Divisions=4, $
             Color=0,VERTICAL=1,RIGHT=1, TITLE=tit,$
             CHARSIZE=3, format='(f3.1)',Position=[0.9, 0.4, 0.92, 0.8]
;'sin('+thlet+'!DBN!N)'

  tvlct,rr,gg,bb,/get
  im=tvrd(true=1)
  write_png,datapath+'pfss_shock_spectrogram.png',im,rr,gg,bb
  
  ;stop
;3 Get the radial distance of the crossing points for every time step
  
  
;4. Make a histogram plot of time on the X-axis versus
; f=C*sqrt(density) on the Y-axis. The color coding will be ThetaBn,
; which I will assume for now is directly proportional to the intensity (it
; should also depend on the shock speed).






end
