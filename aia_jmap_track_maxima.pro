pro test_aia_jmap_track_maxima
;Load the time height data and any additional information

wave=['193']
label='37'
event=load_events_info(label=label)
path=event.savepath
rev='000'
datafile=path+'jmap_data_'+label+'_'+wave[0]+'_r'+rev+'.sav'
infofile=path+'jmap_info_'+label+'_'+wave[0]+'_r'+rev+'.sav'


radrange=[1.28,1.7] ;radial extent of the measurement
numplotmax=2 ;number of maxima to track
dynamic_range=[-90,45]

aia_jmap_track_maxima,datafile,infofile,path=path,numplotmax=numplotmax,allmaxima=allmaxima,nmax=nmax,allgfits=allgfits,time=time,distance=distance,dynamic_range=dynamic_range,radrange=radrange;/gaussfit,
end


PRO aia_jmap_track_maxima,datafile,infofile,gaussfit=gaussfit,radrange=radrange,path=path,numplotmax=numplotmax,allmaxima=allmaxima,nmax=nmax,allgfits=allgfits,time=time,distance=distance,dynamic_range=dynamic_range,refine=refine
;PURPOSE:
;Detect and fit the AIA emission maxima
;CATEGORY:
;AIA/Kinematics
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
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/2013
;
  set_plot,'x'
  resolve_routine,'jmap_find_maxima',/either,/compile_full_file;,/no_recompile
  !P.font=1
  !p.position=[0.1,0.13,0.93,0.92]
  RSUN=6.955e5 ;solar radius in km
  cols=[0,120,100,160,200,230] ;an array of color values to use for plotting
  if not keyword_set(path) then path = './'
  restore,datafile
  restore,infofile
  data_subindex=data_subindex[plotted_frame_initial:plotted_frame_final]
  wav=strtrim(string(data_subindex[0].wavelnth),2)
  if not keyword_set(dynamic_range) then dynamic_range=[-100,100]
  
;How many maxima to track
  if not keyword_set(numplotmax) then numplotmax=2
  
;Plot the time-height map
  polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,time=time,rad=rad,dynamic_range=dynamic_range
  
  distance=rad
  loadct,0,/silent
  tvlct,rr,gg,bb,/get
  tvlct,reverse(rr),reverse(gg),reverse(bb)
  
  ntimes=n_elements(time)
;dtime=time[1:ntimes-1]-time[0:ntimes-2]
  nrad=n_elements(rad)
  
  if not keyword_set(radrange) then begin
     radrange=[1.08,max(rad)]
  endif else begin
     if radrange[0] lt min(rad) then radrange[0]=min(rad)
     if radrange[1] gt max(rad) then radrange[1]=max(rad)
  endelse
  radrange=double(radrange)
  
  oplot,[min(time),max(time)],[radrange[0],radrange[0]],thick=3,linestyle=2,color=0
  oplot,[min(time),max(time)],[radrange[1],radrange[1]],thick=3,linestyle=2,color=0
  data=data_thin_wave
  index=data_subindex[0:ntimes-1]
  timrng=[0,ntimes-1]
  
;Find the radial range index
  radrng=[min(where(rad-radrange[0] ge -1.e-6)),min(where(rad-radrange[1] ge -1.0e-6))]
 

;============================================
;Part 2. Find and fit the time-height maxima
;============================================
  wdef,1,900,500

  ;Find the first numplotmax maxima for every time, return them.
  jmap_find_maxima,data,time,rad,xrange=[time[timrng[0]],time[timrng[1]]],yrange=radrange,$
                   allgfits=allgfits,allmaxima=allmaxima,$
                   mymaxima=mymaxima,nmax=nmax,$
                   numplotmax=numplotmax
  
  
  
;Overplot the time-height diagram again, with the maxima as points.
  wset,0
  polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,dynamic_range=dynamic_range
  loadct,39,/silent

;plot horizontal lines for the range of radial heights
  oplot,[min(time),max(time)],[radrange[0],radrange[0]],thick=3,linestyle=2,color=255
  oplot,[min(time),max(time)],[radrange[1],radrange[1]],thick=3,linestyle=2,color=255
  
  colors=[255,190,250,60,30]
  for timind=timrng[0], timrng[1] do begin
     tmp=where(allmaxima[*,timind-timrng[0]].ind eq 0)
     nmaxpts=min(tmp)
     
;Plot the maxima   
     for mm=0,nmaxpts-1 do begin
        if mm eq numplotmax then break
        if timind lt ntimes-1 then dt = time[timind+1]-time[timind]
                                ;Plot the data maxima with errors
        if not keyword_set(gaussfit) then begin
           
;         plots,time[timind]+dtime[timind]/2.0,rad[mymaximind[mm,timind-timrng[0]]],$
;               color=colors[mm],psym=1,symsize=1,thick=4
           plots,time[timind]+dt/2.0,rad[allmaxima[mm,timind-timrng[0]].ind],$
                 color=colors[mm],psym=1,symsize=1,thick=4
           oploterror,time[timind]+dt/2.0,dt,$
                      rad[allmaxima[mm,timind-timrng[0]].ind],0.0,color=colors[mm],thick=2,/nohat
        endif else begin
                                ;Plot the maxima of the gaussian fits, together with the errors
           plots,time[timind]+dt/2.0,allgfits[mm,0,timind-timrng[0]],$
                 color=colors[mm],psym=1,symsize=1,thick=4
           oploterror,time[timind]+dt/2.0,dt,allgfits[mm,0,timind-timrng[0]],$
                      allgfits[mm,3,timind-timrng[0]],color=colors[mm],thick=2,/nohat
        endelse
     endfor
  endfor

  
;Plot the legend for the different maxima
  for mm=0,numplotmax-1 do begin
     if mm le 1 then polyfill,[0.953,0.99,0.99,0.953],[0.785,0.785,0.815,0.815]-mm*0.05,/norm,color=0
     plots,0.96,0.8-mm*0.05,psym=1,symsize=1,thick=4,/norm,color=colors[mm]
     xyouts,0.961,0.79-mm*0.05,' #'+strtrim(string(mm+1),2),color=colors[mm],/norm,charsize=1.6
  endfor
  
  
;+========================================================
;Fit the maxima positions for determination of kinematics.
;=========================================================
  
  uinput=0.0
  while uinput le 0.0 or uinput gt numplotmax do $
     read,uinput,prompt='Which set of maxima would you like to fit? (1-'+strtrim(string(numplotmax),2)+')'
  uinput--
  
  
  print,''
  print,'Select starting point:'
  cursor,x,y,/down,/data
  plots,x,y,psym=5,symsize=2,thick=2,color=100
  sp=min(where(fix(time-x) gt 0.0))-1
  ;print,sp,time[sp]
  
  
  print,'Select ending point:'
  cursor,x,y,/down,/data
  plots,x,y,psym=5,symsize=2,thick=2,color=200
  ep=min(where(fix(time-x) gt 0.0))-1
  
;record the image
  image=tvrd(/true)
  savname=data_evnum+'_'+data_date+'_'+wav+'emission_maxima_all.png'
  write_png,path+savname,image
  
;Search for the edges of the wave
  tmp={val:0.0D,ind:0L}
  wave_frontedge=replicate(tmp,ep-sp+1);dblarr(ep-sp+1)
  wave_backedge=replicate(tmp,ep-sp+1)
  ;wave_backedge=dblarr(ep-sp+1)
  for ii=sp,ep do begin
     x=rad[allmaxima[uinput,ii].ind:*]
    ;y=smooth(reform(data[ii,allmaximind[uinput,ii]:*]),40,/edge_truncate)
     
     if keyword_set(refine) then begin
        if ii gt sp then begin
           if rad[allmaxima[uinput,ii].ind] lt wave_backedge[ii-sp-1] or rad[allmaxima[uinput,ii].ind] lt rad[allmaxima[uinput,ii-1].ind] - 20./data_subindex[0].r_sun then begin
             ;Refine the positions of the maxima
              print,''
              print,'Refining the positions of the maximum...'
              
              tmp=rad[allmaxima[0:nmax[ii]-1,ii].ind] 
              res=where(tmp ge wave_backedge[ii-sp-1].val and tmp le wave_frontedge[ii-sp-1].val)
              if res[0] eq -1 then begin
                 cnt=0
                 print,'res[0]=-1'
                 while res[0] eq -1 do begin
                    res=where(tmp ge wave_backedge[ii-sp-1].val-cnt*2.0 and tmp le wave_frontedge[ii-sp-1].val+cnt*2.0)
                    cnt++
                 endwhile
                 
              endif else begin
                 tmp=rad[allmaxima[res,ii].ind]
                 maxy=max(tmp,ind)
                 maxind=allmaxima[res[ind],ii].ind
                 allmaxima[uinput,ii].ind=maxind
                 allmaxima[uinput,ii].val=maxy
              endelse
           endif
           
        endif
     endif
     
;Find the front edge of the wave
     y=reform(data[ii,allmaxima[uinput,ii].ind:*])
     tmp=min(where(y le 0.2*max(y)))
     
     wave_frontedge[ii-sp].val=rad[allmaxima[uinput,ii].ind+tmp]
     wave_frontedge[ii-sp].ind=allmaxima[uinput,ii].ind+tmp
                                ;plots,time[ii]+dt/2.0,rad[allmaximind[uinput,ii]+tmp],$
                                ;      color=colors[uinput],psym=4,symsize=1,thick=1
     
;Find the back edge of the wave
     y=reform(data[ii,0:allmaxima[uinput,ii].ind])
     tmp=max(where(y le 0.2*max(y)))
     wave_backedge[ii-sp].val=rad[tmp]
     wave_backedge[ii-sp].ind=tmp
                                ;plots,time[ii]+dt/2.0,rad[tmp],$
                                ;      color=colors[uinput],psym=4,symsize=1,thick=1
     
     oplot,[time[ii]+dt/2.0,time[ii]+dt/2.0],[wave_backedge[ii-sp].val,wave_frontedge[ii-sp].val],$
           color=colors[uinput],thick=1
  endfor
  
  
;Overplot just the points to be fitted.
  polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,dynamic_range=dynamic_range
  loadct,39,/silent
;plot horizontal lines for the range of radial heights
  oplot,[min(time),max(time)],[radrange[0],radrange[0]],thick=3,linestyle=2,color=255
  oplot,[min(time),max(time)],[radrange[1],radrange[1]],thick=3,linestyle=2,color=255

  
  for ii=sp,ep do begin
     plots,time[ii]+dt/2.0,rad[allmaxima[uinput,ii].ind],$
           color=colors[uinput],psym=1,symsize=1,thick=4
     oploterror,time[ii]+dt/2.0,dt,$
                rad[allmaxima[uinput,ii].ind],0.0,color=colors[uinput],thick=2,/nohat
     oplot,[time[ii]+dt/2.0,time[ii]+dt/2.0],[wave_backedge[ii-sp].val,wave_frontedge[ii-sp].val],color=colors[uinput],thick=1
  endfor
  
;Do second order polynomial fitting for the maximum
  print,''
  print,'Fitting a second-order polynomial to the wave peak positions...'
  raddist=reform(rad[allmaxima[uinput,sp:ep].ind])
  times=time[sp:ep]
  bootstrap_sdo,raddist,times,fit_line, p1, p2, p3, s1, s2, s3
  
  tmp={main:0.0D,min:0.0D,max:0.0D}
  wave_fits=replicate(tmp,n_elements(times))

  wave_fits.main=p1[0] + p2[0] * (times)+ 0.5 * p3[0] * (times)^2
  wave_fits.min=(p1[0]-s1[0]) + (p2[0]-s2[0]) * (times)+ 0.5 * (p3[0]-s3[0]) * (times)^2
  wave_fits.max=(p1[0]+s1[0]) + (p2[0]+s2[0]) * (times)+ 0.5 * (p3[0]+s3[0]) * (times)^2
  
  oplot,times+dt/2.0,wave_fits.main,thick=3
  oplot,times+dt/2.0,wave_fits.min,thick=2,linestyle=2
  oplot,times+dt/2.0,wave_fits.max,thick=2,linestyle=2

;Print out the results of the fitting
;initial height
  r0=rad[allmaxima[uinput,sp].ind]
  tmpstr='R!D0!N = '+strtrim(string(r0,format='(f9.5)'),2)+' +/-'+strtrim(string(s1[0],format='(f9.5)'),2)+' Rs'
  print,tmpstr
  xyouts,!P.position[0]+0.02,!P.position[3]-0.04,tmpstr,/norm,charsize=2
  
;final height
  rf=rad[allmaxima[uinput,ep].ind]
  tmpstr='R!D1!N = '+strtrim(string(rf,format='(f9.5)'),2)+' Rs'
  print,tmpstr
  xyouts,!P.position[0]+0.02,!P.position[3]-2*0.04,tmpstr,/norm,charsize=2
  
;initial speed
  v0=p2[0]*RSUN
  errv0=s2[0]*RSUN
  tmpstr='V!D0!N = '+strtrim(string(v0,format='(f9.2)'),2)+' +/-'+strtrim(string(errv0,format='(f9.2)'),2)+' km/s'
  print,tmpstr
  xyouts,!P.position[0]+0.02,!P.position[3]-3*0.04,tmpstr,/norm,charsize=2
  
;final speed
  tmp=p2[0]+p3[0]*(time[sp:ep]-time[sp])
  vf=tmp[n_elements(tmp)-1]*RSUN
  tmpstr='V!D1!N = '+strtrim(string(vf,format='(f9.2)'),2)+ ' km/s'
  print,tmpstr
  xyouts,!P.position[0]+0.02,!P.position[3]-4*0.04,tmpstr,/norm,charsize=2  
  
;acceleration
  accel=p3[0]*RSUN*1000.0
  erraccel=s3[0]*RSUN*1000.0
  tmpstr='a = '+strtrim(string(accel,format='(f9.2)'),2)+' +/-'+strtrim(string(erraccel,format='(f9.2)'),2)+' m/s^2'
  print,tmpstr
  print,''
  xyouts,!P.position[0]+0.02,!P.position[3]-5*0.04,tmpstr,/norm,charsize=2    
  
;record the image
  image=tvrd(/true)
  savname=data_evnum+'_'+data_date+'_'+wav+'emission_maxima.png'
  write_png,path+savname,image
  
;Save the output:
  wave_times=time[sp:ep]
  wave_rads=reform(rad[allmaxima[uinput,sp:ep].ind])
  wave_indices=reform(allmaxima[uinput,sp:ep].ind)
  ind=data_subindex[sp:ep]
  
  wave_data=data_thin_wave[sp:ep,allmaxima[uinput,sp].ind:allmaxima[uinput,ep].ind]
  savname=data_evnum+'_'+data_date+'_'+wav+'_jmap_measurements.sav'
  save,filename=path+savname,time,rad,ind,wave_times,wave_rads,wave_indices,wave_data,$
       wave_frontedge,wave_backedge,r0,rf,v0,vf,errv0,accel,erraccel,wave_fits,$
       allgfits,allmaxima



;+=========================================
;Plot the instantaneous velocity - this will be
;interesting to plot as a function of radial
;distance from the Sun, and compare with
;modeling results!
;==========================================
  savname=path+data_evnum+'_'+data_date+'_'+wav+'_instantaneous_velocity'
  set_plot,'ps'
  device,file=savname+'.eps',/inches,xsize=9.0,ysize=7,$
         /encaps,/color,/helvetica
  v=deriv(times,raddist*RSUN)
  plot,time,v,xrange=[min(times),max(times)],yrange=[-1000,1000],/xs,/ys,$
       charsize=2,charthick=3,xtitle = 'Time [sec]',ytitle='V!Dinst!N[km/s]',$
       title='Instantaneous Front Speed',background=255,color=0,thick=2
  
  device,/close
  exec='convert -flatten '+savname+'.eps '+savname+'.png ; rm -rf '+savname+'.eps '
  spawn,exec
;-==========================================



;+=========================================
;Plot a time series of the FWHM of the fits
;==========================================

  if keyword_set(gaussfit) then begin
     wdef,2,900,500
     loadct,0,/silent
     yrange=[min(allgfits[0:numplotmax-1,2,*]),max(allgfits[0:numplotmax-1,2,*])]
     yrange[1]=[0.1]
     xrange=[min(time),max(time)]
     
     plot,time,allgfits[0,2,*],xrange=xrange,yrange=yrange,/xs,$
          charsize=2,charthick=3,xtitle = 'Time [sec]',ytitle='FWHM [Rs]',$
          title='FWHM of fitted peaks',/nodata,background=255,color=0
     for mm=uinput,uinput do begin
        col=cols[mm]
        oplot,time,allgfits[mm,2,*],psym=10,color=col,thick=2
                                ;oploterror,time,time[1:ntimes-1]-time[0:ntimes-2],allgfits[0,2,*],fltarr(ntimes),$
                                ;           /nohat,color=col,thick=2
     endfor
     image=tvrd(/true)
     savname=data_evnum+'_'+data_date+'_'+wav+'_gaussfit_fwhm_timeseries.png'
     write_png,path+savname,image
  endif
;-==========================================
  
  
  
  
;+=================================================
;Plot a time series of the Fitted Peaks Intensities
;==================================================
  
  if keyword_set(gaussfit) then begin
     wdef,3,900,500
     loadct,0,/silent
     yrange=[min(allgfits[0:numplotmax-1,1,*]),max(allgfits[0:numplotmax-1,1,*])]
     xrange=[min(time),max(time)]
     
     plot,time,allgfits[0,1,*],xrange=xrange,yrange=yrange,/xs,/ys,$
          charsize=2,charthick=3,xtitle = 'Time [sec]',ytitle=' [ADU]',$
          title='Fitted Peaks Intensities',/nodata,background=255,color=0
     
     for mm=uinput,uinput do begin
        col=cols[mm]
        oplot,time,allgfits[mm,1,*],psym=10,color=col,thick=2
                                ;oploterror,time,allgfits[mm,1,*],allgfits[mm,4,*],$
                                ;           /nohat,color=col,thick=2
;,time[1:ntimes-1]-time[0:ntimes-2]
     endfor
     image=tvrd(/true)
     savname=data_evnum+'_'+data_date+'_'+wav+'_gaussfit_peak_intensities_timeseries.png'
     write_png,path+savname,image
  endif else begin
     
;+=================================================
;Plot a time series of the Maxima intensities
;==================================================
     ;wdef,3,900,500
     set_plot,'ps'
     !p.position[0]+=0.06 
     loadct,0,/silent   
     savname=path+data_evnum+'_'+data_date+'_'+wav+'_peak_intensities_timeseries'     
     device,file=savname+'.eps',/inches,xsize=9.0,ysize=7,$
            /encaps,/color,/helvetica
     
     yrange=[min(allmaxima[0:numplotmax-1,*].val),max(allmaxima[0:numplotmax-1,*].val)]
     xrange=[min(time),max(time)]
     
     plot,time[sp:ep],allmaxima[0,sp:ep].val,xrange=xrange,yrange=yrange,/xs,/ys,$
          charsize=2,charthick=4,xtitle = 'Seconds since '+data_subindex[0].date_obs,ytitle=' [ADU?]',$
          title='Wave Peak Intensities',/nodata,background=255,color=0,$
          xthick=4,ythick=4,thick=4
     
     
     for mm=uinput,uinput do begin
        col=cols[mm]
        oplot,time[sp:ep],allmaxima[mm,sp:ep].val,psym=10,color=col,thick=4
                                ;oploterror,time,allgfits[mm,1,*],allgfits[mm,4,*],$
                                ;           /nohat,color=col,thick=2
;,time[1:ntimes-1]-time[0:ntimes-2]
     endfor
     ;image=tvrd(/true)
     ;if keyword_set(outpath) then savname=outpath+savname+'.png'
     ;write_png,savname,image
     device,/close
     exec='convert -flatten '+savname+'.eps '+savname+'.png ; rm -rf '+savname+'.eps '
     spawn,exec
     set_plot,'x'


;START DEBUG!!!
;+=================================================
;Plot a time series of the wave intensities integrated
;between the front and back edges of the wave for all 
;time steps.
;==================================================

     savname=path+data_evnum+'_'+data_date+'_'+wav+'_integral_intensities_timeseries'
     set_plot,'ps'
     device,file=savname+'.eps',/inches,xsize=9.0,ysize=7,$
            /encaps,/color,/helvetica
     
     yrange=[min(allmaxima[0:numplotmax-1,*].val),max(allmaxima[0:numplotmax-1,*].val)]
     xrange=[min(time),max(time)]
     
     for ii=sp,ep do begin
        beg=wave_backedge[ii-sp].ind
        fin=wave_frontedge[ii-sp].ind
        if ii eq sp then wavtotintens=total(data[ii,beg:fin]) else wavtotintens=[wavtotintens,total(data[ii,beg:fin])]
     endfor
     wavtotintens=reform(wavtotintens)
     
     
     yrange=[min(wavtotintens),max(wavtotintens)]
     
     plot,time[sp:ep],wavtotintens,xrange=xrange,yrange=yrange,/xs,/ys,$
          charsize=2,charthick=4,xtitle = 'Seconds since '+data_subindex[0].date_obs,ytitle=' [ADU?]',$
          title='Wave Integral Intensities',/nodata,background=255,color=0,$
          xthick=4,ythick=4,thick=4
     
     
     for mm=uinput,uinput do begin
        col=cols[mm]
        oplot,time[sp:ep],wavtotintens,psym=10,color=col,thick=4
                                ;oploterror,time,allgfits[mm,1,*],allgfits[mm,4,*],$
                                ;           /nohat,color=col,thick=2
;,time[1:ntimes-1]-time[0:ntimes-2]
     endfor
     ;image=tvrd(/true)
     ;if keyword_set(outpath) then savname=outpath+savname+'.png'
     ;write_png,savname,image
     device,/close
     exec='convert -flatten '+savname+'.eps '+savname+'.png ; rm -rf '+savname+'.eps '
     spawn,exec
;END DEBUG!!!
     

;+=================================================
;Plot a time series of the wave thickness
;==================================================
     savname=data_evnum+'_'+data_date+'_'+wav+'_wave_thickness_timeseries'     
     device,file=savname+'.eps',/inches,xsize=9.0,ysize=7,$
            /encaps,/color,/helvetica
     
     wavethick=wave_frontedge.val-wave_backedge.val
     
     yrange=[min(wavethick),max(wavethick)]
     plot,time[sp:ep],wavethick,xrange=xrange,yrange=yrange,/xs,/ys,$
          charsize=2,charthick=4,xtitle = 'Seconds since '+data_subindex[0].date_obs,ytitle='R!DS!N',$
          title='Wave width',background=255,color=0,$
          xthick=4,ythick=4,thick=4,psym=10
     device,/close
     exec='convert -flatten '+savname+'.eps '+savname+'.png ; rm -rf '+savname+'.eps '
     spawn,exec
     set_plot,'x'
  endelse
;-=================================================
  
end
