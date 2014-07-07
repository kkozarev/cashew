pro test_aia_plot_wave_sphere_measurement
;Test this program


;You can run for one event, like this.
  one=0
  if one eq 1 then begin
     event=load_events_info(label='test')
     aia_plot_wave_sphere_measurement,event
  endif
  
  
;Alternatively, run for all events
  all=1
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
;n_elements(events)-1
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_plot_wave_sphere_measurement,event,wav=wavelength
        endfor
     endfor
  endif
end


end


pro aia_plot_wave_sphere_measurement,event,wav=wav
;PURPOSE:
;A quick-plot program for the circle measurements of AIA EUV waves 
;
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
;Written by Kamen Kozarev, 01/16/2012

;+========================================
 init=62
 if not keyword_set(wav) then wav='193'
 evnum=event.label
 savepath=event.savepath
 fname=evnum+'sphere_shocklocations.sav'
 date=event.date

if file_exist(savepath+fname) then begin
 restore,savepath+fname
endif else begin
 print,''
print,'File '+savepath+fname+' does not exist. Quitting...'
print,''
return
endelse
 
;1. Plot the resulting positions and velocities, show the parameters of the fit
  RSUN=6.96e5                   ;radius of the sun, in km.

;1.1 Plot the radius as a function of time.
  xrange=[min(time)/60.0-0.1,max(time)/60.0+0.1]
  
  reltime=strmid(subindex[init].date_obs,11,8)
  p=0
  wdef,0,1024
  tvlct,rr,gg,bb,/get
  min=min(radiusmoments[*,0]/RSUN)
  max=max(radiusmoments[*,0]/RSUN)
  yrange=[min/1.1,max*1.1]
  
  plot, time/60.0,radiusmoments[*,0]/RSUN,$
        psym=5,symsize=1,$
        title='EUV wave radius versus time, AIA/'+wav+' A',$
        xtitle='Time relative to '+reltime+' UT, '+date+', [min]', $
        ytitle='Wave radius, R!DS!N',$
        thick=4,xthick=2,ythick=2,charsize=2,$
        xrange=xrange,xstyle=1,$
        yrange=yrange,ystyle=1 ;,background=0,color=255
  
  ;overplot the second-order fit
  oplot, time/60.0, radiusfitlines/RSUN,thick=2;color=0
  oploterr, time/60.0,radiusmoments[*,0]/RSUN,float(radiussigma)/RSUN,psym=5,thick=2 ;,color=255


  xin=0.18
  yin=0.89
  dy=0.025
  xyouts,xin,yin,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N',$
         charsize=1.8,/normal;,color=0
  xyouts,xin,yin-dy,'r!D0!N = '+strtrim(string(radiusfitparams[0]/RSUN,format='(f15.2)'),2)+$
         ' R!DS!N +/- '+strtrim(string(radiusfitparamssigma[0]/RSUN,format='(f15.6)'),2) + ' R!DS!N',$
         charsize=1.8,/normal;,color=0
  xyouts,xin,yin-2*dy,'r!D0!N = '+strtrim(string(radiusfitparams[0],format='(f15.2)'),2)+' km +/- '+$
         strtrim(string(radiusfitparamssigma[0],format='(f15.2)'),2) + ' km',charsize=1.8,/normal;,color=0
  xyouts,xin,yin-3*dy,'v!D0!N = '+strtrim(string(radiusfitparams[1],format='(f15.2)'),2)+' km/s +/- '+$
         strtrim(string(radiusfitparamssigma[1],format='(f15.2)'),2) + ' km/s',charsize=1.8,/normal;,color=0
  xyouts,xin,yin-4*dy,'a = '+strtrim(string(radiusfitparams[2],format='(f15.2)'),2)+' km/s!U2!N +/- '+$
         strtrim(string(radiusfitparamssigma[2],format='(f15.2)'),2) + ' km/s!U2!N',charsize=1.8,/normal;,color=0
  

  write_png,event.savepath+'kinematics/'+eventname+'_shock_radius.png',tvrd(true=1),rr,gg,bb
  

;1.2 Plot the radial front location as a function of time.
  xrange=[min(time)/60.0-0.1,max(time)/60.0+0.1]
  
  reltime=strmid(subindex[init].date_obs,11,8)
  p=0
  wdef,0,1024
  tvlct,rr,gg,bb,/get
  min=min(frontcircmoments[*,0]/RSUN+1)
  max=max(frontcircmoments[*,0]/RSUN+1.0)
  yrange=[min/1.1,max*1.1]
  
  plot, time/60.0,frontcircmoments[*,0]/RSUN+1.0,$
        psym=5,symsize=1,$
        title='EUV wave front versus time, AIA/'+wav+' A',$
        xtitle='Time relative to '+reltime+' UT, '+date+', [min]', $
        ytitle='Radial distance, R!DS!N',$
        thick=4,xthick=2,ythick=2,charsize=2,$
        xrange=xrange,xstyle=1,$
        yrange=yrange,ystyle=1 ;,background=0,color=255
  
  ;overplot the second-order fit
  oplot, time/60.0, frontfitlines/RSUN+1.0,thick=2;color=0
  oploterr, time/60.0,frontcircmoments[*,0]/RSUN+1.0,float(frontpossigma)/RSUN,psym=5,thick=2 ;,color=255

  xin=0.18
  yin=0.89
  dy=0.025
  xyouts,xin,yin,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N',$
         charsize=1.8,/normal;,color=0
  xyouts,xin,yin-dy,'r!D0!N = '+strtrim(string(frontfitparams[0]/RSUN+1.0,format='(f15.2)'),2)+$
         ' R!DS!N +/- '+strtrim(string(frontfitparamssigma[0]/RSUN,format='(f15.6)'),2) + ' R!DS!N',$
         charsize=1.8,/normal;,color=0
  xyouts,xin,yin-2*dy,'r!D0!N = '+strtrim(string(frontfitparams[0],format='(f15.2)'),2)+' km +/- '+$
         strtrim(string(frontfitparamssigma[0],format='(f15.2)'),2) + ' km',charsize=1.8,/normal;,color=0
  xyouts,xin,yin-3*dy,'v!D0!N = '+strtrim(string(frontfitparams[1],format='(f15.2)'),2)+' km/s +/- '+$
         strtrim(string(frontfitparamssigma[1],format='(f15.2)'),2) + ' km/s',charsize=1.8,/normal;,color=0
  xyouts,xin,yin-4*dy,'a = '+strtrim(string(frontfitparams[2],format='(f15.2)'),2)+' km/s!U2!N +/- '+$
         strtrim(string(frontfitparamssigma[2],format='(f15.2)'),2) + ' km/s!U2!N',charsize=1.8,/normal;,color=0
  

  write_png,event.savepath+'kinematics/'+evnum+'_shock_front_positions.png',tvrd(true=1),rr,gg,bb

  stop

;1.3. Plot the rate of radius increase as a function of time
  loadct,0,/silent
  min=min(radiusmoments[*,1])
  max=max(radiusmoments[*,1])
  yrange=[min/1.1,max*1.1]
  plot, time/60.0,radiusmoments[*,1],$
        title='EUV wave derived expansion rate versus time, AIA/'+wav+' A',$
        xtitle='Time relative to '+reltime+' UT, '+date+', [min]', $
        ytitle='Expansion rate, km/s',$
        thick=4,xthick=2,ythick=2,charsize=1.8,$
        xrange=xrange,xstyle=1,$
        yrange=yrange,ystyle=1
  
  
  write_png,event.savepath+'kinematics/'+eventname+'_shock_radius_expansion_rate.png',tvrd(true=1),rr,gg,bb

  stop

;1.4. Plot the front velocity as a function of time
  loadct,0,/silent
  min=min(frontcircmoments[*,1])
  max=max(frontcircmoments[*,1])
  yrange=[min/1.1,max*1.1]
  plot, time/60.0,frontcircmoments[*,1],$
        title='EUV wave radial front velocity, AIA/'+wav+' A',$
        xtitle='Time relative to '+reltime+' UT, '+date+', [min]', $
        ytitle='Apparent velocity, km/s',$
        thick=4,xthick=2,ythick=2,charsize=1.8,$
        xrange=xrange,xstyle=1,$
        yrange=yrange,ystyle=1
  
  write_png,event.savepath+evnum+'_shock_radial_front_velocity.png',tvrd(true=1),rr,gg,bb
;=========================================================



;=========================================================
end
