pro plot_wave_sphere_measurement
;A quick-plot program for the circle
;measurements of AIA EUV waves 
;01/16/2012 Kamen A. Kozarev

;+========================================
  ;User Input Parameters
  wav='193'
  evindex=11 ;the index of the event. See 'evnums' below
  init=62
;-========================================


  wavelength=['171','193','211','335','094','131','304']
  evnums=['05','06','07','13','19','20','23','32','37','38','41','113']
  sts=['2011/01/25 11:56:00','2011/01/27 11:50:00','2011/01/28 00:45:00',$
       '2011/02/11 12:30:00','2011/03/07 19:35:00','2011/03/08 03:30:00',$
       '2011/03/12 15:20:00','2011/04/27 02:05:00','2011/05/11 02:10:00',$
       '2011/05/29 10:00:00','2012/01/05 06:56:00','2010/06/13 05:35:00']
  evnum=evnums[evindex]
  std=strsplit(sts[evindex],'/ :',/extract)
  date=std[0]+std[1]+std[2]
  eventname='AIA_'+date+'_'+evnum+'_'+wav
  datapath='/Volumes/PLUME/AIA_data/studies/2011events/e'+evnum+'/'
  outpath=datapath
  restore,datapath+eventname+'_shocklocations.sav'
  !P.charthick=1.8

;=================================================================================
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
  

  write_png,outpath+eventname+'shock_radius.png',tvrd(true=1),rr,gg,bb
  

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
  

  write_png,outpath+eventname+'shock_front_positions.png',tvrd(true=1),rr,gg,bb

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
  
  
  write_png,outpath+eventname+'_shock_radius_expansion_rate.png',tvrd(true=1),rr,gg,bb

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
  
  write_png,outpath+eventname+'_shock_radial_front_velocity.png',tvrd(true=1),rr,gg,bb
;=========================================================



;=========================================================
end
