function get_range_indices, values, valrange
;This function returns the start/end indices within an array that correspond to two binding values.
;If the values are outside of the range of the values array, it will return the range [0,nv], where nv is the number of values.
  nv=n_elements(values)
  indrange=lonarr(2)
  if nv eq 1 then begin
     print,'Input array must have more than one value. Quitting...'
     return,0
  endif

  tmp=min(where(values ge valrange[0]))
  if tmp eq -1 then indrange[0]=0 else indrange[0]=tmp
  tmp=max(where(values le valrange[1]))
  if tmp eq -1 then indrange[1]=nv-1 else indrange[1]=tmp
  
  return,indrange
  
end


pro test_aia_iris_density,saved=saved
;Test the comparison of AIA and IRIS density diagnostics
  event=load_events_info(label='140331_01')
  savename=event.aschdempath+'aia_iris_density'
  ;Time range
  trange='2014-03-31T'+['07:59','08:15']+':00'
  
  ;trange='2014-03-31T'+['07:50','07:53']+':00'
  date=event.date
;y-range and x-range of the IRIS slit, in arcseconds
  yrange=[-260,-140]
  xrange=[977.36,977.36]
    
 
  if keyword_set(saved) then begin
     res=file_search(savename+'.sav')
     if res[0] ne '' then begin
        restore, savename+'.sav'
     endif else begin
        print,'No DEM files found for event '+event.label+'. Quitting...'
        return
     endelse
  endif else begin

;Get the pixel to arcsec transformation of AIA data
  aia_transform_px2arcsec,event,axcoords,aycoords
  
;Find the pixel indices
  yind=where(aycoords ge yrange[0] and aycoords le yrange[1])
  ny=n_elements(yind)
  xind=min(where(axcoords ge xrange[0]))
  
  
;Find all the DEM result files
;aschdem_20140331_140331_01_075936_teem_map.sav
  ;demfiles=file_search(event.aschdempath+'aschdem_'+event.date+'_'+event.label+'*_teem_map.sav')
  totdemfiles=file_basename(file_search(event.aschdempath+'aschdem_'+event.date+'_'+event.label+'*_teem_tot.sav'))
  if totdemfiles[0] ne '' then begin
     nt=n_elements(totdemfiles)
     emarray=dblarr(nt,n_elements(yind))
     times=dblarr(nt)
     for ff=0,nt-1 do begin
        restore,event.aschdempath+totdemfiles[ff]
        emarray[ff,*]=reform(10^emlog[xind,yind])
        res=strsplit(totdemfiles[ff],'_',/extract)
        if n_elements(res) eq 6 then strtime=res[3] else strtime=res[4]
        tmp=anytim2jd(strmid(date,0,4)+'-'+strmid(date,4,2)+'-'+strmid(date,6,2)+'T'+$
                      strmid(strtime,0,2)+':'+strmid(strtime,2,2)+':'+strmid(strtime,4,2))
        times[ff]=tmp.int+tmp.frac  
     endfor
     
     tmp=anytim2jd(trange[0])
     tmp1=tmp.int+tmp.frac
     tmp=anytim2jd(trange[1])
     tmp2=tmp.int+tmp.frac
     trange=[tmp1,tmp2]
     trangeind=get_range_indices(times,trange)
     
     times=times[trangeind[0]:trangeind[1]]
     nt=n_elements(times)
     emarray=emarray[trangeind[0]:trangeind[1],*]
     
     save,filename=savename+'.sav',/variables,/compress
     
     
     
  endif else begin
     print,'No DEM files found for event '+event.label+'. Quitting...'
     return
  endelse
  
endelse
  
  los_depth=1.0e2                ;line of sight depth, in km
  los_depth*=1.e5               ;convert to cm
     
;DO THE PLOTTING HERE!
     wdef,0,1000,900
     loadct,4,/silent
     tvlct,rr,gg,bb,/get
     !P.position=[0.14,0.14,0.8,0.92]
     xrang=     [times[0],times[nt-1]]
     date_label=label_date(DATE_FORMAT='%H:%I')
     plot,times,aycoords,$
          xrange=xrang,$
          yrange=yrange,$
          ytickformat='(i4)',$
          xtickformat='LABEL_DATE',$
          xtickunit='Time',xtitle='Time (UT)',ytitle='Solar Y (arcsec)',$
          /nodata,$
          xstyle=1,ystyle=1,$
          charsize=2,charthick=2,$
          xticks=6,yticks=6,color=255
     
     ydata=aycoords[yind]
     
     ;Tried FFT for removal of noise - didn't really work...
     ;ffTransform = FFT(emarray)
     ;powerSpectrum = ABS(ffTransform)^2
     ;scaledPowerSpect = ALOG10(powerSpectrum) 
     ;scaledPS0 = scaledPowerSpect - MAX(scaledPowerSpect)
     ;mask = REAL_PART(scaledPS0) GT -7
     ;maskedTransform = ffTransform*mask
     ;emarray_fft = REAL_PART(FFT(maskedTransform, /inverse))
     
     density=alog10(sqrt(emarray/los_depth)) ;/(2*sqrt(2*alog(2)))
     
     
     min= 10.2;min(density)           ;10.2
     max=max(density)           ;11.2
     
;loadct,4,/silent
     for t=0,nt-2 do begin
        xmin=times[t]
        xmax=times[t+1]
        for r=0,ny-2 do begin
           ymin=ydata[r]
           ymax=ydata[r+1]
           color=255.0*(density[t,r]-min)/(max-min) ; Scale Colors
           if color lt 0 then color = 0
           if color gt 255 then color = 255
           polyfill,[xmin,xmax,xmax,xmin],[ymin,ymin,ymax,ymax],color=color,/data
        endfor
     endfor
     
     AXIS, YAXIS=0,YRANGE=yrange, color=255,ythick=1,yticks=6,ytickformat='(A1)',ystyle=1
     AXIS, YAXIS=1,YRANGE=yrange, color=255,ythick=1,yticks=6,ytickformat='(A1)',ystyle=1
     fcolorbar, MIN=min,MAX=max,Divisions=6, $
                Color=255,VERTICAL=1,RIGHT=1,TITLE='Log (Density / cm!U-3!N), LOS = '+$
                strtrim(string(los_depth/1.e5,format='(f8.1)'),2)+' km',$
                CHARSIZE=2, charthick=2, format='(f5.2)',Position=[0.88, 0.14, 0.9, 0.92]
     write_png,savename+'.png',tvrd(/true),rr,gg,bb
  
    
  
end
