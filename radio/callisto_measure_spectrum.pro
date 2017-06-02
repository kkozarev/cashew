;+====================================================================
pro test_callisto_measure_spectrum

;Run the program like this to combine four files at different times
;and frequency ranges
;timerange='2011-May-11 02:' + ['27:00','36:00']
;timerange='2013-Nov-19 10:' + ['22:10','29:50']
freqrange=[20,390]

  ;You can run this for a single or few events, like so
  one=1
  if one eq 1 then begin
     label='131119_01'
     events=load_events_info(label=label)
     for ev=0,n_elements(events)-1 do $
        callisto_measure_spectrum,events[ev];,freqrange=freqrange ;timerange=timerange,
  endif
  
  ;Alternatively, run for all events, like this
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do $
        callisto_measure_spectrum,events[ev];,freqrange=freqrange ;timerange=timerange,
  endif

end
;-===============================================================================



;+===============================================================================
PRO callisto_measure_spectrum,event,timerange=timerange,station=station,freqrange=freqrange,plot=plot
;PURPOSE
; This procedure loads Callisto data, plotting a spectrum for a given event for the duration of available AIA data.
;
;CATEGORY:
;AIA/Radio.Callisto
;
;INPUTS:
;       event - an event called by event_load_info()
;
;OPTIONAL INPUT:
;       timerange - a two-element string array with the data in a specific format.
;                   For example, ['2011-NOV-05 10:20:30','2011-NOV-05 10:36:00']
;       station - the name of the radio observatory. For plotting purposes only, for now.
;       freqrange - a two-element integer/float array with lower,higher frequency, in MHz.
;                   For example, [40,180]
;       fit - if keyword set, attempt a second-order polynomial fit to the brightest emission
;       plot - if keyword set, plot the data.
;OUTPUT:
;
;OPTIONAL OUTPUT:
;     
;DEPENDENCIES:
;callisto_file_search, radio_spectro_fits_read (SSW)
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 01/29/2014


;Get the proper files to load
  callisto_file_search,event,files,station=statn
  
  if files[0] eq '' then begin
     print,''
     print,'No eCallisto data available for event '+event.label+'!'
     print,''
     return
  endif
  
;The proper time range of the Callisto data is determined, in the appropriate format.
  if not keyword_set(timerange) then begin
     timerange=strarr(2)
     ;prepare the event time range, based on the AIA event time range
     tmp=strsplit(anytim(event.st,/vms),'- :.',/extract)
     timerange[0]=tmp[2]+'-'+tmp[1]+'-'+tmp[0]+' '+tmp[3]+':'+tmp[4]+':'+tmp[5]
     tmp=strsplit(anytim(event.et,/vms),'- :.',/extract)
     timerange[1]=tmp[2]+'-'+tmp[1]+'-'+tmp[0]+' '+tmp[3]+':'+tmp[4]+':'+tmp[5]
  endif

  set_plot,'x'
  loadct,8
  tvlct,rr,gg,bb,/get
  tvlct,reverse(rr),reverse(gg),reverse(bb)
  !P.font=1
  !p.position=[0.12,0.17,0.95,0.92]
  
  nfiles=n_elements(files)
  nff=n_elements(files[*,0])
  ntf=n_elements(files[0,*])
  
  if not keyword_set(station) then station=statn
;Scan the sizes of the different files by reading them once first and recording the array sizes.
;This will allow to combine them later without errors.
  nxtot=0
  nytot=0
  nx=0
  for ff=0,nff-1 do begin
     for tf=0,ntf-1 do begin
        if find_file(files[ff,tf]) eq '' then begin
           print,''
           print,'Error: File "'+ files[ff,tf]+'" was not found. Exiting...'
           print,''
           return
        endif
        radio_spectro_fits_read,files[ff,tf],z1,x1,y1
        if n_elements(x1) gt nx then nx=n_elements(x1)
        if ff eq 0 then nxtot+=nx
     endfor
     nytot+=n_elements(y1)
  endfor
  if nx gt nxtot then nxtot=nx
  zz=dblarr(nxtot,nytot)
   

  yst=0
  yen=0
;Do the actual reading and combining of the radio files
  for ff=0,nff-1 do begin
     xst=0
     xen=0
     for tf=0,ntf-1 do begin
        if find_file(files[ff,tf]) eq '' then begin
           print,''
           print,'Error: File "'+ files[ff,tf]+'" was not found. Exiting...'
           print,''
           return
        endif
        radio_spectro_fits_read,files[ff,tf],z1,x1,y1
        xen=xst+n_elements(x1)-1
        yen=yst+n_elements(y1)-1
        if tf eq 0 then x=x1 else x=[x,x1]
        if xen eq n_elements(zz[*,0]) then xen--
        if yen eq n_elements(zz[0,*]) then yen--
        zz[xst:xen,yst:yen]=z1
        xst=xen
     endfor
     if ff eq 0 then y=y1 else y=[y,y1]
     yst=yen
  endfor

  
;Fix the frequencies
  if nff gt 1 then elimwrongchannels, zz, x, y
  
;Do a constant background subtraction on the intensities
  zb=constbacksub(zz, /auto)
  
;Select the frequency ranges
  if keyword_set(freqrange) then freqrng=freqrange else $
     freqrng = [y[n_elements(y)-1],y[0]]   
  frng=[min(where(freqrng[1]-y gt 0.0)),min(where(freqrng[0]-y gt 0.0))]
  if frng[0] eq -1 then frng[0]=0
  if frng[1] eq -1 then frng[1]=n_elements(y)-1

;Select the time ranges
  if keyword_set(timerange) then $
     timrng=[max(where(anytim(timerange[0])-x gt 0.0)),max(where(anytim(timerange[1])-x gt 0.0))] $
  else $
     timrng=[0,n_elements(x)-1]
  pzb=zb
  pzb[where(zb lt 0.0)]=0.0
  
  window,0,xsize=1200,ysize=800  ; for XWIN only!
;  device, set_resolution=[1200,800], SET_PIXEL_DEPTH=24, DECOMPOSED=0  ;For the Z-buffer

  spectro_plot, reverse(pzb,2), x, reverse(y) ,/xs, /ys, $
                xrange = timerange, thick=2,$
                yrange = [freqrng[0],freqrng[1]], ytitle = 'Frequency [MHz]', $
                title='eCallisto ('+station+') radio spectrum, event '+event.label,charsize=3,charthick=4
  
  
;Select points on the plot, and overplot
  print,''
  print,'This program will let you select points on the radio plot.'
  print,'LEFT-click on the plot to select a new point for analysis.'
  print,'You can select as many points as you need.'
  print,'You do not need to select in chronological order.'
  print,'When done selecting points, RIGHT-click to continue with the analysis.'
  cc=0
  xselectvals=0
  yselectvals=0
  while (!MOUSE.button ne 4) do begin
     CURSOR, x1, y1, /data, wait=3000
     wait,0.1
     PLOTS,x1, y1, /data,psym=2,symsize=3
     if cc eq 0 then begin
        xselectvals=x1
        yselectvals=y1
     endif else begin
        xselectvals=[xselectvals,x1]
        yselectvals=[yselectvals,y1]
     endelse
     cc++
     print,'Point #'+strtrim(string(cc),2)+' selected: x='+strtrim(string(x1),2)+', y='+strtrim(string(y1),2)
  endwhile
  print,''
  image=tvrd(/true)
  write_png,event.callistopath+'callisto_'+event.date+'_'+event.label+'_spectrum.png',image
  
  ;Keep only the unique clicks, in case the user repeatedly clicked in the same spot
  uniqvals=uniq(xselectvals)
  xselectvals=xselectvals[uniqvals]
  yselectvals=yselectvals[uniqvals]
  
  ;Order the selections chronologically
  order=sort(xselectvals)
  xselectvals=xselectvals[order]
  yselectvals=yselectvals[order]
  

;Convert the selected frequencies to coronal densities, assuming emission at
;1st harmonic.
;Then convert coronal densities to coronal heights, assuming a simple
;model like Newkirk (1961): r = 4.0 / (2 * log(f) - 12.824 - log(4))
  rad = 4.0 / (2 * alog10(yselectvals*1.e6) - 12.824 - alog10(4))
  time=xselectvals-xselectvals[0]

  if n_elements(time) lt 3 then begin
     print,''
     print,'Not enough points selected! Quitting.'
     print,''
     return
  endif
  
  fit=poly_fit(time,rad,2) 
  y0=fit[0]
  v0=fit[1]
  a=fit[2]
  curvefit=y0+v0*time+a*time^2
  
  rsuninkm=695700.
  accel=a*rsuninkm*1000
  initspeed=v0*rsuninkm
  
  calculate_savgol_kinematics, time, rad*rsuninkm, distance=distance, speed=speed, acceleration=acceleration, /plot
  
  set_plot,'x'
  tvlct,rr,gg,bb,/get
  tvlct,rr,gg,bb
  loadct,0
  !p.multi=0
  !p.background=255
  !p.color=0
  !p.thick=2
  !x.thick=2
  !y.thick=2
  !p.charsize=2
  !p.charthick=2
  !p.position=[0.15,0.15,0.9,0.9]
  wdef,0,1200,800
  plot,time,rad,xthick=2,ythick=2,thick=2,psym=4,symsize=3,$
       charsize=3,charthick=2,xstyle=1,ystyle=1,$
       xtitle='Time [sec]',ytitle='Distance [R!DSun!N]',title='Inferred Shock Radial Position'
  oplot,time,curvefit,thick=2
  xyouts,0.16,0.85,'r = r!D0!N + v!D0!Nt + at!U2!N',charsize=4,charthick=3,/norm
  xyouts,0.16,0.8,'r!D0!N = '+strtrim(string(y0,format='(f5.2)'),2)+' R!DSun!N',charsize=4,charthick=3,/norm
  xyouts,0.16,0.75,'v!D0!N = '+strtrim(string(initspeed,format='(f7.2)'),2)+' km/s',charsize=4,charthick=3,/norm
  xyouts,0.16,0.7,'a = '+strtrim(string(accel,format='(f7.2)'),2)+' m/s!U2!N',charsize=4,charthick=3,/norm
  
  image=tvrd(/true)
  write_png,event.callistopath+'callisto_'+event.date+'_'+event.label+'_shock_kinematics.png',image

  
end
;-====================================================================
