;+====================================================================
pro test_callisto_plot_spectra

;Run the program like this to combine four files at different times
;and frequency ranges
;timerange='2011-May-11 02:' + ['27:00','36:00']
;timerange='2013-Nov-19 10:' + ['22:10','29:50']
freqrange=[20,390]


  ;You can run this for a single or few events, like so
  one=0
  if one eq 1 then begin
     label='131119_01'
     events=load_events_info(label=label)
     for ev=0,n_elements(events)-1 do $
        callisto_plot_spectra,events[ev];,freqrange=freqrange ;timerange=timerange,
  endif

  ;Alternatively, run for all events, like this
  all=1
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do $
        callisto_plot_spectra,events[ev];,freqrange=freqrange ;timerange=timerange,
  endif
  
end
;-===============================================================================



;+===============================================================================
PRO callisto_plot_spectra,event,timerange=timerange,station=station,freqrange=freqrange,fit=fit,plot=plot
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
  image=tvrd(/true)
  write_png,event.callistopath+'callisto_'+event.date+'_'+event.label+'_spectrum.png',image
  
  
;======================================
;Part 2. Find and fit the spectrum maxima
  if keyword_set(fit) then begin
     wdef,1,900,500
     
     allgfits=fltarr(100,5,timrng[1]-timrng[0]+1)
     allmaxima=fltarr(100,timrng[1]-timrng[0]+1)
     allmaximind=fltarr(100,timrng[1]-timrng[0]+1)
     mymaxima=fltarr(100,timrng[1]-timrng[0]+1)
     mymaximind=fltarr(100,timrng[1]-timrng[0]+1)
     nmax=intarr(timrng[1]-timrng[0]+1)
     
     for timind=timrng[0],timrng[1] do begin
        
;arr=smooth(reform(zb[timind,frng[0]:frng[1]]),4,/edge_truncate)
        arr=reform(zb[timind,frng[0]:frng[1]])
        plot,y[frng[0]:frng[1]],arr,xrange=[freqrng[0],freqrng[1]],$
             yrange=[min(arr),max(arr)],/xs,$
             charsize=2,charthick=3,xtitle = 'Frequency [MHz]' ;,thick=2
;Plot the mean value
;oplot,[freqrng[0],freqrng[1]],[mean(arr),mean(arr)],thick=2
        
        
;This function finds the local minima
        ind=lclxtrem(arr,10)
        
        
;Record the maxima in each of the intervals set by the minima locations.
        maxima=fltarr(100)
        indmaxima=intarr(100)
        gfit=fltarr(100,5)
        cc=0
        
        for ii=0,n_elements(ind)-1 do begin
                                ;plot the ranges of the local maximum intervals
           oplot,[y[ind[ii]+frng[0]],y[ind[ii]+frng[0]]],[min(zb),max(zb)]
           
                                ;find the local maximum in every interval
           if ii lt n_elements(ind)-1 then begin
              locarr=arr[ind[ii]:ind[ii+1]]
              locy=y[ind[ii]+frng[0]:ind[ii+1]+frng[0]]
              locmax=max(locarr)
              lmaxind=ind[ii]+!C+frng[0]
              
                                ;This is a filter that lets through only the biggest maxima.
                                ;locmax lt mean(arr) or 
              if (ind[ii+1]-ind[ii] le 6) then continue
              maxima[cc]=locmax
              indmaxima[cc]=lmaxind
              
              if keyword_set(gaussfit) then begin
                 res=gaussfit(locy,locarr,aa,nterms=6)
;Follow up with a Levenberg-Marquardt fitting algorightm
                 res=lmfit(locy,locarr,aa,/double,function_name='gaussianfit',sigma=sigma)
                 if cc eq 0 then print,aa[1],rad[lmaxind]
                 gfit[cc,0]=aa[1]             ;X-location (radial) of the peak
                 gfit[cc,1]=max(res)          ;Y-location (intensity) of the peak
                 gfit[cc,2]=2*sqrt(2*alog(2)*aa[2]^2) ;The FWHM of the gaussian fit
                 if gfit[cc,2] eq 'Inf' or gfit[cc,2] eq 'NaN' then gfit[cc,2]=1.0e-10
                 gfit[cc,3]=sigma[1] ;The error in radial position of the maximum
                 gfit[cc,4]=sigma[0] ;The error in the fitted peak value
                 
                 
                 zz=(locy-aa[1])/aa[2]
                                ;6-term fit
                 oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4]+locy^2*aa[5],linestyle=2,thick=2
                                ;5-term fit
                                ;oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4],linestyle=2,thick=2
                                ;4-term fit
                                ;oplot,locy,aa[3]+aa[0]*exp(-0.5*zz^2),linestyle=2,thick=2
                 plots,y[lmaxind],locmax,psym=2,symsize=2,thick=2,/data
                                ;print,2*sqrt(2*alog(2)*aa[2]^2)  ;The
                                ;FWHM of the gaussian fit - doesn't
                                ;                           seem right
                                ;                           though...
              endif
              cc++
           endif
        endfor
        nmax[timind]=cc
        maxima=maxima[0:cc-1]
        
        indmaxima=indmaxima[0:cc-1]
        
        sort=reverse(sort(maxima))
        ordmaxima=maxima[sort]
        ordindmaxima=indmaxima[sort]
        
        allmaximind[0:cc-1,timind-timrng[0]]=ordindmaxima
        allmaxima[0:cc-1,timind-timrng[0]]=ordmaxima
        
;for ii=0,n_elements(sort)-1 do begin
                                ;oplot,[y[ordindmaxima[ii]],y[ordindmaxima[ii]]],[min(zb),max(zb)],linestyle=3,thick=3
                                ;print,ordindmaxima[ii],ordmaxima[ii]
;endfor
        
;Other things to do:
;- Order the local maxima by size, and only record the N largest
;  maxima
;- Find the true FWHM of the gaussian fits
;- Do this for all time steps, plot the locations of the four largest maxima for every time on the
;spectrum
        
;wait,0.1
        
     endfor
     
     ;wdef,2,900,500
     spectro_plot, pzb, x,y ,/xs, /ys, $
                   xrange = timerange, $
                   yrange = [freqrng[0],freqrng[1]], ytitle = 'Frequency [MHz]', $
                   title='eCallisto ('+station+') radio spectrum, event '+event.label,charsize=3,charthick=4
     
     loadct,39,/silent
     colors=[255,190,250,60,30]
     numplotmax=3
     for timind=timrng[0], timrng[1] do begin
        tmp=where(allmaximind[*,timind-timrng[0]] eq 0)
        nmax=min(tmp)
        
        for mm=0,nmax-1 do begin
           if mm eq numplotmax then break
           plots,x[timind],y[allmaximind[mm,timind-timrng[0]]],$
                 color=colors[mm],psym=1,symsize=1,thick=4
        endfor
     endfor
     
     for mm=0,numplotmax-1 do begin
        if mm le 1 then polyfill,[0.933,0.97,0.97,0.933],[0.785,0.785,0.815,0.815]-mm*0.05,/norm,color=0
        plots,0.94,0.8-mm*0.05,psym=1,symsize=1,thick=4,/norm,color=colors[mm]
        xyouts,0.941,0.79-mm*0.05,' #'+strtrim(string(mm+1),2),color=colors[mm],/norm,charsize=1.6
     endfor
     
     image=tvrd(/true)
     write_png,event.callistopath+'callisto_'+event.date+'_'+event.label+'_emission_maxima.png',image
  endif
  set_plot,'x'
end
;-====================================================================
