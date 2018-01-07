pro find_wave_radial_positions_auto, rad_data, exit_status
;PURPOSE
;Procedure to find automatically the edges of the EUV wave at multiple
;timesteps using Savitzky-Golay filtering, local minima/maxima
;ordering, and proximity/intensity metrics.
;
;INPUTS
;     RAD_DATA - annulus data structure
;     EXIT_STATUS - indicating how the process went
;
;OUTPUTS - UPDATES TO:
;     WAVE_FRONTEDGE - sub-structure of rad_data holding information
;                      about the wave front edge
;     WAVE_PEAK      - sub-structure of rad_data holding information
;                      about the wave peak
;     WAVE_BACKEDGE - sub-structure of rad_data holding information
;                      about the wave back edge
;
;
;AUTHOR: Kamen Kozarev, 05/20/2017
;
  
  exit_status += 0
  plot = 1
  debug = 0
  
  timefitrange=rad_data.timefitrange
  sp=timefitrange[0]
  ep=timefitrange[1]
  data=rad_data.data
  time=rad_data.time
  yarray=rad_data.y_rsun_array
  yrng=rad_data.radfitrange
  
  rad = yarray[yrng[0]:yrng[1]]
  radrange=[rad[0],rad[n_elements(rad)-1]]
  max_fwhm=(radrange[1]-radrange[0])/6
  dat = data[*,yrng[0]:yrng[1]]
  
  
  wave_frontedge=rad_data.wave_frontedge
  wave_backedge=rad_data.wave_backedge
  wave_peak=rad_data.wave_peak
  reached_front_edge=0 ;a flag to let us know the wave has reached the front edge
  
  if plot gt 0 then begin
     set_plot,'x'
     !p.multi=0
     !p.position=[0.12,0.12,0.95,0.93]
     !p.background=255
     !p.color=0
     wdef,0,800
     loadct,39,/silent
  endif
  
;LOOP OVER TIME STEPS
  ; Determine the wave edge for each timestep within the region of interest 
  for ii=sp, ep do begin
     coeff=0
     newtime = time.jd
     caldat, newtime[ii], m, d, y, h, m, s 

     
     ;---------------------------------------------------------
     ; Filter out the negative data
     col = reform(dat[ii,*])
     negInd = where(col lt 0) 
     if negInd[0] ne -1.0 then begin
        col[negInd] = 0
     endif
     ;---------------------------------------------------------
     
     
     ;---------------------------------------------------------
     ;Savitzki-Golay processing of the original data
     order = 0
     degree = 4
     sghw = 25
     while n_elements(col) le sghw*2. do sghw=fix(sghw/2.)+1
     savgol_filter=SAVGOL(sghw, sghw, order, degree)
     sgsmooth=CONVOL(reform(col), savgol_filter, /EDGE_TRUNCATE)
     ;Reduce the negativity of deep local minima
     veryneg=where(sgsmooth lt 0.)
     if veryneg[0] ne -1 then sgsmooth[veryneg]=-1.*sqrt(abs(0.1*sgsmooth[veryneg]))
     nallpts=n_elements(sgsmooth)
     ;---------------------------------------------------------

     
     ;---------------------------------------------------------
     ;Plot the original and smoothed data
     if plot eq 1 then begin
        plot, rad, sgsmooth, xtitle='Radial distance, R!DS!N',ytitle='Pixel intensity',charthick=2,$
              title=strtrim(string(ii),2)+' Wave J-map profile, '+time[ii].cashew_time,$
              /nodata,/xstyle,/ystyle,charsize=2
        oplot,rad,col,thick=1.5
        oplot,rad,sgsmooth,color=40,thick=2
     endif
     ;---------------------------------------------------------
     
     
     ;---------------------------------------------------------
     ;Determine the indices of the start/end of the fitting window
     if ii eq sp then wavstartind=0 else wavstartind = prev_wavstartind - 30
     if wavstartind lt 0 then wavstartind = 0
     if ii eq sp then wavendind=min(where(sgsmooth[wavstartind:*] lt 0.)) - 1 $
     else wavendind = prev_wavendind + 30
     if wavendind gt n_elements(sgsmooth)-1 then wavendind = n_elements(sgsmooth)-1
     if wavendind lt 0 then wavendind=n_elements(sgsmooth)-1
     ;---------------------------------------------------------
     
     ;---------------------------------------------------------
     ;Create the sub-arrays, where we will look for the wave signature.
     sgsubdata=sgsmooth[wavstartind:wavendind]
     ;sgsubdata=col[wavstartind:wavendind]
     sgsubrad=rad[wavstartind:wavendind]
     npts=n_elements(sgsubdata)
     ;---------------------------------------------------------

     ;This is only for plotting the data at each fitting window
     ;if plot eq 1 then begin
     ;   plot,sgsubrad,sgsubdata,yrange=[min(sgsubdata),max(sgsubdata)],/ystyle,/xstyle,thick=2
     ;   oplot,sgsubrad,fltarr(n_elements(sgsubrad))
     ;endif

     ;---------------------------------------------------------
     ;Find the peak location. Make sure the peak doesn't go back.
     peakinds=lclxtrem(sgsubdata,20,/maxima)
     peakvals=sgsubdata[peakinds]

     ordpeakinds=peakinds[reverse(sort(peakvals))]
     ordpeaks=peakvals[reverse(sort(peakvals))]
     
     ;Always assume that the wave is the brightest feature in the first step!
     peakind=ordpeakinds[0]
     ;Go along and find the peak that is close enough. 
     if ii gt sp then begin
        ctct=1
        while peakind lt prev_peakind - 10 do begin
           subordpeakinds=ordpeakinds[ctct:*]
           metric1 = reform(1. / sgsubdata[subordpeakinds])
           metric1i = sgsubdata[subordpeakinds]
           prevpeak = (sgsubrad[prev_peakind])[0]
           metric2 = abs(sgsubrad[subordpeakinds] - prevpeak)
           metric = [transpose(metric1),transpose(metric2)]
           multisort,metric,index=[0,1]
           peakind=subordpeakinds[where(metric2 eq metric[1,0])]
           ctct++
        endwhile
     endif
     peakRad=(sgsubrad[peakind])[0]
     peakData=(sgsubdata[peakind])[0]
     wave_peak[ii].rad = peakRad
     wave_peak[ii].xind=ii
     wave_peak[ii].yind = wavstartind+peakind
     wave_peak[ii].val = peakData
     
     if peakind eq nallpts-1 then reached_front_edge = 1
     ;---------------------------------------------------------     

     
     if plot eq 1 then begin
        oplot,[sgsubrad[peakind],sgsubrad[peakind]],[-max(sgsubdata),max(sgsubdata)]
     endif
     
     ;---------------------------------------------------------
     ;Find the back edge of the wave
     behind_data = sgsubdata[0:peakind]
     behind_rad = sgsubrad[0:peakind]
     nb=n_elements(behind_data)
     ;Find all minima behind the peak.
     minsep=20
     while minsep ge nb do minsep--
     behind_minind=lclxtrem(behind_data,minsep)
     ;Search for the lowest and closest minima to the peak
     if behind_minind[0] lt 0 then behind_minind = 0
     ;This is looking for the lowest minimum in range
     tmp=min(behind_data[behind_minind],tmpind)
     altbehminid = behind_minind[tmpind]
     ;Instead, use a metric taking into account distance from maximum and
     ;the relative sizes of the minima
     metric1 =  (abs(peakdata[0] - behind_data[behind_minind]))
     wt1 = 0.8
     metric2 = (abs(peakrad[0] - behind_rad[behind_minind]))
     wt2 = 1.
     metric = (metric1^wt1) * (metric2^wt2)
     behminid=behind_minind[(sort(metric))[0]]
     
     if ii gt sp then begin
        if (sgsubrad[prev_peakind]-prev_backedge) gt (2.*(peakrad-sgsubrad[behminid])) then begin
           ;print,(sgsubrad[prev_peakind]-prev_backedge) , 2.*((peakrad-sgsubrad[behminid]))
           behminid = altbehminid
        endif
     endif
     
     backEdge = sgsubrad[behminid]
     wave_backedge[ii].rad = backEdge
     wave_backedge[ii].yind = wavstartind+behminid
     wave_backedge[ii].xind = ii
     wave_backedge[ii].val = col[wave_backedge[ii].yind]
     ;---------------------------------------------------------
     

     
     ;---------------------------------------------------------
     ;Find the front edge of the wave
     ahead_data = sgsubdata[peakind:npts-1]
     ahead_rad = sgsubrad[peakind:npts-1]
     na=n_elements(ahead_data)
     if reached_front_edge gt 0 then begin
        ahminid = npts-1
     endif else begin
        ;Find all minima ahead of the peak.
        minsep=20
        while minsep ge na do minsep--
        ahead_minind=lclxtrem(ahead_data,minsep)
        ;Search for the lowest and closest minima to the peak
        if ahead_minind[0] lt 0 then ahead_minind = npts-1-peakind
        ;This is looking for the lowest minimum in range
        tmp=min(ahead_data[ahead_minind],tmpind)
        altahminid = peakind+ahead_minind[tmpind]

     ;Instead, use a metric taking into account distance from maximum and
       ;the relative sizes of the minima
        tmp = abs(peakdata[0] - ahead_data[ahead_minind])
        metric1 = tmp / mean(tmp)
        metric1 = 1/metric1
        wt1 = 0.5
        
        tmp = abs(peakrad[0] - ahead_rad[ahead_minind])
        metric2 = tmp / mean(tmp)
        wt2 = 0.5
        metric = (metric1*wt1) + (metric2*wt2)
        sortahminids=peakind+ahead_minind[(sort(metric))]
        ahminid=sortahminids[0]
        origahminid=ahminid
        if ii gt sp then begin
           if (prev_frontedge-sgsubrad[prev_peakind]) gt (2.*(sgsubrad[ahminid]-peakrad)) then begin
              ;print,(prev_frontedge-sgsubrad[prev_peakind]) , (2.*(sgsubrad[ahminid]-peakrad))
              if n_elements(sortahminids) gt 1 then ahminid = sortahminids[1]
           endif
        endif
     endelse
     ;Determine the front
     frontEdge = sgsubrad[ahminid]
     wave_frontedge[ii].rad = frontEdge
     wave_frontedge[ii].yind = wavstartind+ahminid
     wave_frontedge[ii].xind = ii
     wave_frontedge[ii].val = col[wave_frontedge[ii].yind]
     ;---------------------------------------------------------                           
     
     
     ;---------------------------------------------------------
     ;Some more plotting
     if plot eq 1 then begin
        ;local minima approach plotting
        oplot,[sgsubrad[behminid],sgsubrad[behminid]],[-max(sgsubdata),max(sgsubdata)],thick=2
        oplot,[sgsubrad[ahminid],sgsubrad[ahminid]],[-max(sgsubdata),max(sgsubdata)],thick=2
     endif
     ;---------------------------------------------------------
     
     if reached_front_edge then break
     wait,2
     prev_wavstartind=wavstartind
     if prev_wavstartind lt 0 then prev_wavstartind=0
     prev_wavendind=wavendind
     if prev_wavendind gt nallpts-1 then prev_wavendind = nallpts-1
     prev_peakind = peakind
     prev_backedge = backedge
     prev_frontedge = frontedge
  endfor
  
  if plot gt 0 then wdel,0
  rad_data.wave_frontedge=wave_frontedge
  rad_data.wave_backedge=wave_backedge
  rad_data.wave_peak=wave_peak
  
;Find the wave thicknesses and average intensities
;Wave thickness in Rsun
  rad_data.wavethick = rad_data.wave_frontedge.rad - rad_data.wave_backedge.rad
  
;Wave average intensity over time
  backinds = rad_data.wave_backedge.yind
  frontinds = rad_data.wave_frontedge.yind
  xrng = rad_data.timefitrange
  for tt = xrng[0], xrng[1] do $
     if backinds[tt] ge 0 and frontinds[tt] gt 0 and frontinds[tt] - backinds[tt] gt 0 $
     then rad_data.avgintense[tt] = mean(rad_data.data[tt,backinds[tt]:frontinds[tt]])
  if ii lt ep then rad_data.timefitrange[1]=ii
end
