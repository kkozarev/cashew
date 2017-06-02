pro find_wave_edge_lateral, lat_data
;PURPOSE
;Procedure to find the edges of the EUV wave using a Gaussian fit
;At each timestep a Gaussian is fitted to the radial data and the
;wave edges are defined at +/- one sigma
;
;INPUTS
;     LAT_DATA - annulus data structure
;
;OUTPUTS
;     WAVE_FRONTEDGE - Structure storing the positions of the front of
;                      the wave
;     WAVE_BACKEDGE - Structure storing the positions of the back of
;                     the wave

  plot = 1
  debug = 0

  loadct, 0
  mymaxima=lat_data.mymaxima
  timefitrange=lat_data.timefitrange
  sp=timefitrange[0]
  ep=timefitrange[1]
  data=lat_data.data
  time=lat_data.time
  yarray=lat_data.x_deg_array
  yrng=lat_data.latfitrange
  
  lat = yarray[yrng[0]:yrng[1]]
  dat = data[*,yrng[0]:yrng[1]]
  
  mind=0
  nSigma = 1.5
  
  wave_frontedge=lat_data.wave_frontedge
  wave_backedge=lat_data.wave_backedge
  wave_peak=lat_data.wave_peak
  reached_front_edge=0 ;a flag to let us know the wave has reached the front edge
  
  set_plot,'x'
  if plot gt 0 then wdef,0,800
  !p.background=255
  !p.color=0
  loadct,39,/silent
  
;LOOP OVER TIME STEPS
  ; Determine the wave edge for each timestep within the region of interest 
  for ii=sp, ep do begin
     alt=0
     newtime = time.jd
     caldat, newtime[ii], m, d, y, h, m, s 
     
     col = dat[ii,*]
     colSmooth = reform(smooth(col, 8, /edge_truncate))
     
; Filter out the negative data
     average = mean(colSmooth)
     negInd = where(colSmooth lt 0) 
     if negInd[0] ne -1.0 then begin
        colSmooth[negInd] = 0;average 
     endif
     
     ; Fit a Gaussian to determine front and back edges
     gfit4 = mpfitpeak(lat, colSmooth, coeff, nterms=4)
     ;Check the previous FWHM location iteratively
     if ii gt sp then begin 
       if coeff[1] lt wave_backedge[ii-1].lat - 0.1 then $
           gfit4 = mpfitpeak(lat[10:*], colSmooth[10:*], coeff, nterms=4)
     endif
     FWHM=2*sqrt(2*alog(2))*coeff[2]
     
     
     if finite(coeff[0]) eq 0 then begin
        coeff=[0, 0, 0]
        alt=1
     endif
     
     if debug gt 0 then begin
        print,time[ii].cashew_time
        print, "COEFF"
        print,'peak value       peak centroid        Gaussian sigma'
        print, coeff
        print, "One Sigma"
        print, coeff[1]+nSigma*coeff[2]
     endif
     
     if plot eq 1 then begin
        ;print, h, ":", m
        
        plot, lat, colSmooth, xtitle='Lateral distance, deg',ytitle='Pixel intensity',charthick=2,$
              title=strtrim(string(ii),2)+' Wave Lat. J-map profile, '+time[ii].cashew_time,/nodata,charsize=2
        oplot,lat,colSmooth,color=40
        
        oplot, lat, gfit4, color=180
     endif
     
     
     gaussHeight = coeff[0]
     gaussCenter = coeff[1]
     gaussStdev = coeff[2]
     
     ; If the Gaussian fit is bad, use the searching down from
     ; intensity peak method instead
     if gaussHeight lt 0 then alt=1
     if gaussHeight gt 250 then alt=1
     if gaussCenter gt yarray[yrng[1]] then alt=1
     
     if alt eq 0 then begin
        ; Calculate front and back edges
        frontEdge = gaussCenter+nsigma*gaussStdev
        backEdge = gaussCenter-nsigma*gaussStdev
        
        ; Chop off the front edge at the last location of viable data
        if frontEdge ge yarray[yrng[1]] then begin 
           frontEdge = yarray[yrng[1]]
           if reached_front_edge eq 0 then reached_front_edge=1
        endif
        ;if frontEdge lt mymaxima[mind, ii].lat then frontEdge = mymaxima[mind,ii].lat
        
        ;Chop off the back edge at the first location of viable data
        if backEdge lt yarray[yrng[0]] then backEdge = yarray[yrng[0]]
        
        wave_frontedge[ii].lat = frontEdge
        tmp=min(where(yarray ge frontEdge))
        if tmp[0] gt -1 then wave_frontedge[ii].yind = tmp else wave_frontedge[ii].yind = 0
        if reached_front_edge eq 1 then wave_frontedge[ii].yind = yrng[1]
        wave_frontedge[ii].xind = ii
        ;wave_frontedge[ii].val = col[wave_frontedge[ii].yind]
        if wave_frontedge[ii].yind lt n_elements(gfit4) then $
           wave_frontedge[ii].val = gfit4[wave_frontedge[ii].yind] $
        else wave_frontedge[ii].val = col[wave_frontedge[ii].yind]
        
        wave_backedge[ii].lat = backEdge
        tmp=min(where(yarray ge backEdge))
        if tmp[0] gt -1 then wave_backedge[ii].yind = tmp else wave_backedge[ii].yind = 0
        wave_backedge[ii].xind = ii
        ;wave_backedge[ii].val = col[wave_backedge[ii].yind]
        if wave_backedge[ii].yind lt n_elements(gfit4) then $
           wave_backedge[ii].val = gfit4[wave_backedge[ii].yind] $
        else wave_backedge[ii].val = col[wave_backedge[ii].yind]
        
        
        wave_peak[ii].lat = gaussCenter
        wave_peak[ii].xind=ii
        tmp=min(where(yarray ge gaussCenter))
        if tmp[0] gt -1 then wave_peak[ii].yind = tmp else wave_peak[ii].yind = 0
        ;wave_peak[ii].val = col[wave_peak[ii].yind]
        wave_peak[ii].val = gaussHeight
        
        if plot eq 1 then begin   
           oplot, [frontEdge, frontEdge], [-10,40]
           oplot, [backEdge, backEdge], [-10,40]
        endif
        ; If Gaussian fit fails, use old method
     endif else begin
        ;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
        y=reform(lat_data.data[ii,mymaxima[mind,ii].ind:*])
        y=smooth(y,2,/edge_truncate)

        np=n_elements(y)
        tmp=min(where(y le 0.25*max(y)))
        if tmp[0] eq -1 then tmp=np-1
        wave_frontedge[ii].lat=yarray[mymaxima[mind,ii].ind+tmp]
        wave_frontedge[ii].yind=mymaxima[mind,ii].ind+tmp
        wave_frontedge[ii].xind=ii
         
        if wave_frontedge[ii].lat lt mymaxima[mind,ii].lat then begin
           wave_frontedge[ii].lat = mymaxima[mind,ii].lat
        endif
        
        ;Find the back edge of the wave
        
;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
        y=reform(lat_data.data[ii,0:mymaxima[mind,ii].ind])
        y=smooth(y,2,/edge_truncate)
        np=n_elements(y)
        y=reverse(y,1)          ;reverse the array so the search is the same
       
        tmp=min(where(y le 0.25*mymaxima[mind,ii].lat))
        if tmp[0] eq -1 then tmp=np-1
        
        wave_backedge[ii].yind=mymaxima[mind,ii].ind-tmp
        wave_backedge[ii].xind = ii
        wave_backedge[ii].lat = yarray[mymaxima[mind,ii].ind-tmp]
        
        if plot eq 1 then begin
           oplot, [wave_frontedge[ii].lat, wave_frontedge[ii].lat], [-10,40],color=220
           oplot, [wave_backedge[ii].lat, wave_backedge[ii].lat], [-10,40],color=220
        endif
        
     endelse
     
     ; Get keyboard input to iterate through plots
     ; during plot mode
     if plot eq 1 then begin
        ;print,''
        ;test = get_kbrd(1)
        stop
        wait,1
     endif
     
;     nearestStart =value_locate(lat, wave_frontedge[ii].lat)
;     nearestEnd = value_locate(lat, wave_backedge[ii].lat)    
;     if wave_backedge[ii].lat lt min(lat) then nearestEnd = lat[0]
;     if nearestStart eq -1.0 || nearestEnd eq -1.0 then begin
;        lat_data.avgIntense[ii] = -1.0
;     endif else begin
;        lat_data.avgIntense[ii] = mean(col[nearestEnd:nearestStart])
;     endelse
     
  endfor
  
  if plot gt 0 then wdel,0
  lat_data.wave_frontedge=wave_frontedge
  lat_data.wave_backedge=wave_backedge
  lat_data.wave_peak=wave_peak

;Find the wave thicknesses and average intensities
;Wave thickness in Rsun
  lat_data.wavethick=lat_data.wave_frontedge.lat-lat_data.wave_backedge.lat
;Wave average intensity over time
  backinds=lat_data.wave_backedge.yind
  frontinds=lat_data.wave_frontedge.yind
  xrng=lat_data.timefitrange
  for tt=xrng[0],xrng[1] do $
     lat_data.avgintense[tt]=mean(lat_data.data[tt,backinds[tt]:frontinds[tt]])
end
