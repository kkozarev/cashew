pro find_corr_start, data, time, yarray, datastruct, ht_km, fitrange, yrng, mind, maxRadIndex,$
                     startInd=startInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge,$
                     startCorr=startCorr, constrain=constrain

  ; To print out additional information set debug to 1
  debug = 0

  nt = n_elements(time)
  dat=data
  
  ; Set the initial start correction to zero
  ;startCorr = 0

  ;; ind=where(dat lt 0.0)
  ;; if ind[0] gt -1 then dat[ind] = 0.0

  ;; ; Go through and sum up the intensities for each time step
  ;; totalPixVals=dblarr(nt)

  ;; for tt=0,nt-1 do begin
  ;;    tmp=total(dat[tt,*])
  ;;    totalPixVals[tt]=tmp
  ;; endfor
     

; Peform a secondary scan and bring up the end index depending
; on where the distance from the current frontedge to the 
; edge of valid radian data
  
     

     ; Compute the difference between the last point of valid data
     ; in topDiff, and the difference between the current maxima
     ; and the wave front edge, frontDiff
;     frontDiff = fltarr(n_elements(wave_frontedge))

  frontDiff = abs(wave_frontedge[0].rad - mymaxima[0,startInd].rad)        
        
                                ; Iteratively find the start position
  if frontDiff lt 0.0075 then begin
     
     startCorr++
     startInd++


     if debug eq 1 then begin
        help, wave_frontedge
        print, wave_frontedge
     endif

     fitrange[0] = startInd
     datastruct.xfitrange=fitrange
     
     aia_jmap_find_maxima,data,time.relsec,yarray,mymaxima=mymaxima,allmaxima=allmaxima,$
                          yrange=[yarray[datastruct.yfitrange[0]],yarray[datastruct.yfitrange[1]]],$
                          numplotmax=3
     
     if keyword_set(constrain) then begin

        maxinds=jmap_filter_maxima_radial(time.relsec,ht_km,allmaxima,fitrange=datastruct.xfitrange) ;,outliers=outliers
        mymaxima=maxinds

     endif

           
     find_wave_frontedge, data, yarray, yrng, time, fitrange, mymaxima, mind,$
                          maxRadIndex, datastruct=datastruct, wave_frontedge=wave_frontedge

     if debug eq 1 then begin
        help, wave_frontedge
        print, wave_frontedge
     endif
           
     find_corr_start, data, time, yarray, datastruct, ht_km, fitrange, yrng, mind, maxRadIndex,$
                      startInd=startInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge,$
                      startCorr=startCorr
   
  endif else begin

     return
     ;; aia_jmap_find_maxima,data,time.relsec,yarray,mymaxima=mymaxima,allmaxima=allmaxima,$
     ;;                      yrange=[yarray[datastruct.yfitrange[0]],yarray[datastruct.yfitrange[1]]],$
     ;;                      numplotmax=3
     
     ;; if keyword_set(constrain) then begin
     ;;    maxinds=jmap_filter_maxima_radial(time.relsec,ht_km,allmaxima,fitrange=datastruct.xfitrange) ;,outliers=outliers
     ;;    mymaxima=maxinds
     ;; endif
     
     ;; find_wave_frontedge, data, yarray, yrng, time, fitrange, mymaxima, mind,$
     ;;                      maxRadIndex, datastruct=datastruct, wave_frontedge=wave_frontedge
     
  endelse

  return

end



     


     


 
