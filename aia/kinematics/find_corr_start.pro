pro find_corr_start, data, time, yarray, datastruct, ht_km, fitrange, yrng, mind, maxRadIndex,$
                     startInd=startInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge,$
                     startCorr=startCorr, constrain=constrain, wave_backedge=wave_backedge

  ; To print out additional information set debug to 1
  debug = 0

  nt = n_elements(time)
  dat=data
     

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

           
     find_wave_edge, data, yarray, yrng, time, fitrange, mymaxima, mind,$
                          maxRadIndex, datastruct=datastruct, wave_frontedge=wave_frontedge, wave_backedge=wave_backedge

     if debug eq 1 then begin
        help, wave_frontedge
        print, wave_frontedge
     endif
           
     find_corr_start, data, time, yarray, datastruct, ht_km, fitrange, yrng, mind, maxRadIndex,$
                      startInd=startInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge,$
                      startCorr=startCorr, wave_backedge=wave_backedge
   
  endif else begin

     return
     
  endelse

  return

end



     


     


 
