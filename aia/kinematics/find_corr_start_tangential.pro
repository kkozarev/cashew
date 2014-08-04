pro find_corr_start_tangential, data, time, yarray, datastruct, ht_km, height, fitrange, yrng, mind, maxRadIndex,$
                     startInd=startInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge,$
                     startCorr=startCorr, constrain=constrain, wave_backedge=wave_backedge
;PURPOSE                                                                                                  
;Procedure to find the improved and final start position of the
;EUV wave.
;                                                                                                         
;INPUTS                                                                                                   
;     DATA - annulus data from aia_annulus_analyze_radial.pro                                             
;     TIME - array of times to corresponding annulus data                                                 
;     YARRAY - array of radii from annulus plots
;     MYMAXIMA - maxima located between start and end times
;     WAVE_BACKEDGE - structure containing backedge positions
;     STARTIND - current index of the start of EUV wave
;     WAVE_FRONTEDGE - structure containing frontedge positions
;     MAXRADINDEX - Radius of last viable AIA data
;                                                      
;OUTPUTS                                                                                                  
;     STARTEND - updated index of front end position         
;     STARTCORR - number of corrections applied to end index
;    


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

  frontDiff = abs(wave_frontedge[0].rad - mymaxima[0,startInd].rad)        
        
  print, yrng

; Iteratively find the start position

; Difference between max and frontedge is small
; so we haven't found the front of the wave yet
; skip to the next timestep and update maxima
; and wave edge information
  if frontDiff lt 0.0075 then begin
     
     startCorr++
     startInd++

     if debug eq 1 then begin
        help, wave_frontedge
        print, wave_frontedge
     endif

     fitrange[0] = startInd
     datastruct.xfitrange=fitrange
     
     ; Update maxima
     aia_jmap_find_maxima,data,time.relsec,yarray,mymaxima=mymaxima,allmaxima=allmaxima,$
                          yrange=[yarray[datastruct.yfitrange[0]],yarray[datastruct.yfitrange[1]]],$
                          numplotmax=3
       print, yrng

       if keyword_set(constrain) then begin
                                ; Constrain maxima
          maxinds=jmap_filter_maxima_tangential(time.relsec, mind, ht_km, height, allmaxima,fitrange=datastruct.xfitrange) ;,outliers=outliers
          mymaxima=maxinds

          print, yrng
       endif

     ; Compute the edge with the new maxima
     find_wave_edge_tangential, data, yarray, yrng, time, fitrange, mymaxima, mind,$
                          maxRadIndex, datastruct=datastruct, wave_frontedge=wave_frontedge, wave_backedge=wave_backedge

     print, yrng
     if debug eq 1 then begin
        help, wave_frontedge
        print, wave_frontedge
     endif
           
     ; Recursively check the next start position
     find_corr_start_tangential, data, time, yarray, datastruct, ht_km, height, fitrange, yrng, mind, maxRadIndex,$
                      startInd=startInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge,$
                      startCorr=startCorr, wave_backedge=wave_backedge
   
  endif else begin

     ; frontDiff is not small, so we have found the wave
     ; return and continue with the new startInd
     return
     
  endelse

  return

end



     


     


 
