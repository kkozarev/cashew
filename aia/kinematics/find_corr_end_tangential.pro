pro find_corr_end_tangential, data, time, rad, startInd=startInd, endInd=endInd, wave_frontedge=wave_frontedge,$
                   maxRadIndex=maxRadIndex, endCorr=endCorr
;PURPOSE                                                                                                  
;Procedure to find the improved and final end position of the
;EUV wave.
;                                                                                                         
;INPUTS                                                                                                   
;     DATA - annulus data from aia_annulus_analyze_radial.pro                                             
;     TIME - array of times to corresponding annulus data                                                 
;     RAD - array of radii from annulus plots
;     STARTIND - index of the start of EUV wave
;     WAVE_FRONTEDGE - structure containing frontedge positions
;     MAXRADINDEX - Radius of last viable AIA data
;                                                      
;OUTPUTS                                                                                                  
;     ENDIND - index of front end position         
;     ENDCORR - number of corrections applied to end index
;    


  ; To print out additional information set debug to 1
  debug = 1

  nt = n_elements(time)
  dat=data

  ind=where(dat lt 0.0)
  if ind[0] gt -1 then dat[ind] = 0.0


     ; Compute the difference between the last point of valid data
     ; in topDiff, and the difference between the current maxima
     ; and the wave front edge, frontDiff
  topDiff = fltarr(n_elements(wave_frontedge))
  for i=0, n_elements(wave_frontedge)-1 do begin
     topDiff[i] = abs(wave_frontedge[i].rad - rad[maxRadIndex])
     if debug eq 1 then begin
        print, "Current index: ", i
        print, "top diff: ", topDiff[i]
     endif
     
  endfor
     
     ; Update the endIndex to cutoff once we reach
                                ; the last location of valid data
  minEndArr = where(topDiff eq min(topDiff))
  if minEndArr[0] ne -1 then begin
     minEnd = minEndArr[0]
     endInd = minEnd + startInd
     endCorr = minEnd
     if endCorr eq n_elements(wave_frontedge) then begin
        endInd = minEnd+startInd
        endCorr = minEnd
     endif
  endif
  
  if debug eq 1 then begin
     print, "End Index is: ", endInd
     print, "End correction is: ", endCorr
     print, "End Time: ", time[endInd]
  endif

  return
  
end
