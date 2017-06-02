pro find_corr_end_tangential, data, time, rad, startInd=startInd, endInd=endInd, wave_frontedge=wave_frontedge,$
                   maxRadIndex=maxRadIndex, endCorr=endCorr, maxFrontEdge=maxFrontEdge
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
  debug = 0

  nt = n_elements(time)
  dat=data

  ind=where(dat lt 0.0)
  if ind[0] gt -1 then dat[ind] = 0.0

  if debug eq 1 then begin
     print, "Current frontedge"
     help, wave_frontedge
     print, wave_frontedge
     
     print, "Max front edge value: "
     print, rad[maxFrontEdge]
  endif


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
     
     ; Test to see if we pass the maxFrontEdge value found by
     ; filtering the kinematics data
     ; If we exceed this, we have likely hit the plateau in the
     ; tangential data where overexpansion has ceased.
     if wave_frontedge[i].rad gt rad[maxFrontEdge] then begin
        if debug eq 1 then print, "Greater than wavefront edge, removing point"
        endInd = i + startInd
        endCorr = i
        return
     endif

  endfor
     
     ; Update the endIndex to cutoff once we reach
                                ; the last location of valid data
  minEndArr = where(topDiff eq min(topDiff))
  if minEndArr[0] ne -1 then begin
     minEnd = minEndArr[0]
     
     print, "minEnd is:"
     print, minEnd

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
