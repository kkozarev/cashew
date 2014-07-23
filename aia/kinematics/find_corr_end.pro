pro find_corr_end, data, time, rad, startInd=startInd, endInd=endInd, wave_frontedge=wave_frontedge,$
                    maxRadIndex=maxRadIndex, startCorr=startCorr, endCorr=endCorr



  ; To print out additional information set debug to 1
  debug = 1

  nt = n_elements(time)
  dat=data
  
  ; Set the initial start correction to zero
  startCorr = 0

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
     endInd = minEnd + startInd + 1
     endCorr = minEnd+1
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
