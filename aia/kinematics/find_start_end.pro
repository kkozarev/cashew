pro find_start_end, data, time, yrng, startInd=startInd, endInd=endInd, quit=quit

;PURPOSE
;Procedure to automatically find start end times of the EUV front
;Takes data, sums up pixel intensities at each time step, determines
;start and end times from how the sum of intensities change
;
;INPUTS
;     DATA - annulus data from aia_annulus_analyze_radial.pro
;     TIME - array of times to corresponding annulus data
;     YRNG - radial distance ranges from
;            aia_annulus_analyze_radial.pro
;
;OUTPUTS
;     STARTIND - index of front start position
;     ENDIND - index of front end position

  nt = n_elements(time)
  
  ; Go through and sum up the intensities for each time step
  totalPixVals=dblarr(nt)
  for tt=0,nt-1 do begin
     dat=data[tt,yrng[0]:yrng[1]]
     ind=where(dat gt 0.0)
     if ind[0] gt -1 then tmp=total(dat[ind]) else tmp=0
     totalPixVals[tt]=tmp
  endfor
     
     
  cgplot, totalPixVals, /window
  
  prevVal = totalPixVals[0]
  maxDuration = 0
     ;; for tt=0, nt-1 do begin
     ;;    if totalPixVals[tt] gt prevVal then begin
     ;;       prevVal = totalPixVals[tt]
     ;;       maxDuration++
     ;;       print, maxDuration
     ;;    endif else begin
     ;;       prevVal = totalPixVals[tt]
     ;;       maxDuration = 0
     ;;    endelse

     ;;    if maxDuration gt 6 then begin
     ;;       backgroundEnd = tt - 5
     ;;       break
     ;;    endif
     ;; endfor

; Better plan -> running average of data
  currentMean = totalPixVals[0]
  for tt=0, nt-1 do begin
     currentMean = mean(totalPixVals[0:tt])
;        print, currentMean
 ;       print, totalPixVals[tt]
     if totalPixVals[tt] gt currentMean then begin
        maxDuration++
        print, maxDuration
     endif else begin
        maxDuration = 0 
     endelse
     if maxDuration gt 8 then begin
        backgroundEnd = tt
        break
     endif
  endfor

  slope = dblarr(nt)
  julianTime = time.jd
  
  for tt=backgroundEnd-8, backgroundEnd+15 do begin
     slope[tt] = (totalPixVals[tt] - totalPixVals[tt-1])
  endfor
  
  quit = 0
  startInd = min(where(slope gt 250))
  if startInd eq -1 then begin
     print, "Could not find valid starting point, exiting..."
     quit = 1
     return
  endif

  startTime = time[startInd]

  backgroundLevel = mean(totalPixVals[0:startInd])
  
  threshold = 0.10
  startLevel = backgroundLevel + threshold*backgroundLevel
  print, startLevel
  print, backgroundEnd
  
  for tt=startInd, nt-1 do begin
     print, totalPixVals[tt]
     if totalPixVals[tt] lt startLevel then begin
        endTime = time[tt]
           endInd = tt
           break
        endif
  endfor

  print, "Start Index: ", startInd
  print, "End Index: ", endInd
     
;     print, "Start Time: ", startTime
;     print, "End Time: ", endTime
  

end
