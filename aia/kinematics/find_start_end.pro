pro find_start_end, data, time, rad, startInd=startInd, endInd=endInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge, maxRadIndex=maxRadIndex

;PURPOSE
;Procedure to automatically find start end times of the EUV front
;Takes data, sums up pixel intensities at each time step, determines
;start and end times from how the sum of intensities change
;
;INPUTS
;     DATA - annulus data from aia_annulus_analyze_radial.pro
;     TIME - array of times to corresponding annulus data
;
;OUTPUTS
;     STARTIND - index of front start position
;     ENDIND - index of front end position

  nt = n_elements(time)

  dat=data

  ind=where(dat lt 0.0)
  if ind[0] gt -1 then dat[ind] = 0.0

  ; Go through and sum up the intensities for each time step
  totalPixVals=dblarr(nt)

  for tt=0,nt-1 do begin
     tmp=total(dat[tt,*])
     totalPixVals[tt]=tmp
  endfor
     
  cgplot, totalPixVals, /window


  ; Plot a variety of Gaussian fits to
  ; see if this would be useful for
  ; start/end detection
  ;; x = lindgen(n_elements(totalPixVals))
  ;; gfit1 = gaussfit(x, totalPixVals, coeff, nterms=3)
  ;; gfit2 = gaussfit(x, totalPixVals, coeff, nterms=4)
  ;; gfit3 = gaussfit(x, totalPixVals, coeff, nterms=5)
  ;; gfit4 = gaussfit(x, totalPixVals, coeff, nterms=6)

  ;; cgPlot, gfit1, /overPlot, color='blue', /window
  ;; cgPlot, gfit2, /overPlot, color='green', /window
  ;; cgPlot, gfit3, /overPlot, color='red', /window
  ;; cgPlot, gfit4, /overPlot, color='cyan', /window

; Peform a secondary scan and bring up the end index depending
; on where the distance from the current frontedge to the 
; edge of valid radian data
  if keyword_set(mymaxima) then begin
     ;; print, maxRadIndex
     ;; print, rad[maxRadIndex]
     
     topDiff = fltarr(n_elements(wave_frontedge))
     for i=0, n_elements(wave_frontedge)-1 do begin
        topDiff[i] = abs(wave_frontedge[i].rad - rad[maxRadIndex])
     endfor

     minIndArr = where(topDiff eq min(topDiff))
     minInd = minIndArr[0]
     endInd = minInd + startInd
     return
  endif

        


  prevVal = totalPixVals[0]
  maxDuration = 0

; Primary scan - look for data which exceeds a running 
; mean of totalPixVals
  currentMean = totalPixVals[0]
  backgroundEnd = -1
  for tt=0, nt-1 do begin
     currentMean = mean(totalPixVals[0:tt])
;        print, currentMean
 ;       print, totalPixVals[tt]
     if totalPixVals[tt] gt currentMean then begin
        maxDuration++
      ;  print, maxDuration
     endif else begin
        maxDuration = 0 
     endelse
; If we have exceeded the mean for 8 timesteps save
; this as the end of the quiet background of totalPixVals
     if maxDuration gt 8 then begin
        backgroundEnd = tt
        break
     endif
  endfor

  slope = dblarr(nt)
  julianTime = time.jd

; Make sure valid data was actually found
  if backgroundEnd eq -1 then begin
     startInd = -1
     endInd = -1
     return
  end

; Select an end window location for slope computation
  endWindow = backgroundEnd+15
  if backgroundEnd+15 gt n_elements(totalPixVals)-1 then endWindow = n_elements(totalPixVals)-1
  
; For a window around the end of the background compute the slope
  for tt=backgroundEnd-8, endWindow do begin
     slope[tt] = (totalPixVals[tt] - totalPixVals[tt-1])
  endfor
  
; Ideally the front should be marked by a rapid increase in the 
; slope, finding the place where we have a large slope within
; the background window should mark the start of the front.
; Save this as the starting index
  startInd = min(where(slope gt 250))

  if startInd eq -1 then begin
     print, "Could not find valid starting point, exiting..."
     return
  endif
  
; To find the end position, define a threshold
; based on the mean pixel value of the background
  backgroundLevel = mean(totalPixVals[0:startInd])
  
; Look for when we are within 10% of this threshold
  threshold = 0.10
  endLevel = backgroundLevel + threshold*backgroundLevel

  endInd = -1  
  for tt=startInd, nt-1 do begin
     ;print, totalPixVals[tt]
     ; Save the first instance of falling below the
     ; threshold as the end index
     if totalPixVals[tt] lt endLevel then begin
        endTime = time[tt]
        endInd = tt
        break
     endif
  endfor

  if endInd eq -1 then begin
     print, "Could not find valid ending point, exiting..."
     return
  endif

  print, "Start Index: ", startInd
  print, "End Index: ", endInd
       
end
