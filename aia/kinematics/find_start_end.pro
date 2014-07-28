pro find_start_end, data, time, rad, startInd=startInd, endInd=endInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge,$
                    maxRadIndex=maxRadIndex, startCorr=startCorr, endCorr=endCorr

;PURPOSE
;Procedure to automatically find initial estimates of the start end times of the EUV front
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

  ; To print out additional information set debug to 1
  debug = 1

  nt = n_elements(time)
  dat=data
  
  ; Set the initial start correction to zero
  startCorr = 0

  ind=where(dat lt 0.0)
  if ind[0] gt -1 then dat[ind] = 0.0

  ; Go through and sum up the intensities for each time step
  totalPixVals=dblarr(nt)

  for tt=0,nt-1 do begin
     tmp=total(dat[tt,*])
     totalPixVals[tt]=tmp
  endfor

  
  totalSmoothVals = smooth(totalPixVals, 6, /edge_truncate)

     
  ;cgplot, totalPixVals, /window


  cgplot, totalSmoothVals, /window

  slope = dblarr(nt)
  concavity = dblarr(nt)

; Compute Slope
  for i = 1, n_elements(totalSmoothVals)-1 do begin
     slope[i] = (totalSmoothVals[i] - totalSmoothVals[i-1])
  endfor
  
 endPoint = 0
  done = 0

  while (done eq 0) do begin
;; ;  cgplot, slope, color='blue', /overplot, /window
     maxPosDuration = 0
     for i = endPoint, n_elements(slope)-1 do begin
        print, i
        if slope[i] gt 0 then begin
           maxPosDuration++
        endif else begin
           print, "Breaking"
           endPoint = i+1
           break
        endelse
     endfor
     
     if maxPosDuration gt 6 then done = 1
  endwhile
        
; Find maxima to give GaussFit a good guess
  currentMax = 0
  currentMaxInd = 0

  maxStr = {val:0.0, ind:0}
  maxStruct = replicate(maxStr, n_elements(totalPixVals)-1)

  for i = 0, n_elements(totalPixVals)-1 do begin
     if totalPixVals[i] gt currentMax then begin
        maxStruct[i].val = totalPixVals[i]
        maxStruct[i].ind = i
        currentMax = totalPixVals[i]
        currentMaxInd = i
     endif 
  endfor

stop


;estimates = [9000,30,10]


  ; Plot a variety of Gaussian fits to
  ; see if this would be useful for
  ; start/end detection
  x = lindgen(n_elements(totalPixVals))
;  gfit1 = gaussfit(x, totalPixVals, coeff, nterms=3)
  gfit2 = gaussfit(x[0:18], totalPixVals[0:18], coeff, estimates=estimates, nterms=3)
;  gfit3 = gaussfit(x, totalPixVals, coeff, nterms=5)
;  gfit4 = gaussfit(x, totalPixVals, coeff, nterms=6)

;  cgPlot, gfit1, /overPlot, color='blue', /window
  cgPlot, gfit2, /overPlot, color='green', /window
;  cgPlot, gfit3, /overPlot, color='red', /window
;  cgPlot, gfit4, /overPlot, color='cyan', /window

  minusTwoSigma = coeff[1] - 2*coeff[2]
  plusTwoSigma = coeff[1] + 2*coeff[2]
  plusFourSigma = coeff[1] + 4*coeff[2]
  
  cgPlot, [plusTwoSigma, plusTwoSigma], [0, 800], /Overplot, /window
  cgPlot, [minusTwoSigma, minusTwoSigma], [0, 800], /Overplot, /window  
  cgPlot, [plusFourSigma, plusFourSigma], [0, 800], /OverPlot, /window

  prevVal = totalPixVals[0]
  maxDuration = 0

;; ; Primary scan - look for data which exceeds a running 
;; ; mean of totalPixVals
;;   currentMean = totalPixVals[0]
;;   backgroundEnd = -1
;;   for tt=0, nt-1 do begin
;;      currentMean = mean(totalPixVals[0:tt])
;;      if debug eq 1 then begin
;;         print, "Current running mean is: ", currentMean
;;         print, "Current total pixel value is: ", totalPixVals[tt]
;;      endif 
;;      if totalPixVals[tt] gt currentMean then begin
;;         maxDuration++
;;         if debug eq 1 then print, "Number of times running average exceeded: ", maxDuration
;;      endif else begin
;;         maxDuration = 0 
;;      endelse
;; ; If we have exceeded the mean for 6 timesteps save
;; ; this as the end of the quiet background of totalPixVals
;;      if maxDuration gt 6 then begin
;;         backgroundEnd = tt
;;         print, "Location of background end: ", backgroundEnd
;;         break
;;      endif
;;   endfor

; Improved Primary scan, using GaussFit to provide initial starting
; time guess

  gaussStartInd = round(minusTwoSigma)
  startGuess = gaussStartInd
  
  gaussEndInd = round(plusTwoSigma)
  endGuess = gaussEndInd

  
  if debug eq 1 then begin
     print, "Two sigma Gauss Index"
     print, gaussStartInd
     print, "Gaussian based start index guess"
     print, startGuess
  endif
  
  backgroundEnd = startGuess
  
  slope = dblarr(nt)
  julianTime = time.jd
  
; Make sure valid data was actually found
  if backgroundEnd eq -1 then begin
     startInd = -1
     endInd = -1
     return
  end

; Select an end window location for slope computation
  endWindow = backgroundEnd+20
  startWindow = backgroundEnd
  if backgroundEnd+20 gt n_elements(totalPixVals)-1 then endWindow = n_elements(totalPixVals)-1
  if startWindow lt 0 then startWindow = 0
  
; For a window around the end of the background compute the slope
  for tt=startWindow, endWindow do begin
     slope[tt] = (totalPixVals[tt] - totalPixVals[tt-1])
     if debug eq 1 then begin
        print, "Current step: ", tt
        print, "Current slope: ", slope[tt]
     endif
  endfor
  
; Ideally the front should be marked by a rapid increase in the 
; slope, finding the place where we have a large slope within
; the background window should mark the start of the front.
; Save this as the starting index
  startInd = min(where(slope gt 225))

  if debug eq 1 then print, "Slope detected start: ", startInd

  if startInd eq -1 then begin
     print, "Could not find valid slope based starting point"
     print, "Using Gaussian based start index guess"
     startInd = startGuess
  endif
  
  endInd = endGuess

;; ; To find the end position, define a threshold
;; ; based on the mean pixel value of the background
;;   backgroundLevel = mean(totalPixVals[0:startInd])
  
;; ; Look for when we are within 10% of this threshold
;;   threshold = 0.10
;;   endLevel = backgroundLevel + threshold*backgroundLevel

;;   endInd = -1  
;;   for tt=startInd, nt-1 do begin
;;      ;print, totalPixVals[tt]
;;      ; Save the first instance of falling below the
;;      ; threshold as the end index
;;      if totalPixVals[tt] lt endLevel then begin
;;         endTime = time[tt]
;;         endInd = tt
;;         if debug eq 1 then print, "End Index: ", endInd
;;         break
;;      endif
;;   endfor

;;   if endInd eq -1 then begin
;;      print, "Could not find valid ending point, exiting..."
;;      return
;;   endif

; Better Gaussian based end time detection





  print, "Start Index: ", startInd
  print, "Start Time: ", time[startInd]
  print, "End Index: ", endInd
  print, "End Time: ", time[endInd]
  
stop       


end
