pro find_start_end_radial, rad_data
;
;PURPOSE
;Procedure to automatically find initial estimates of the start end times of the EUV front
;Takes data, sums up pixel intensities at each time step, determines
;start and end times from a Gaussian fit and how the sum of intensities change
;
;INPUTS
;     DATA - annulus data from aia_annulus_analyze_radial.pro
;     TIME - array of times to corresponding annulus data
;     RAD - array of radii used in the data
;OUTPUTS
;     STARTIND - index of front start position
;     ENDIND - index of front end position

  ;To print out additional information set debug to 1
  debug = 0
  
  time=rad_data.time
  nt = n_elements(time)
  yrng=rad_data.radfitrange
  data=rad_data.data[*, yrng[0]:yrng[1]]
  rad=rad_data.y_rsun_array
  ind=where(data lt 0.0)
  if ind[0] gt -1 then data[ind] = 0.0

  ;Go through and sum up the intensities for each time step
  totalPixVals=dblarr(nt)
  for tt=0,nt-1 do begin
     tmp=total(data[tt,*])
     totalPixVals[tt]=tmp
  endfor

  ;Smooth the sum of the pixel intensities for the local minima detection
  totalSmoothVals = smooth(totalPixVals, 6, /edge_truncate)
  tmp = lindgen(n_elements(totalSmoothVals))
  
  ;Compute the local minima and maxima to center the Gaussian fit on first wave
  maxima = get_local_maxima(totalSmoothVals, tmp)
  minind = lclxtrem(totalSmoothVals-smooth(totalSmoothVals, 20, /edge_truncate), 10)

  firstMaxInd = where(maxima.ind eq min(maxima.ind))
  goodMinInd = min(where(minind gt maxima[firstMaxInd].ind))
  
  if goodMinInd eq -1 then begin
     minind[goodMinInd] = n_elements(data)-1
  endif
  
  x = lindgen(n_elements(totalPixVals))
  
  ;If more than one max is found, use the first otherwise there is
  ;only one wave present in the data and filtering is unnecessary
  if n_elements(maxima) gt 1 then begin
     ; Correct for smoothing
     corr = 0

     ; Make sure to not prematurely cut off the Gaussian fit if
     ; we have found the biggest one
     if maxima[firstMaxInd].val eq max(maxima.val) then begin
        gaussData = totalPixVals
     endif else begin
        if minind[goodMinInd] + corr gt n_elements(totalPixVals)-1 then corr=0
        gaussData = totalPixVals[0:minind[goodMinInd]+corr]
        x = x[0:minind[goodMinInd]+corr]
     endelse
  endif else begin
     gaussData = totalPixVals
     x=lindgen(n_elements(totalPixVals))
  endelse
  
  gaussData=totalPixVals
  x=lindgen(n_elements(totalPixVals))
  sigma_factor=3.
  
  if debug gt 0 then begin
     set_plot,'x'
     wdef,0,800,600
     !p.background=255
     !p.color=0
     !p.multi=0
     !p.position=[0.14,0.12,0.95,0.93]
     loadct,39,/silent
     plot,totalpixvals,charsize=2,title='Automatic J-map wave start/end detect',$
          xtitle='Time, px',ytitle='Intensity',/xstyle
  endif
  
  ;Compute a Gaussian fit to determine start and end times
  gfit2 = mpfitpeak(x, gaussData, coeff, nterms=4)
  
  
  ;If the peak or stdev is outrageous, refit with all of the data
  if coeff[2] gt n_elements(totalPixVals)/2 or coeff[0] lt 0 then begin
     x = lindgen(n_elements(totalPixVals))
     gfit2 = mpfitpeak(x, totalPixVals, coeff, nterms=4)
  endif
  
  if debug gt 0 then oplot,gfit2,color=120

  
  
  ;Set the start/end locations
  minusTwoSigma = fix(coeff[1] - sigma_factor*coeff[2])
  plusTwoSigma = fix(coeff[1] + sigma_factor*coeff[2])
  
  ; Refit the Gaussian with all of the
  ; data if the initial start guess is negative
  if minusTwoSigma lt 0 then begin
     x = lindgen(n_elements(totalPixVals))
     gfit2 = mpfitpeak(x, totalPixVals, coeff, nterms=4)

     minusTwoSigma = fix(coeff[1] - sigma_factor*coeff[2])
     plusTwoSigma = fix(coeff[1] + sigma_factor*coeff[2])
  endif    
  
  if minusTwoSigma lt 0 then begin
     minusTwoSigma = 0
     print,''
     print, "No valid start point found, setting index to 0..."
  endif
     
  if plusTwoSigma gt n_elements(totalPixVals)-1 then begin
     plusTwoSigma=n_elements(totalPixVals)-1
     print,''
     print, "No valid end point found, setting index to "+strtrim(string(n_elements(totalPixVals)-1),2)+'...'
  endif
  
  if debug gt 0 then begin
     oplot,[plusTwoSigma, plusTwoSigma], !y.crange,color=60
     oplot,[minusTwoSigma, minusTwoSigma], !y.crange,color=60
  endif
  
;The rest of the procedure makes sure the maxima exceed the background
;noise, and tries to find the start and end time based on the slope of
;the time profile.


;-----------------------------------------------------------------
;FINDING THE START POSITION
; Use MPFitPeak to provide initial starting and ending
; time guess

  startGuess = minusTwoSigma
  endGuess = plusTwoSigma
  
  backgroundLevel = mean(totalPixVals[0:startGuess])
  backgroundThresh = backgroundLevel + 0.50*backgroundLevel
  
                                ; Make sure the first maximum is
                                ; sufficiently above background, or
                                ; fit the entire data duration
  if maxima[firstMaxInd].val lt backgroundThresh then begin
     if debug eq 1 then print, "Below background threshold, recomputing..."
     x = lindgen(n_elements(totalPixVals))
     gfit2 = mpfitpeak(x, totalPixVals, coeff, nterms=4)
     if debug gt 0 then oplot,gfit2,color=120

     minusTwoSigma = coeff[1] - sigma_factor*coeff[2]
     plusTwoSigma = coeff[1] + sigma_factor*coeff[2]

     if debug gt 0 then begin
        oplot,[plusTwoSigma, plusTwoSigma], !y.crange,color=60
        oplot,[minusTwoSigma, minusTwoSigma], !y.crange,color=60      
     endif
     
     startGuess = round(minusTwoSigma)
     endGuess = round(plusTwoSigma)
     
  endif

  if debug eq 1 then begin
     print, "Gaussian based start index guess"
     print, startGuess
     print, "Gaussian based end index guess"
     print, endGuess
  endif
  
  
  backgroundEnd = startGuess
  if backgroundEnd lt 0 then backgroundEnd=0 ;DEBUG
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
  if backgroundEnd+20 ge n_elements(totalPixVals)-1 then endWindow = n_elements(totalPixVals)-2
  if startWindow le 0 then startWindow = 0
  
  
; For a window around the end of the background compute the slope
  for tt=startWindow, endWindow do begin
     slope[tt] = (totalPixVals[tt+1] - totalPixVals[tt])
     if debug eq 1 then begin
        print, "Current step: ", tt
        print, "Current slope: ", slope[tt]
     endif
  endfor
  
; Ideally the front should be marked by a rapid increase in the 
; slope, finding the place where we have a large slope within
; the background window should mark the start of the front.
; Save this as the starting index
  startInd = min(where(slope gt 225.))
  if debug eq 1 then print, "Slope detected start: ", startInd
  
  if startInd eq -1 then begin
     print, "Could not find valid slope based starting point"
     print, "Using Gaussian based start index guess"
     startInd = startGuess
  endif

  ; If the Gaussian fit is incomplete, force a gaussian fit over all data
  if startInd gt n_elements(gfit2)-1 then begin
     x = lindgen(n_elements(totalPixVals))
     gfit2 = mpfitpeak(x, totalPixVals, coeff, nterms=4)
  endif
  

  
;-----------------------------------------------------------------
;FINDING THE END POSITION

; To find the end position, define a threshold
; based on the mean pixel value of the background
  backgroundLevel = mean(totalPixVals[0:backgroundEnd])
  endLevel = backgroundLevel
  if debug eq 1 then print, "Background threshold at: ", endLevel
  
  ; First try and find when the data crosses the background
  endInd = -1
  tmp=max(totalPixVals,maxInd)
  for tt = maxInd, nt-1 do begin
     if totalPixVals[tt] lt endLevel then begin
        endTime = time[tt]
        endInd = tt
        if debug eq 1 then print, "End Index: ", endInd
        break
     endif
  endfor
  
  
  ; If unsuccesful, find where the Gaussian crosses the background
  if endInd eq -1 then begin
     for tt=startInd, nt-1 do begin
                                ;print, totalPixVals[tt]
                                ; Save the first instance of falling below the
                                ; threshold as the end index
        if tt eq n_elements(totalPixVals) then break
        if tt eq n_elements(gfit2) then break
        
        if gfit2[tt] lt endLevel then begin
           endTime = time[tt]
           endInd = tt
           if debug eq 1 then print, "End Index: ", endInd
           break
        endif
     endfor
  endif
  
  
  ; If nothing works use the Gaussian based two sigma guess
  if endInd eq -1 then begin
     print, "Could not find valid ending point, using Gaussian based guess"
     endInd = endGuess
     if endInd gt n_elements(totalPixVals)-1 then endInd = n_elements(totalPixVals)-2
     rad_data.timefitrange=[startInd, endInd]
     return 
  endif

  if debug gt 0 then begin
     oplot,[startInd,startInd], !y.crange,color=160
     oplot,[endInd,endInd], !y.crange,color=160      
  endif
  
  ;Exit if a good start position is not found
  if startInd eq -1 then begin
     print,'Wave start time is unreasonable. Rerun manually. Quitting...'
     return
  endif
  if endInd eq -1 then begin
     print,'Wave end time is unreasonable. Rerun manually. Quitting...'
     return
  endif
  if startInd eq endInd then begin
     print,'Wave start time is the same as wave end time. Rerun manually. Quitting...'
     return
  endif
  ;print,'Press any key to continue.'
                                ;test = get_kbrd(1)
                                ;wait,1
  if debug gt 0 then begin
     wdel,0
     loadct,0,/silent
  endif
  
  ;The final result is written into rad_data
  rad_data.timefitrange=[startInd, endInd]
  
end
