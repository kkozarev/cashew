pro find_start_end, data, time, startInd=startInd, endInd=endInd, mymaxima=mymaxima, wave_frontedge=wave_frontedge

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

  x = lindgen(n_elements(totalPixVals))
  gfit1 = gaussfit(x, totalPixVals, coeff, nterms=3)
  gfit2 = gaussfit(x, totalPixVals, coeff, nterms=4)
  gfit3 = gaussfit(x, totalPixVals, coeff, nterms=5)
  gfit4 = gaussfit(x, totalPixVals, coeff, nterms=6)

  cgPlot, gfit1, /overPlot, color='blue', /window
  cgPlot, gfit2, /overPlot, color='green', /window
  cgPlot, gfit3, /overPlot, color='red', /window
  cgPlot, gfit4, /overPlot, color='cyan', /window

  if keyword_set(mymaxima) then begin
     for i=startInd, endInd do begin
        topDiff = wave_frontedge[i-sp].rad
        


  prevVal = totalPixVals[0]
  maxDuration = 0

; Better plan -> running average of data
  currentMean = totalPixVals[0]
  backgroundEnd = -1
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

  if backgroundEnd eq -1 then begin
     startInd = -1
     endInd = -1
     return
  end

  endWindow = backgroundEnd+15
  if backgroundEnd+15 gt n_elements(totalPixVals)-1 then endWindow = n_elements(totalPixVals)-1
  
  for tt=backgroundEnd-8, endWindow do begin
     slope[tt] = (totalPixVals[tt] - totalPixVals[tt-1])
  endfor
  
  quit = 0
  startInd = min(where(slope gt 250))
  if startInd eq -1 then begin
     print, "Could not find valid starting point, exiting..."
     return
  endif

  startTime = time[startInd]

  backgroundLevel = mean(totalPixVals[0:startInd])
  
  threshold = 0.10
  startLevel = backgroundLevel + threshold*backgroundLevel
  print, startLevel
  print, backgroundEnd

  endInd = -1
  
  for tt=startInd, nt-1 do begin
     print, totalPixVals[tt]
     if totalPixVals[tt] lt startLevel then begin
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
     
;     print, "Start Time: ", startTime
;     print, "End Time: ", endTim
  
end
