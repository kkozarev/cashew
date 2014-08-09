pro make_gradient_map_tangential, times, rad, data, yrng, intensityData=intensityData
  
  ; Just take gradient in the x direction for tangential plots

  intensityData = dblarr(n_elements(times), n_elements(rad))

 ; ind = where(data lt 0)
;  data[ind] =  

  ; Select a number of points for a window to average over
  nPts = 10
  window = indgen(nPts)
  start = nPts

  for t=1, n_elements(data[*,0])-1 do begin
     for r=0, n_elements(data[0,*])-1  do begin
        meanDiff = 0
        currentPixel = data[t, r]
        skip = 0
        ; Take surrounding pixels and create an intensity score
        
        if t eq 0 then begin
           for i = 0, n_elements(window)-1 do begin
              rightDiff = abs(data[t+i,r]-data[t+i+1,r])
              print, rightDiff
              meanDiff = meanDiff+rightDiff
           endfor
           meanDiff = meanDiff / n_elements(window)
           intensityData[t,r] = meanDiff
        endif else if t eq n_elements(times)-1 then begin      
           for i = 0, n_elements(window)-1 do begin
              leftDiff = abs(data[t,r]-data[t-i-1, r])
              meanDiff = meanDiff+leftDiff
           endfor
           meanDiff = meanDiff / n_elements(window)
           intensityData[t,r] = meanDiff
        endif else begin
           nIterations = 0
           meanDiff = 0
           for i=0, n_elements(window)-1 do begin
              if t - i le 1 then break
              nIterations++
              leftDiff = abs(data[t,r]-data[t-i-1,r])
              meanDiff = meanDiff + leftDiff
           endfor
           ;; for i = 0, n_elements(window)-1 do begin
           ;;    if t+i gt n_elements(times)-2 then break
           ;;    nIterations++
           ;;    rightDiff = abs(data[t+i,r]-data[t+i+1,r])
           ;;    meanDiff = meanDiff + rightDiff
           ;; endfor
           meanDiff = meanDiff / nIterations
           intensityData[t,r] = meanDiff
        endelse

        
        ;; if data[t,r] gt 0 && meanDiff lt 20 then begin
        ;;    intensityData[t,r] = data[t,r]
        ;;    ;print, "time is:"
        ;;    ;print, times[t]
        ;;    print, "Diff is:"
        ;;    print, meanDiff
        ;; endif

     endfor
  endfor

  intensityData = intensityData
  cgimage, intensityData[*, yrng[0]:yrng[1]]
 
  
end
         
