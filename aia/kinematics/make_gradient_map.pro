pro make_gradient_map, times, rad, data, yrng, intensityData=intensityData
  
  intensityData = dblarr(n_elements(times), n_elements(rad))

 ; ind = where(data lt 0)
;  data[ind] =  
  
  for t=0, n_elements(times)-1 do begin
     for r=yrng[0], yrng[1] do begin
        currentPixel = data[t, r]
        skip = 0
        ; Take surrounding pixels and create an intensity score
        
        if t eq 0 then begin
           if r eq 0 then begin
              upDiff = abs(data[t,r]-data[t,r+1])
              rightDiff = abs(data[t,r]-data[t+1,r])
              meanDiff = mean([upDiff, rightDiff])
              intensityData[t,r] = meanDiff
           endif else if r eq n_elements(rad)-1 then begin
              downDiff = abs(data[t,r]-data[t,r-1])
              rightDiff = abs(data[t,r]-data[t+1,r])
              meanDiff = mean([downDiff, rightDiff])
              intensityData[t,r] = meanDiff
           endif else begin
              downDiff = abs(data[t,r]-data[t,r-1])
              rightDiff = abs(data[t,r]-data[t+1,r])
              upDiff = abs(data[t,r]-data[t,r+1])
              meanDiff = mean([downDiff, rightDiff, upDiff])
              intensityData[t,r] = meanDiff
           endelse
        endif else if r eq 0 then begin
           if t eq n_elements(times)-1 then begin
              leftDiff = abs(data[t,r]-data[t-1,r])
              upDiff = abs(data[t,r]-data[t,r+1])
              meanDiff = mean([leftDiff, upDiff])
              intensityData[t,r] = meanDiff
           endif else begin
              leftDiff = abs(data[t,r]-data[t-1,r])
              rightDiff = abs(data[t,r]-data[t+1,r])
              upDiff = abs(data[t,r]-data[t,r+1])
              meanDiff = mean([leftDiff, upDiff, rightDiff])
              intensityData[t,r] = meanDiff
           endelse
        endif else if t eq n_elements(times)-1 then begin
           if r eq n_elements(rad)-1 then begin
              leftDiff = abs(data[t,r]-data[t-1,r])
              downDiff = abs(data[t,r]-data[t,r-1])
              meanDiff = mean([leftDiff, downDiff])
              intensityData[t,r] = meanDiff
           endif else begin
              downDiff = abs(data[t,r]-data[t,r-1])
              leftDiff = abs(data[t,r]-data[t-1,r])
              upDiff = abs(data[t,r]-data[t,r+1])
              meanDiff = mean([downDiff, leftDiff, upDiff])
              intensityData[t,r] = meanDiff
           endelse
        endif else if r eq n_elements(rad)-1 then begin
           leftDiff = abs(data[t,r]-data[t-1,r])
           rightDiff = abs(data[t,r]-data[t+1,r])
           downDiff = abs(data[t,r]-data[t,r-1])
           meanDiff = mean([leftDiff, downDiff, rightDiff])
           intensityData[t,r] = meanDiff
        endif else begin
           leftDiff = abs(data[t,r]-data[t-1,r])
           rightDiff = abs(data[t,r]-data[t+1,r])
           downDiff = abs(data[t,r]-data[t,r+1])
           upDiff = abs(data[t,r]-data[t,r+1])
           meanDiff = mean([leftDiff, downDiff, rightDiff, upDiff])
           intensityData[t,r] = meanDiff
        endelse
        
        if data[t,r] gt 0 && meanDiff lt 20 then begin
           intensityData[t,r] = data[t,r]
           ;print, "time is:"
           ;print, times[t]
           print, "Diff is:"
           print, meanDiff
        endif

     endfor
  endfor


  intensityData = intensityData
  cgimage, intensityData[*, yrng[0]:yrng[1]]
end
         
