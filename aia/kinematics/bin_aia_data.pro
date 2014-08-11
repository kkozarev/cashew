pro bin_aia_data, time, yarray, data, yrng, binData=binData


binData = data

; Compute thresholds from background average
backgroundMean = mean(data[0:5, 100:n_elements(data[0,*])-1])
backgroundStdev = stddev(data[0:5, 100:n_elements(data[0,*])-1])

print, "Background average is: "
print, backgroundMean

print, "Background stdev is"
print, backgroundStdev


;; for t=0, n_elements(data[*,0])-1 do begin
;;    for r=0, n_elements(data[0,*])-1 do begin
;;       bindata[t,r] = 0.0
;;       if data[t, r] lt 0.0 then begin
;;          binData[t, r] = -10.0
;;       endif else if data[t, r] gt 50.0 && data[t,r] lt 75.0 then begin
;;          binData[t,r] = 25.0
;;       endif else if data[t,r] gt 75.0 then begin
;;          binData[t,r] = 150.0
;;       endif
;;    endfor
;; endfor 

firstBin = backgroundMean +3*backgroundStdev
secondBin = backgroundMean + 4*backgroundStdev
thirdBin = backgroundMean + 6*backgroundStdev
fourthBin = backgroundMean + 8*backgroundStdev


for t=0, n_elements(data[*,0])-1 do begin
   for r=0, n_elements(data[0,*])-1 do begin
      bindata[t,r] = 0.0
      if data[t, r] lt backgroundMean then begin
         binData[t, r] = -1.0
      endif else if data[t, r] gt backgroundMean && data[t,r] lt firstBin then begin
         binData[t,r] = 1.0
      endif else if data[t,r] gt firstBin && data[t,r] lt secondBin then begin
         binData[t,r] = 5.0
      endif else if data[t,r] gt secondBin && data[t,r] lt thirdBin then begin
         binData[t,r] = 15.0
      endif else if data[t,r] gt thirdBin && data[t,r] lt fourthBin then begin
         binData[t,r] = 35.0
      endif else begin
         binData[t,r] = 70.0
      endelse 
   endfor
endfor 

cgImage, binData, /window


stop
end
