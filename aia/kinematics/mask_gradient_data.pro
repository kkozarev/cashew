pro mask_gradient_data, time, yarray, data, yrng, intensityData,$
    correctData = correctData

; First compute a quiet background average using data from the upper
; left corner

;background = mean(intensityData[0:5, 100:n_elements(yarray)-1])
background = mean(intensityData)
print, background

;threshold = background

threshold = 25.0

newData = data
nExceed = 0 

for i=0, n_elements(intensityData[*,0])-1 do begin
;   print, "Last column exceeded number is: "
;   print, nExceed 
;   nExceed = 0
   for j=0, n_elements(intensityData[i,*])-1 do begin
      if intensityData[i, j] gt threshold then begin
         nExceed++
;         squareDiff = sqrt((intensityData[i, j])^2 - (threshold)^2)
;         print, squareDiff
;        correctData[i, j] = 1.0/squareDiff
         newData[i, j] = 1.0
      endif
   endfor
endfor
       


   ;; goodIndices = where(intensityData[i,*] gt threshold)
   ;; print, i
   ;; print, n_elements(goodIndices)
   ;; if goodIndices[0] gt 0 then begin
   ;;    correctData[i, goodIndices] = 1.0 
   ;; endif

correctData = data / newData


end


