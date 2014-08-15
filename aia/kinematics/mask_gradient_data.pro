pro mask_gradient_data, time, yarray, data, yrng, intensityData,$
    correctData = correctData

;PURPOSE
;Procedure to select pixels which exhibit a large gradient and filter
;out the rest by dividing two images. In one image, pixels with a
;large gradient are assigned a value of one, all other pixels are
;untouched. Then the original image is divided by the modified image
;such that pixels that have a high gradient retain their original
;value and all other pixels are now set to one. This should allow us
;to fit only the data with a high gradient
;
;INPUTS
;     DATA - annulus data from aia_annulus_analyze_radial.pro
;     TIME - array of times to corresponding annulus data
;     YARRAY - array of y-axis data
;     YRNG - array of valid proper start/stop indices in yarray
;     INTENSITYDATA - array of gradient data produced previously
;OUTPUTS
;     CORRECTDATA - corrected image which has set pixels with low
;                   intensities to 1.0





; First compute a quiet background average using data from the upper
; left corner

;background = mean(intensityData[0:5, 100:n_elements(yarray)-1])
background = mean(intensityData)
print, background

;threshold = background

;This threshold was chosen based on the current bin settings
;in bin_aia_data.pro, we want to select regions with high gradients 
;so based on bin_aia_data we want the threshold to be above gradients
;between smaller valued bins
threshold = 30.0

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

; Divide the original image by the modified one
correctData = data / newData

end


