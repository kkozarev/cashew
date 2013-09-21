pro aia_img_diagnostics,img
;This procedure calculates pixel metrics and information about the
;image for diagnostics, to test if/what information might be used to
;automatically set parameters for the feature finding algorithm YAFTA.
;Written on August 17, 2011, by KAK


;1. Calculate the histogram of the image:
hist=histogram(img,min=min(img))
maxfreq=max(hist)
mode=where(hist eq maxfreq)+min(img)
plot,hist,yrange=[1.0,1.0e6],psym=10,/ylog

;2. Find the mean, variance, skewness and kurtosis of the image
mom=moment(img)
med=median(img)

print,''
print,'----IMAGE STATISTICS----'
print,''
print,'The Mean of the image is '+strtrim(string(mom[0]),2)
print,''
print,'The Median of the image is '+strtrim(string(med),2)
print,''
print,'The Mode of the image is '+strtrim(string(mode),2)
print,''
print,'The Variance of the image is '+strtrim(string(mom[1]),2)
print,''
print,'The Standard Deviation of the image is '+strtrim(string(sqrt(mom[1])),2)
print,''
print,'The Skewness of the image is '+strtrim(string(mom[2]),2)
print,''
print,'The Kurtosis of the image is '+strtrim(string(mom[3]),2)
print,''

end
