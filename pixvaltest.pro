pro pixvaltest,image

imsiz=size(image)
if imsiz[2] eq 0 then begin
   print,"You need to supply an image, like 'IDL> pixvaltest,image'!"
   return
   endif
image=reform(image)

wdef,0,1024
loadct,3,/silent
;tvscl,sqrt(image)
tv,bytscl(image,0,40)

featavg=0.0
backavg=0.0
print, ''
print, 'Select 20 points along the top of the feature'
for i=0,19 do begin
print, ''
print, 'Point #'+strtrim(string(i+1),2)
wait,0.4
cursor,x,y,/device
plots,x,y,psym=7,symsize=1.5,/device
print, 'x: '+strtrim(string(x),2) + '   y: '+ strtrim(string(y),2) + '    I[x,y] = '+ strtrim(string(image[x,y]),2)
featavg=featavg+image[x,y]*1.0
endfor
featavg=featavg/20.0


print, ''
print, 'Select 20 points just upstream of the top of the feature'
for i=0,19 do begin
print, ''
print, 'Point #'+strtrim(string(i+1),2)
wait,0.4
cursor,x,y,/device
plots,x,y,psym=7,symsize=1.5,/device
print, 'x: '+strtrim(string(x),2) + '   y: '+ strtrim(string(y),2) + '    I[x,y] = '+ strtrim(string(image[x,y]),2)
backavg=backavg+image[x,y]*1.0
endfor
backavg=backavg/20.0

print,''
print,'Average value of feature top for this image: '+strtrim(string(featavg),2)
print,'Average value of background  for this image: '+strtrim(string(backavg),2)
print,'Percentage change from background to feature: '+strtrim(string((featavg-backavg)/backavg*100),2)+ ' %'
end
