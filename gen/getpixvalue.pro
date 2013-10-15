pro getpixvalue,image
;PURPOSE:
; Select and obtain the pizel value
;CATEGORY:
; General
;
;INPUTS:
; image
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;

image=reform(image)

wdef,0,1024
loadct,3,/silent
;tvscl,sqrt(image)
tv,image

more=1
while more eq 1 do begin
print,''
print,'Choose a point:'
wait,0.4
cursor,x,y,/device
plots,x,y,psym=7,symsize=1.5,/device
print, 'x: '+strtrim(string(x),2) + '   y: '+ strtrim(string(y),2) + '    I[x,y] = '+ strtrim(string(image[x,y]),2)

answer=''
read,'More? (Y/N)',answer
if answer eq 'N' or answer eq 'n' then more=0
endwhile

end
