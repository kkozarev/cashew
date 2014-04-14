pro aia_manual_circlefit,circlepos,p,np=np,error=error,mousebutton=mousebutton,$
                    special_point=special_point
;PURPOSE:
;This procedure uses the mpfitellipse procedure to make a circle fit to 
;a bunch of points selected by the user.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;
;KEYWORDS:
; np - number of points the user must select manually for the fittng.
;      Default is 10.
;
;OUTPUTS:
; circlepos - a two by n array containing the x and y positions of the
;          ellipse fit. 'n' is the number of points chosen in the fit,
;          default is 501
; p - ellipse parameters from the fit.
; 
;DEPENDENCIES:
; mpfitellipse
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 10/2010
;

if not keyword_set(np) then np=10
xarr=fltarr(np)
yarr=fltarr(np)

print,'User must select '+strtrim(string(np),2)+' points to fit a circle.'
print,''


for i=0,np-1 do begin
print, 'Select position #'+strtrim(string(i),2)
cursor,x,y,/device
xarr[i]=x
yarr[i]=y
if i eq 0 and keyword_set(special_point) then special_point=[x,y]
mousebutton=!mouse.button
;if !mouse.button eq 4 then begin
;   print,'Right mouse button was pressed - operation is canceled.'
;   p=fltarr(4)
;   circlepos=fltarr(2,501)
;   return
;endif
wait,0.5
plots,x,y,psym=5,symsize=1,/device
endfor

p=mpfitellipse(xarr,yarr,/circular)
;      P[0]   Circle radius
;      P[1]   Circle radius  
;      P[2]   Circle center - x value
;      P[3]   Circle center - y value

print,p

phi = dindgen(501)*2D*!dpi/500

xm = p[2] + p[0]*cos(phi)*cos(p[4]) + p[1]*sin(phi)*sin(p[4])
ym = p[3] - p[0]*cos(phi)*sin(p[4]) + p[1]*sin(phi)*cos(p[4])

;plot, xarr, yarr, psym=4,/device
plots, floor(xm), floor(ym),/device,thick=3
circlepos=fltarr(2,501)
circlepos[0,*]=xm
circlepos[1,*]=ym

end
