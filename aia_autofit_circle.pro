pro aia_autofit_circle,xarr,yarr,ellpos,p,error=error
;This procedure uses the mpfitellipse procedure to make a circle fit to 
;a bunch of points supplied to the program. It is based on aiamancirclefit.pro.
;Kamen Kozarev, 09/06/2011

;INPUT: None

;KEYWORDS
;

;OUTPUT
; circlepos - a two by n array containing the x and y positions of the
;          ellipse fit. 'n' is the number of points chosen in the fit,
;          default is 501
; p - ellipse parameters from the fit.

if n_elements(xarr) ne n_elements(yarr) then begin
   print,'Number of supplied x- and y- coordinates supplied differs!'
   return
endif
   
np=n_elements(xarr)

p=mpfitellipse(xarr,yarr,/circular,/quiet)
;      P[0]   Circle radius
;      P[1]   Circle radius  
;      P[2]   Circle center - x value
;      P[3]   Circle center - y value

;print,p

phi = dindgen(501)*2D*!dpi/500

xm = p[2] + p[0]*cos(phi)*cos(p[4]) + p[1]*sin(phi)*sin(p[4])
ym = p[3] - p[0]*cos(phi)*sin(p[4]) + p[1]*sin(phi)*cos(p[4])

;plot, xarr, yarr, psym=4,/device
;plots, floor(xm), floor(ym),/device,thick=3
ellpos=fltarr(2,501)
ellpos[0,*]=xm
ellpos[1,*]=ym

end
