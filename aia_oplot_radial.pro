pro aia_oplot_radial,subindex,s_point,location
;PURPOSE:
;This program overplots a radial line passing through a given point on
;the solar surface.
;
;CATEGORY:
;
;
;INPUTS:
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
;Written by Kamen Kozarev, 01/2012
;         Michael Hammer, 07/30/2013 - make it work for both east and west limbs.

index=reform(subindex[0])
point=s_point
!P.thick=3

;sun center is at
cenx=2047.5-index.SUBROI_X0
ceny=2047.5-index.SUBROI_Y0

flarepos = point
profrad=2000
plots,flarepos[0],flarepos[1],psym=2,symsize=2,/device,color=255

xp=[cenx,flarepos[0]]
yp=[ceny,flarepos[1]]

inan=atan(float(yp[1]-yp[0])/float(xp[1]-xp[0]))*180/!PI

print,xp
print,yp
print,inan

multiplier = 1
if location eq 'E' then multiplier = -1

xp[1] = xp[0] + round((profrad+index.R_SUN)*cos(inan*!PI/180.)) * multiplier
yp[1] = yp[0] + round((profrad+index.R_SUN)*sin(inan*!PI/180.)) * multiplier

print,xp
print,yp

plots,xp,yp,/device,color=255

end
