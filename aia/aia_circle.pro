function aia_circle, xcenter, ycenter, rad, plot=plot,color=color
;PURPOSE:
; calculate the coordinates of a circle
;
;CATEGORY:
; AIA/General
;
;INPUTS:
; xcenter
; ycenter
; rad
;
;KEYWORDS:
; plot
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;Modified by Kamen Kozarev, 02/22/2014 - added the color keyword

radius=rad
points = (2 * !PI / 199.0) * FINDGEN(200)

x = xcenter + radius * cos(points)
y = ycenter + radius * sin(points)

if not keyword_set(color) then col=!P.color else col=color
;if not keyword_set(background) then bckg=!P.background else bckg=background

if keyword_set(plot) then plots,x,y,/device,thick=3,color=col

return, transpose([[x],[y]])

end
