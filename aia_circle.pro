function aia_circle, xcenter, ycenter, rad, plot=plot
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
;

radius = rad + 10
points = (2 * !PI / 199.0) * FINDGEN(200)

x = xcenter + radius * cos(points)
y = ycenter + radius * sin(points)

if keyword_set(plot) then plots,x,y,/device,thick=3

return, transpose([[x],[y]])

end
