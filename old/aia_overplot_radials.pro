function aia_overplot_radials,index,image,latrange=latrange,angres=angres,flarepos=flarepos,offset=offset,profrad=profrad,tv=tv,color=color,radcirc=radcirc
;PURPOSE:
;This program makes a radial grid of profiles going from the sun
;center out to some distance above the limb, centered in latitude on a
;point the user supplies.
;offset is the [x,y] pair of coordinates of the lower left corner of
;the image on the full AIA frame, if it is a subframe.
;latrange in degrees around the flare site
;angres in degrees between successive radial profiles
;flarepos is the location of the flare.
;if tv is set, the tv command is used instead of tvscl.
;color is the color used to draw the radial profiles.
;if radcirc is set, overplot circles at every 0.1 Rs above the limb
;centered on sun center.
;
;CATEGORY:
; AIA/Kinematics
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
;Written by Kamen Kozarev, 03/2011
;

image=reform(image)
index=reform(index)

if not keyword_set(latrange) then latrange=30
if not keyword_set(angres) then angres=0.5
if not keyword_set(profrad) then profrad=900 ;pixels above the limb
if keyword_set(color) then !P.color=color else !P.color=255
!P.thick=3


wdef,0,n_elements(image[*,0]),n_elements(image[0,*])
if keyword_set(tv) then tv,image else plot_image,image,min=-70,max=20

if not keyword_set(offset) then offset=[0,0]
;sun center is at
cenx=index[0].X0_MP - offset[0]
ceny=index[0].X0_MP - offset[1]

if not keyword_set(flarepos) then begin
print,''
print,'Select event central position:'
cursor,x,y,/data
flarepos=[x,y]
endif
plots,flarepos[0],flarepos[1],psym=2,symsize=2,/data,color=0

xp=[cenx,flarepos[0]]
yp=[ceny,flarepos[1]]

inan=atan(float(yp[1]-yp[0])/float(xp[1]-xp[0]))*180/!PI

xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos(inan*!PI/180.))
yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin(inan*!PI/180.))
plots,xp,yp,/data,color=0

xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos((inan-5)*!PI/180.))
yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin((inan-5)*!PI/180.))
plots,xp,yp,/data,color=0

xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos((inan+5)*!PI/180.))
yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin((inan+5)*!PI/180.))
plots,xp,yp,/data,color=0


for n=0,fix(latrange/angres)-1 do begin
   an=(inan-latrange/2.0+angres*n)*!PI/180.
   
   xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos(an))
   yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin(an))
   
   plots,xp,yp,/data,linestyle=2
endfor


if keyword_set(radcirc) then begin
   lat=findgen(719)/2.0
   for i=0,5 do begin
      x=cenx+(index[0].R_SUN*(1+0.1*i))*cos(lat*!PI/180.0)
      y=ceny+(index[0].R_SUN*(1+0.1*i))*sin(lat*!PI/180.0)
      if i eq 0 then plots,x,y,/data else plots,x,y,/data,linestyle=2
   endfor
endif


return,flarepos


end
