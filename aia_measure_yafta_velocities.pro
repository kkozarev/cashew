pro aia_measure_yafta_velocities,latrange=latrange,angres=angres,profrad=profrad
;This program will serve to measure velocities based on yaftawave features.
; Measurements will be made along radial lines passing through the features,
;as well as along concentric arcs parallel to the limb, also passing through the features.
;Kamen Kozarev 11/01/2011

if not keyword_set(latrange) then latrange=45 ;latitudinal extent of the radial lines
if not keyword_set(angres) then angres=0.5 ;distance between succesive lines
if not keyword_set(profrad) then profrad=900 ;pixels above the limb


;Load the AIA data
;This is a datacube used for testing.
restore,'/Users/kkozarev/AIA/algoTests/yaftawave/normalized_AIA_20110125_05_193_subdata.sav'
index=subindex
subindex=0

;sun center is at
cenx=index[0].X0_MP - offset[0]
ceny=index[0].X0_MP - offset[1]

;The location of the AR, in pixels!
if keyword_set(arcoords) then begin
   flarepos=aia_get_arcoords(reform(index[0]), arcoords)
endif else begin
   if not tag_exist(reform(index[0]),"ARX0") then begin
      print,'AR Coordinates not present. Aborting...'
      return,0
   endif
   flarepos=[index[0].ARX0,index[0].ARY0]
endelse
plots,flarepos[0],flarepos[1],psym=2,symsize=2,/data,color=0

xp=[cenx,flarepos[0]]
yp=[ceny,flarepos[1]]



;Load the corresponding yaftawave results



;Plot an image of the FOV
tvscl,subdata[*,*,0]

;Plot radial lines separated by angres degrees passing through the feature and sun center.
inan=atan(float(yp[1]-yp[0])/float(xp[1]-xp[0]))*180/!PI

xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos(inan*!PI/180.))
yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin(inan*!PI/180.))
plots,xp,yp,/data,color=0

;xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos((inan-5)*!PI/180.))
;yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin((inan-5)*!PI/180.))
;plots,xp,yp,/data,color=0

;xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos((inan+5)*!PI/180.))
;yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin((inan+5)*!PI/180.))
;plots,xp,yp,/data,color=0

for n=0,fix(latrange/angres)-1 do begin
   an=(inan-latrange/2.0+angres*n)*!PI/180.
   xp[1] = xp[0] + round((profrad+index[0].R_SUN)*cos(an))
   yp[1] = yp[0] + round((profrad+index[0].R_SUN)*sin(an))
   plots,xp,yp,/data,linestyle=2
endfor

;Plot concentric arcs above the limb passing through the feature.
lat=findgen(719)/2.0
for i=0,5 do begin
   x=cenx+(index[0].R_SUN*(1+0.1*i))*cos(lat*!PI/180.0)
   y=ceny+(index[0].R_SUN*(1+0.1*i))*sin(lat*!PI/180.0)
   if i eq 0 then plots,x,y,/data else plots,x,y,/data,linestyle=2
endfor

;;;;;;;;;Start the time loop. For every time step, measure the feature locations.

;1. For the radial lines, measure the feature position along each line and record.


;2. For the curved lines, measure the angular distance from the central radial line
;on both sides. the central radial line is the line passing through the sun center and 
;through the AR location.

;The results will be recorded in separate arrays/structure arrays.


;;;;;;;;End the time loop.


;Possibly, this procedure will determine the velocities and accelerations by doing
;second- and third-order, as well as power law fitting to the positions over time.
;I will need to specify some uncertainty in the position determination, and then run 
;the bootstrapping routine to minimize the errors.


;Finally, the results will have to be visualized in some way...
;Need to figure out how to map the velocities over time on a dynamic plot.




end
