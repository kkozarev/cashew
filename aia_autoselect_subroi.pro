function aia_autoselect_subroi, index, angular_coords
;This function will automatically determine the subregion
;from the angular coordinates of the active region in arcseconds.


;Definitions

;This parameter determines the region: 4-fold is 1k square region,
;2-fold is 2k square region
fold=4.0

pscale=index.imscl_mp ;arcsecs per pixel
acx=angular_coords[0]*1.0 ;arcsecs
acy=angular_coords[1]*1.0 ;arcsecs
nx=4096.0 ;number of pixels in x-direction
ny=4096.0 ;number of pixels in y-direction
subroi=[0.0,0.0,0.0,0.0] ; the [x0,y0,x1,y1] pairs of coordinates

;First, convert arcsecs to pixels
;pcx and pcy are the AR coordinates, not an edge of the image!
pcx = acx / pscale + nx/2.0
pcy = acy / pscale + ny/2.0

if pcx ge nx-1 or pcy ge nx-1 then begin
   print,'Coordinates exceed image size. Aborting...'
   return,0
endif

;===================================================

;Next, figure out the roi size

;The X-direction
if pcx le nx/2.0 then begin
;If the x-position is in the eastern hemisphere,
;then find how close to the end it is. We are
;interested in stuff off the limb, so look towards
;the edges of the images.
   dx=pcx-nx/fold
   if dx le 0.0 then begin
      ;If the pcx is closer than nx/fold to the edge,
      ;the x-range is from 0 to nx/fold
      subroi[0]=0.0
      subroi[2]=nx/fold-1
   endif else begin
      ;If the pcx is farther than nx/fold from the edge,
      ;leave 100 pixels to the west of the input x coord.
      subroi[0]=pcx-nx/fold+100.0
      subroi[2]=pcx+100.0-1
   endelse
   
endif else begin
;If the x-position is in the western hemisphere,
;then find how close to the end it is. We are
;interested in stuff off the limb, so look towards
;the edges of the images.
   dx=(nx-1)-pcx
   if dx lt nx/fold then begin
      ;If the pcx is closer than nx/fold to the edge,
      ;the x-range is from (1.0-1.0/fold) nx to nx
      subroi[0]=(1.0-1.0/fold)*nx
      subroi[2]=nx-1
   endif else begin
      ;If the pcx is farther than nx/fold from the edge,
      ;leave 100 pixels to the east of the input x coord.
      subroi[0]=pcx-100.0
      subroi[2]=pcx+nx/fold-100.0-1
   endelse


endelse


;---------------------------------------------------


;The Y-direction
if pcy le ny/2.0 then begin
;If the y-position is in the southern hemisphere,
;then find how close to the end it is. We are
;interested in stuff off the limb, so look towards
;the edges of the images.
   dy=pcy-ny/fold/2.0
   if dy le 0.0 then begin
      ;If the pcy is closer than ny/fold/2.0 to the edge,
      ;the y-range is from 0 to ny/fold
      subroi[1]=0.0
      subroi[3]=ny/fold-1
   endif else begin
      ;If the pcy is farther than ny/fold/2.0 from the edge:
      subroi[1]=pcy-ny/fold/2.0
      subroi[3]=pcy-1+ny/fold/2.0
   endelse
   
endif else begin
;If the y-position is in the northern hemisphere,
;then find how close to the end it is. We are
;interested in stuff off the limb, so look towards
;the edges of the images.
   dy=(ny-1)-pcy
   if dy le ny/fold/2.0 then begin
      ;If the pcy is closer than ny/fold/2.0 to the edge,
      ;the y-range is from (1.0-1.0/fold)*ny to ny
      subroi[1]=(1.0-1.0/fold)*ny
      subroi[3]=ny-1
   endif else begin
      ;If the pcy is farther than ny/fold/2.0 from the edge:
      subroi[1]=pcy-ny/fold/2.0
      subroi[3]=pcy-1+ny/fold/2.0;
   endelse


endelse

;===================================================

return, fix(subroi)

end
