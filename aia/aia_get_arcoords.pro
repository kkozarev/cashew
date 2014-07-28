function aia_get_arcoords, index, angular_coords, subroi_start, arlonlat=arlonlat
;PURPOSE:
;A small program to convert angular to pixel AR coordinates.
;
;CATEGORY:
; AIA/General
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
; fits2headwcs, wcs_convert_from_coord, wcs_get_pixel
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;01/2012, Kamen Kozarev: Introduced WCS procedures to make this more
;accurate. 
;
if n_elements(angular_coords) lt 2 then begin
   print,'Input coordinates not an array (angular_coords[2]). Quitting...'
   return,-1
endif

pscale=index.imscl_mp ;arcsecs per pixel
acx=angular_coords[0]*1.0 ;arcsecs
acy=angular_coords[1]*1.0 ;arcsecs

;Convert to a WCS structure
wcs=fitshead2wcs(index[0])
pxcoords=wcs_get_pixel(wcs,[acx,acy])
arlonlat=arcmin2hel(acx/60.,acy/60.,date=index.date_obs)

pcx=abs(pxcoords[0]-subroi_start[0])
pcy=abs(pxcoords[1]-subroi_start[1])

return,[pcx,pcy]
end
