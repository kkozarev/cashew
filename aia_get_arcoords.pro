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
; fits2headwcs, wds_convert_from_coord, wcs_get_pixel
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;01/2012, Kamen Kozarev: Introduced WCS procedures to make this more
;accurate. 
;

pscale=index.imscl_mp ;arcsecs per pixel
acx=angular_coords[0]*1.0 ;arcsecs
acy=angular_coords[1]*1.0 ;arcsecs

;Convert to a WCS structure
wcs=fitshead2wcs(index[0])
;wcs_convert_from_coord,wcs,[acx,acy],'hg',lon,lat
wcs_convert_from_coord,wcs,angular_coords,'hg',lon,lat
pxcoords=wcs_get_pixel(wcs,[acx,acy])
arlonlat=[lon,lat]

pcx=abs(pxcoords[0]-subroi_start[0])
pcy=abs(pxcoords[1]-subroi_start[1])

return,[pcx,pcy]
end
