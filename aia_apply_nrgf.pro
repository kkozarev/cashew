function aia_apply_nrgf, index, data

;This function will take one or more images and apply the NRGF (Morgan
;et al. 2006) on them after projecting them into angular coordinates 
;using aia_project_rect2angular.pro

;The function returns a deprojected image similar to the input, but
;with normalized radial brightness counts. The result will be used
;with one of the feature finding algorithms, such as YAFTA.

  
;1. Run aia_project_rect2angular on the input in order to turn it into
;an x-y map with position angle on X and height above the limb on Y.
;The resulting image will span 360 degrees in position angle and range
;in height from the limb to the last height at which a full circle can
;be drawn on the AIA image without going out of bounds of the CCD.
aia_project_rect2angular,index,data,projdata

;2. Looping over all rows of the new image: 
;a) find the average brightness (Bavg) and standard deviation
;(SigBavg) of the row; 
;b) calculate the normalized brightness Bnorm = (B-Bavg)/SigBavg,
;where B is the brightness of a single pixel along the row.
nx=n_elements(projdata[*,0,0])
ny=n_elements(projdata[0,*,0])
nt=n_elements(projdata[0,0,*])
normdata=projdata

for tt=0,nt-1 do begin
   im=reform(projdata[*,*,tt])
   for yy=0,ny-1 do begin
      Bavg=mean(im[*,yy])
      SigBavg=stddev(im[*,yy])
      for xx=0,nx-1 do im[xx,yy]=(im[xx,yy]-Bavg)/SigBavg
   endfor
   normdata[*,*,tt]=im
endfor

;3. Deproject the image back to the original rectangular coordinates
;using aia_project_angular2rect.pro. NOTE: This function does not exist yet -
;how to do it?
;aia_project_angular2rect,index,normdata,projdata

return, normdata;projdata
end
