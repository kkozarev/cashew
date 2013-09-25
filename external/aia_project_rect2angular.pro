pro aia_project_rect2angular, index, data, projdata, norm=norm, plot=plot,loud=loud,latrange=latrange,latzero=latzero, resolution=resolution


;
;This program transforms the r-theta annulus above the solar limb to
;an x-y rectangular projection.
;
;
;INPUT
;     index - an index or array of indices for AIA data
;     data - a datacube of AIA images
;     ring_width - the width in pixels of the annulus above the limb
;                  (currently hard-coded.)
;
;OUTPUT
;     projdata - returns a datacube of the projected annulus only.
;
;Written by David Long, 02/2010
  
  n=n_elements(index)

;how much latitudinal extent to extract from the spherical image.
  if keyword_set(latrange) then angle_size=latrange else angle_size = 360d
;center of the image in degrees clockwise from north.
  if keyword_set(latzero) then positioning=latzero else positioning = 0.0d
;To get a resolution of 0.1 degrees you define a resolution of 10, for 0.5 degrees resolution = 2 and so on
if not keyword_set(resolution) then resolution = 10d;6000d   ; a resolution of 6000 gives 0.6 arcsecond per pixel... (KAK)
  ;ring_width = 400d ;width of the ring in the radial direction, arcseconds
  
;DEBUG!
     theta = (dindgen(angle_size+1.) - positioning)*(!dpi/180.)
;theta needs to be set such that the angular resolution matches that
;of AIA! This is not done currently. Then the 'resolution' keyword
;will become obsolete.
;END DEBUG!

     r_in = index[0].rsun_obs+9.0*index[0].imscl_mp

;DEBUG!
;Change made on August 11, 2011 by KAK
;     r_out = ind.rsun_obs + ring_width
;Have the radius of the annulus run between the limb and the edge of
;the image in the X-direction.
     r_out=index[0].naxis1/2.0*index[0].imscl_mp
;END DEBUG!

ring_width=fix((r_out-r_in)/index[0].imscl_mp)

  passband=strtrim(string(index[0].wavelnth),2)

  projdata = dblarr((angle_size*resolution)+1., ring_width,n)
  
  for i = 0, n-1 do begin
     if keyword_set(loud) then begin
        print,''
        print,'Preparing image '+strtrim(string(i+1),2)+' out of '+strtrim(string(n),2)
     endif

     ind=index[i]
     dat=data[*,*,i]

     index2map, ind, dat, map
     
     wcs = fitshead2wcs(ind)
     
     coord = wcs_get_coord(wcs)
     

     x_in = r_in*cos(theta)
     y_in = r_in*sin(theta)
     
     arr_in = dblarr(2, angle_size+1.)
     
     arr_in[0,*] = x_in
     arr_in[1,*] = y_in
     
     pix_in = wcs_get_pixel(wcs, arr_in)
     
     x_out = r_out*cos(theta)
     y_out = r_out*sin(theta)
     
     arr_out = dblarr(2, angle_size+1.)
     
     arr_out[0,*] = x_out
     arr_out[1,*] = y_out
     
     pix_out = wcs_get_pixel(wcs, arr_out)
     
     pix_cen = wcs_get_pixel(wcs, [0,0])
     pix_edge_in = wcs_get_pixel(wcs, [r_in,0])
     pix_edge_out = wcs_get_pixel(wcs, [r_out,0])
     
     rad_pix = pix_edge_in[0] - pix_cen[0]
     
     rad_pix_in = rad_pix
     rad_pix_out = pix_edge_out[0] - pix_cen[0]
     
     ring = dblarr(2, angle_size*2.+2)
     
     ring[0, 0:long(angle_size)] = pix_in[0,*]
     ring[0, long(angle_size+1):long(2*angle_size)+1] = reverse(pix_out[0,*], 2)
     ring[1, 0:long(angle_size)] = pix_in[1,*]
     ring[1, long(angle_size+1):long(2*angle_size)+1] = reverse(pix_out[1,*], 2)
     
     o = obj_new('idlanroi', ring[0,*], ring[1,*])
     
     mask = o->computemask(dim = [ind.naxis1, ind.naxis2], mask_rule = 2)
     
     mask_img = map.data*mask
     
     sz = size(mask_img, /dimensions)
     
     new_r = rebin(transpose((rad_pix_out - rad_pix_in)*dindgen(ring_width)/ring_width-1. + rad_pix_in), $
                   (angle_size*resolution + 1.), ring_width)  
     new_theta = rebin(((dindgen(angle_size*resolution + 1.)-positioning*resolution)*(!dpi/180d)*(1d/resolution)), $
                       (angle_size*resolution + 1.), ring_width)                  
     
     xpolar = new_r*cos(new_theta) + pix_cen[0]
     ypolar = new_r*sin(new_theta) + pix_cen[1]
     
     new_img = bilinear(mask_img, xpolar, ypolar)                                 
     
     rev_img_1 = reverse(new_img, 1)
     projdata[*,*,i]=rev_img_1
     
; Image normalisation along lines of constant R to try and highlight
; hidden features.
     if keyword_set(norm) then begin
        for j = 0, long(ring_width-1) do begin 
           projdata[*,j,i] = (rev_img_1[*,j])/mean(rev_img_1[*,j])
        endfor
     endif else begin
        for j = 0, long(ring_width-1) do begin 
           projdata[*,j,i] = (rev_img_1[*,j])
        endfor
     endelse

; Make difference image and plot
     if keyword_set(plot) then begin    
        image = reform(projdata[*,*,i])
        
        plot_image, sqrt(image), xtitle = 'Theta (0.25 degree increments)', ytitle = 'Radius in arcsec (0 = R_sun)', charsize = 2, $
                    title = 'Normalized image '+passband+'A, t = '+strtrim(string(map.time),2)
     endif
    
  endfor
  
end
