pro call_aia_deproject

wav='193'
st='2011-05-11 02:10:00'
et='2011-05-11 02:10:48'
ring_width=550
files=aia_file_search(st,et,wav)


aia_project_annulus,files,ring_width,projdata

stop

end

pro aia_project_annulus_old, files, ring_width, projdata
;
;This program transforms the r-theta annulus above the solar limb to
;an x-y rectangular projection.
;
;
;INPUT
;     index - an index or array of indices for AIA data
;     data - a datacube of AIA images
;     ring_width - the width in pixels of the annulus above the limb
;
;OUTPUT
;     projdata - returns a datacube of the projected annulus only.
;
;Written by David Long, 02/2010

n=n_elements(files)
f=files
projdata=fltarr(721,ring_width,n)

; Project individual images
  for i = 0, n-1 do begin

  read_sdo, f[i], in, da ,/uncomp_delete 
  aia_prep, in, da, ind, dat
    
                                ;convert the index and data to
                                ;a map (Dominic Zarro's Mapping
                                ;software in SSW)
     ;http://hesperia.gsfc.nasa.gov/rhessidatacenter/complementary_data/maps/
     index2map, ind, dat, map
     ;to plot the map, do plot_map,map
     ;stop

     
     wcs = fitshead2wcs(ind)
     ;stop
     coord = wcs_get_coord(wcs)
     
     theta = dindgen(361)*(!dpi/180.)
     
     r_in = ind.rsun_obs
     r_out = ind.rsun_obs + ring_width
     
     x_in = r_in*cos(theta)
     y_in = r_in*sin(theta)
     
     arr_in = dblarr(2, 361)
     
     arr_in[0,*] = x_in
     arr_in[1,*] = y_in
     
     pix_in = wcs_get_pixel(wcs, arr_in)
     
     x_out = r_out*cos(theta)
     y_out = r_out*sin(theta)
     
     arr_out = dblarr(2, 361)
     
     arr_out[0,*] = x_out
     arr_out[1,*] = y_out
     
     pix_out = wcs_get_pixel(wcs, arr_out)
     
     pix_cen = wcs_get_pixel(wcs, [0,0])
     pix_edge_in = wcs_get_pixel(wcs, [r_in,0])
     pix_edge_out = wcs_get_pixel(wcs, [r_out,0])
     
     rad_pix = pix_edge_in[0] - pix_cen[0]
     
     rad_pix_in = rad_pix
     rad_pix_out = pix_edge_out[0] - pix_cen[0]
     
     rev_pix_x = reverse(pix_out[0,*])
     rev_pix_y = reverse(pix_out[1,*])
     
     ring = dblarr(2, 722)
     
     ring[0, 0:360] = pix_in[0,*]
     ring[0, 361:721] = rev_pix_x
     ring[1, 0:360] = pix_in[1,*]
     ring[1, 361:721] = rev_pix_y
     
     o = obj_new('idlanroi', ring[0,*], ring[1,*])
     
     mask = o->computemask(dim = [4096, 4096], mask_rule = 2)
     
     mask_img = map.data*mask
     
     sz = size(mask_img, /dimensions)
     
     new_r = rebin(transpose((rad_pix_out - rad_pix_in)*dindgen(ring_width)/ring_width-1. + rad_pix_in), 721, ring_width)  
     new_theta = rebin((dindgen(721)/720d)*(!dpi*2d), 721, ring_width)                  
     
     xpolar = new_r*cos(new_theta) + pix_cen[0]
     ypolar = new_r*sin(new_theta) + pix_cen[1]
     
     new_img = bilinear(mask_img, xpolar, ypolar)                                 
     
     rev_img_1 = reverse(new_img, 1)

     s_img_1 = shift(rev_img_1, 180, 0)
     projdata[*,*,i]=s_img_1

; Plot
;     plot_image, s_img_1, xtitle = 'Theta (0.5 degree increments)', min = -1e5, max = 1e5, $
;                 ytitle = 'Radius (0 = R_sun)', charsize = 2, title = '2010-06-13 event, t = '+num2str(map.time)

   
  endfor


end
