; Quick routine to try and transform annulus to plot of r against
; angle

pro aia_project_annulus, index, data, ring_width, projdata

;  window, 0, xs = 1500, ys = 700
;  !p.multi = [0,1,1]
;  loadct, 0

;  f = file_search('/Volumes/Data_HDD/Data/SDO/20100613/193A/*.fits')

;  n = size(f, /n_elements)

n=n_elements(index)


projdata=fltarr(721,ring_width,n)


; Base image

;  read_sdo, f[0], in, da  
;  aia_prep, in, da, ind, dat
  
ind=index[0]
dat=data[*,*,0]
  index2map, ind, dat, map
  
  wcs = fitshead2wcs(ind)
  
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
  
  rev_img_0 = reverse(new_img, 1)

  s_img_0 = shift(rev_img_0, 180, 0)
  
projdata[*,*,0]=s_img_0

; Second image
  for i = 1, n-1 do begin
  ind=index[i]
  dat=data[*,*,i]

    ; read_sdo, f[i], in, da
   
    ; aia_prep, in, da, ind, dat
    
     index2map, ind, dat, map
     
     wcs = fitshead2wcs(ind)
     
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

; Make difference image and plot

     diff_image = s_img_1 - s_img_0

     plot_image, diff_image, xtitle = 'Theta (0.5 degree increments)', min = -1e5, max = 1e5, $
                 ytitle = 'Radius (0 = R_sun)', charsize = 2, title = '2010-06-13 event, t = '+num2str(map.time)

;     IF i LE 9 THEN BEGIN
;        s = arr2str(i, /trim)
;        x2png, 'polar_plot_20100613_193_00' + s +'.png'
;     ENDIF
;     IF (i GT 9) AND (i LE 99) THEN BEGIN
;        s = arr2str(i, /trim)
;        x2png, 'polar_plot_20100613_193_0' + s +'.png'
;     ENDIF
;     IF i GT 99 THEN BEGIN
;        s = arr2str(i, /trim)
;        x2png, 'polar_plot_20100613_193_' + s +'.png'
;     ENDIF
;     
;     ans = ' '
;     read, 'ok?', ans
   
  endfor
     
  plot_image, mask_img^0.35
;stop
  set_line_color

;  for i = 0, 360 do begin

;     r_line = dindgen(ring_width) + r_in
     
;     x_line_in = r_line*cos(i*(!dpi/180d))
;     y_line_in = r_line*sin(i*(!dpi/180d))
     
;     arr_line = dblarr(2, ring_width)
;     arr_line[0,*] = x_line_in
;     arr_line[1,*] = y_line_in
     
;     pix_line = wcs_get_pixel(wcs, arr_line)
     
;     oplot, pix_line[0,*], pix_line[1,*], color = 3

;     wait, 0.1

;  endfor


;stop

end
