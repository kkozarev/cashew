pro test_aia_project_annulus_2

wav='193'
st='2011-05-11 02:19:00'
et='2011-05-11 02:40:12'
ring_width=500
files=aia_file_search(st,et,wav)

; Identify AEC-affected images.
  read_sdo, files, ind_arr, /nodata, only_tags = 'exptime,rsun_obs'
  tmp=where(ind_arr.exptime gt 1.)
  fls = files[tmp]
  ind_arr=ind_arr[tmp]

aia_project_annulus_2,fls,ring_width,projdata,/rawplot
stop

;Explore the lateral expansion speed



stop
end


pro aia_project_annulus_2, files, ring_width, projdata, norm=norm, rawplot=rawplot, diffplot=diffplot, loud=loud,latrange=latrange,latzero=latzero, resolution=resolution,savepath=savepath
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
;Upgraded by Kamen Kozarev, 07/2013

;The save path
  if not keyword_set(savepath) then savepath='./'
;how much latitudinal extent to extract from the spherical image.
  if keyword_set(latrange) then angle_size=latrange else angle_size = 100d
;center of the image in degrees clockwise from north.
  if keyword_set(latzero) then positioning=latzero else positioning = 25d
;To get a resolution of 0.1 degrees you define a resolution of 10, for 0.5 degrees resolution = 2 and so on
  if not keyword_set(resolution) then resolution = 10d   
  ;ring_width = 400d ;width of the ring in the radial direction, arcseconds
  n=n_elements(files)
  f=files
  projdata = dblarr(n, (angle_size*resolution)+1., ring_width)
  
  
  for i = 0, n-1 do begin
     if keyword_set(loud) then begin
        print,''
        print,'Preparing image '+strtrim(string(i+1),2)+' out of '+strtrim(string(n),2)
     endif

     
     read_sdo, f[i], in, da ,/uncomp_delete
     da*=1.0D
     aia_prep, in, da, ind, dat
     dat[where(dat lt 0.0)]=0.0D
     da=0.0

     passband=strtrim(string(ind.wavelnth),2)
     
     index2map, ind, dat, map

     wcs = fitshead2wcs(ind)
     
     coord = wcs_get_coord(wcs)
     
     theta = (dindgen(angle_size+1.) - positioning)*(!dpi/180.)
     
     r_in = ind.rsun_obs
     r_out = ind.rsun_obs + ring_width
     
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
     
     mask = o->computemask(dim = [4096, 4096], mask_rule = 2)
     
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
     
; Image normalisation along lines of constant R to try and highlight
; hidden features.
     if keyword_set(norm) then begin
        for j = 0, long(ring_width-1) do begin 
           projdata[i,*,j] = (rev_img_1[*,j])/mean(rev_img_1[*,j])
        endfor
     endif else begin
        for j = 0, long(ring_width-1) do begin 
           projdata[i,*,j] = (rev_img_1[*,j])
        endfor
     endelse

;Plot the image
     if keyword_set(rawplot) or keyword_set(diffplot) then begin    
        if i eq 0 then begin
           
           tvlct,rr,gg,bb,/get
           ;tvlct,reverse(rr),reverse(gg),reverse(bb)
           wdef,0,n_elements(projdata[0,*,0]),n_elements(projdata[0,0,*])
           !p.font=0
        endif
        
        image = reform(projdata[i,*,*])
        step=strtrim(string(i),2)
        if i lt 10 then step='0'+step
        if i lt 100 then step='0'+step
        fname='t'+step+'.png'
        
        if keyword_set(rawplot) then begin
           loadct,9,/silent
           tvlct,rr,gg,bb,/get
           tvlct,reverse(rr),reverse(gg),reverse(bb)
           
           im=sqrt(image)
           if i eq 0 then maxx=max(im)/2.0
           tvscl,bytscl(im,0,maxx)
           
           xyouts,0.01,0.96,'AIA/'+strtrim(string(ind.wavelnth),2)+'   '+ind.date_obs,$
                  /norm,charsize=2,charthick=2
           finfname='raw_'+fname
           write_png,'deprojected_'+finfname,tvrd(/true)
        endif

        if keyword_set(diffplot) and i gt 0 then begin
           loadct,0,/silent
           tvlct,rr,gg,bb,/get
           tvscl,bytscl(projdata[i,*,*]-projdata[0,*,*],$
                        min(projdata[i,*,*]-projdata[i-1,*,*])/5.0,$
                        max(projdata[i,*,*]-projdata[i-1,*,*])/10.0)
           xyouts,0.01,0.96,'AIA/'+strtrim(string(ind.wavelnth),2)+'   '+ind.date_obs,$
                  /norm,charsize=2,charthick=2
           finfname='rdiff_'+fname
           write_png,'deprojected_'+finfname,tvrd(/true)
        endif
                                ;plot_image, sqrt(image), xtitle = 'Theta (0.25 degree increments)', ytitle = 'Radius in arcsec (0 = R_sun)', charsize = 2, $
                                ;           title = 'Normalized image
                                ;           '+passband+'A, t =
                                ;           '+strtrim(string(map.time),2)
     endif
  endfor
  
  
  
; If 335A, max = 15, else max = 5
  
;  max = (passband eq 335) ? 15 : 5
;  
;  window, 0, xs = 1000, ys = 1000
;  !p.multi = 0
;  
;  arr = transpose(projdata, [1, 2, 0])
  
;  f = file_search('/Volumes/Data_HDD/Data/SDO/20100613/'+num2str(passband)+'A/*.fits')
  
;  n = size(f, /n_elements)
  
;  for i = 0, n-1 do begin
     
;     read_sdo, f[i], in, da
     
;     plot_image, arr[*,*,i], xtitle = 'Theta ('+num2str(int(1./resolution))+' degree increments)', $
;                 ytitle = 'Radius in arcsec (0 = R_sun)', charsize = 2, min = 0, max = max, $
;                 title = 'Normalised image '+num2str(passband)+'A, t = '+num2str(anytim(in.t_obs, /yohkoh))
     
;     IF i LE 9 THEN BEGIN
;        s = arr2str(i, /trim)
;        x2png, 'polar_plot_20100613_'+num2str(passband)+'_00' + s +'.png'
;     ENDIF
;     IF (i GT 9) AND (i LE 99) THEN BEGIN
;        s = arr2str(i, /trim)
;        x2png, 'polar_plot_20100613_'+num2str(passband)+'_0' + s +'.png'
;     ENDIF
;     IF i GT 99 THEN BEGIN
;        s = arr2str(i, /trim)
;        x2png, 'polar_plot_20100613_'+num2str(passband)+'_' + s +'.png'
;     ENDIF
;     
;  endfor

end
