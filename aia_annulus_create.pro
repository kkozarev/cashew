pro test_aia_annulus_create
wav='193'
events=load_events_info()
ring_width=550
;The May 2011 event is index=3
files=aia_file_search(events[3].st,events[3].et,wav)
thrange=[10,120]
savepath=events[3].savepath
rrange=[0.9,1.5] ;radial range from Sun center in Rs
aia_annulus_create,files,projdata,thrange=thrange,/diff,/plot,savepath=savepath,rrange=rrange
stop
end


pro aia_annulus_create, f, projdata, passband=passband, diff=diff, plot=plot, norm=norm, latzero=latzero, ring_width=ring_width,thrange=thrange,rrange=rrange,datascale=datascale,savename=savename,savepath=savepath
;PURPOSE:
; Routine to produce RD polar-deprojected data using an annulus technique applied to SDO
; images.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;
;KEYWORDS:
;   FILES: The list of files to read for the deprojected images.
;   PASSBAND: Passband being studied. Required for labelling and to
;             identify data.
;   DIFF: Set to produce windowed running difference images.
;   RING_WIDTH - a width of the ring, in Rsun
;   RRANGE - radial range from center of the sun, in Rsun
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
; 
;
;MODIFICATION HISTORY:
;   Written 2013-Jul-07 by David Long using code from rd_annulus.pro.
;   2013/07/30 - Reworked for the CfA infrastructure by Kamen Kozarev
;   2013/09/30 - Added savename, savepath keywords - Kamen Kozarev
;


  
  if not keyword_set(passband) then passband = '193'
  img_size = [1200., 800.]
  ang_step = 0.2                ; Angle step size (degree)
  res = 0.5                     ; Height step size (arcsec)

; Width of ring (effectively distance from limb to edge of aperture in arcsec)
  if not keyword_set(ring_width) then ring_width = 400d $
     else ring_width
  tot_ang = 360.                ; Looking at 360 degrees in steps of ang_step
  if not keyword_set(thrange) then thrang=[0,360.] else thrang=thrange
  thrang*=!PI/180.0
  
  
  


; Identify AEC-affected images.
  read_sdo, f, ind_arr, /nodata, only_tags = 'exptime,rsun_obs'
  fls = f[where(ind_arr.exptime gt 1.)]
  read_sdo, fls, ind_arr, /nodata
  
;Convert the radial range to arcsecs.
  if keyword_set(rrange) then begin
     rrang=(rrange-1)*ind_arr.rsun_obs
  endif else begin
     rrang=[0.0,ring_width]
  endelse


; Setup Z-buffer for plotting 
  set_plot, 'z'
  Device, Set_Resolution=[img_size[0], img_size[1]], set_pixel_depth=24, decomposed = 1   
  !p.multi = 0
  
; Define inner and outer radius
  r_in = ind_arr[0].rsun_obs+rrang[0]    ; Inner radius
  r_out = ind_arr[0].rsun_obs + rrang[1] ; Outer radius
  
; Get image coordinates
  aia_prep, fls[0], -1, ind, dat
  wcs = fitshead2wcs(ind) 
  crd = wcs_get_coord(wcs)
  sz = size(dat)

  if keyword_set(datascale) then datascale=[ang_step, res/ind.cdelt1]

; Get the height of each image pixel from Sun centre
  h = float(reform(sqrt((crd[0,*,*] - 0.)^2. + (crd[1,*,*] - 0.)^2.), sz[1], sz[2]))  

; Identify pixels that we're interested in
  arc_out = where(h gt (r_in-1.))
  arc_in = where(h le (r_in-1.))

; Using code from nrgf.pro by Huw Morgan to try and histogram out the
; pixels by distance from Sun centre.

; Max number of bins
  maxbin = round(max(h[arc_out])-min(h[arc_out]))
; No bins we want
  dist_bins = (r_out-r_in)/res

; Histogram the pixels that we're interested in
  hgt = h[arc_out]
  hts = (min(hgt)+findgen(maxbin)*(max(hgt)-min(hgt))/(maxbin-1.))/ind.cdelt1

; Convert identified pixels to r-theta space
  a = (max(hts) - min(hts))*dindgen(dist_bins)/(dist_bins-1.) + min(hts)  
  new_theta = rebin((dindgen(tot_ang/ang_step)*ang_step)*!dtor, tot_ang/ang_step, n_elements(a))
  new_r = rebin(transpose(a), tot_ang/ang_step, n_elements(a))
   
  
; Sort out new x- and y-coordinates
  p = wcs_get_pixel(wcs, [0,0])  
  xpol = new_r*cos(new_theta)+p[0]  
  ypol = new_r*sin(new_theta)+p[1]       
  
; If differencing normal cadence images, use running window of 5
; images.
  if keyword_set(diff) then lwr_img = 5 else lwr_img = 0

; FOR loop to produce difference images
  for img_no = lwr_img, size(fls, /n_elements)-1 do begin
     
; First image
     aia_prep, fls[img_no-lwr_img], -1, i_0, dat_0
     dat_0 = dat_0/i_0.exptime
 
; Second image    
     if keyword_set(diff) then begin
        aia_prep, fls[img_no], -1, i_1, dat_1
        dat_1 = dat_1/i_1.exptime

; Difference image    
        d_img = dat_1 - dat_0
     endif else d_img = dat_0

; Remove pixels that we're not interested in. 
     msk = d_img
     msk[arc_out] = 1.
     msk[arc_in] = 0.
     
     diff_img = d_img*msk

     if not keyword_set(diff) then diff_img = diff_img^0.225

; Interpolate the annulus to a square plot
     img = shift(reverse(bilinear(diff_img, xpol, ypol)), 90./ang_step)
     if img_no eq lwr_img then projdata=dblarr(size(fls, /n_elements)-lwr_img,n_elements(img[*,0]),n_elements(img[0,*]))
     projdata[img_no-lwr_img,*,*]=img
  
; Define x and y zero points for plotting     
     x_pos = thrang[0]*180./!PI
     y_pos = r_in-1.
     
  ;Select the angular coverage of the image
     plotimg=img
     if keyword_set(thrange) then begin
        tmp=where(new_theta[*,0] ge thrang[0] and new_theta[*,0] le thrang[1])
        plotimg=img[tmp,*]
     endif
     
; Define image title      
     if keyword_set(diff) then img_tit = '!5RD image '+passband+'A, t = '+num2str(anytim(i_1.date_d$obs, /yohkoh, /time_only, /sec, /trun)) else $
        img_tit = '!5Plain image '+passband+'A, t = '+num2str(anytim(i_0.date_d$obs, /yohkoh, /time_only, /sec, /trun))
  
; Sort out image scaling   
     if keyword_set(diff) then begin
        case passband of
           '94': int_range = [-5., 5]
           '304': int_range = [-5., 5]
           '335': int_range = [-5., 5]
           '131': int_range = [-5., 5]
           '171': int_range = [-5., 5]
           '193': int_range = [-50, 50]
           '211': int_range = [-50, 50]
           else: int_range = [min(plotimg), max(plotimg)]
        endcase
     endif else begin
        int_range = [min(plotimg), max(plotimg)]
     endelse
; Sort out image colour
     if keyword_set(diff) then loadct, 0 else aia_lct, r, g, b, wavelnth = passband, /load

     if keyword_set(plot) then begin
; Plot the image
        plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
                    ytitle = '!5Radius [arcsec from Sun centre]', $
                    charsize = 1.5, title = img_tit, max = int_range[1], $
                    origin = [x_pos[0], y_pos], charthick = 1.1, $
                    scale = [ang_step, res/ind.cdelt1], $
                    pos = [0.1, 0.1, 0.95, 0.95], min = int_range[0]
        
; Save the image as a PNG
        tvlct, r, g, b, /get
        snapshot = TVRD(/true)
;     image = bytarr(3, img_size[0], img_size[1])
;     image[0,*,*] = r[snapshot]
;     image[1,*,*] = g[snapshot]
;     image[2,*,*] = b[snapshot]
        
        if keyword_set(diff) then $
           write_png, 'rd_annulus_plot_'+passband+'_' + $
                      time2file(i_1.date_d$obs, /sec)+'.png', snapshot $
        else write_png, 'plain_annulus_plot_'+passband+'_' + $
                        time2file(i_0.date_d$obs, /sec)+'.png', snapshot
     endif
  endfor

  set_plot, 'x'
  
  tmp=strsplit(ind[0].date_obs,'-T',/extract)
  date=tmp[0]+tmp[1]+tmp[2]
  if not keyword_set(savepath) then savepath=''
  if not keyword_set(savename) then savname=savepath+'aia_deprojected_annulus_'+date+'_'+passband+'.sav' $
  else savname=savepath+savename
  save,filename=savname,projdata,ring_width,thrang,passband,ang_step,res,ind_arr,new_theta,rrange
 
end
