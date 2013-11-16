pro test_aia_annulus_create
;wav='193'
;event=load_events_info(label='110511_01')
;ring_width=550
;The May 2011 event is index=3
;thrange=[10,120]
rrange=[0.9,1.5] ;radial range from Sun center in Rs
;aia_annulus_create,event,thrange=thrange,/run,/plot,savepath=savepath,rrange=rrange
;stop
;stop

;Alternatively, run for all events
events=load_events_info()
ring_width=550
wavelengths=['193','211']

;n_elements(events)-1
for ev=0,n_elements(events)-1 do begin
   event=events[ev]
   for w=0,n_elements(wavelengths)-1 do begin
      wavelength=wavelengths[w]
      aia_annulus_create,event,thrange=thrange,/run,/plot,rrange=rrange,wav=wavelength
   endfor
endfor
end


pro aia_annulus_create, event, wav=wav, run=run, base=base, plot=plot, norm=norm, latzero=latzero, ring_width=ring_width,thrange=thrange,rrange=rrange,datascale=datascale,savename=savename,savepath=savepath,annulus_data=annulus_data
;PURPOSE:
; Routine to produce RD polar-deprojected data using an annulus technique applied to SDO
; images.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;   EVENT - load an event information structure
;
;KEYWORDS:
;   FILES: The list of files to read for the deprojected images.
;   WAV: Passband being studied. Required for labelling and to
;             identify data.
;   RUN: Set to produce windowed running difference images.
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
;   2013/07/30, Kamen Kozarev - Reworked for the CfA infrastructure
;   2013/09/30, Kamen Kozarev - Added savename, savepath keywords
;   2013/11/13, Kamen Kozarev - Integrated the event 

  tmp=strsplit(event.date,'/',/extract)
  date=tmp[0]+tmp[1]+tmp[2]
  if not keyword_set(wav) then passband = '193' else passband = wav
  if not keyword_set(savepath) then savepath=event.savepath+'annulusplot/'
  fls=aia_file_search(event.st,event.et,passband,missing=locmissing,/remove_aec)
  
  img_size = [1200., 800.]
  ang_step = 0.2                ; Angle step size (degree)
  res = 0.5                     ; Height step size (arcsec)

; Width of ring (effectively distance from limb to edge of aperture in arcsec)
  if not keyword_set(ring_width) then ring_width = 500d $
     else ring_width
  tot_ang = 360.                ; Looking at 360 degrees in steps of ang_step
  if not keyword_set(thrange) then thrang=[0,360.] else thrang=thrange
  thrang*=!PI/180.0
  
; Identify AEC-affected images.
 ; read_sdo, f, ind_arr, /nodata, only_tags = 'exptime,rsun_obs'
  ;fls = f[where(ind_arr.exptime gt 1.)]
  read_sdo, fls, ind_arr, /nodata
  
  ;aia_load_event,event.st,event.et,wav,ind_arr,data,/remove_aec,/nodata


  
;Convert the radial range to arcsecs.
  if keyword_set(rrange) then begin
     rrang=(rrange-1)*ind_arr.rsun_obs
  endif else begin
     rrang=[0.0,ring_width]
  endelse


; Setup Z-buffer for plotting 
  set_plot, 'z'
  Device, Set_Resolution=[img_size[0], img_size[1]], set_pixel_depth=24, decomposed = 0
  !p.multi = 0
  !P.font=1
  chsize=1.6
  chthick=1.0
  
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
  if keyword_set(run) then lwr_img = 5 else lwr_img = 0

; MAIN TIME STEP LOOP!
  for img_no = lwr_img, size(fls, /n_elements)-1 do begin
     img_strind=strtrim(string(img_no),2)
     if img_strind lt 100 then img_strind='0'+img_strind
     if img_strind lt 10 then img_strind='0'+img_strind

; First image
     aia_prep, fls[img_no-lwr_img], -1, i_0, dat_0
     dat_0 = dat_0/i_0.exptime
 
; Second image    
     if keyword_set(run) then begin
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

     if not keyword_set(run) then diff_img = diff_img^0.225

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
     if keyword_set(run) then begin
        img_tit = '!5RDIFF image '+passband+'A, t = '+num2str(anytim(i_1.date_d$obs, /yohkoh, /time_only, /sec, /trun))
     endif else begin
        if keyword_set(base) then begin
           img_tit = '!5BDIFF image '+passband+'A, t = '+num2str(anytim(i_0.date_d$obs, /yohkoh, /time_only, /sec, /trun))
        endif else begin
           img_tit = '!5Raw image '+passband+'A, t = '+num2str(anytim(i_0.date_d$obs, /yohkoh, /time_only, /sec, /trun))
        endelse
     endelse
  
; Sort out image scaling   
     if keyword_set(run) then begin
        case passband of
           '94': int_range = [-5., 5]
           '304': int_range = [-5., 5]
           '335': int_range = [-5., 5]
           '131': int_range = [-5., 5]
           '171': int_range = [-5., 5]
           '193': int_range = [-50, 30]
           '211': int_range = [-50, 30]
           else: int_range = [min(plotimg), max(plotimg)]
        endcase
     endif else begin
        int_range = [min(plotimg), max(plotimg)]
     endelse

  tmp=strsplit(ind[0].date_obs,'-T',/extract)
  date=tmp[0]+tmp[1]+tmp[2]
  if not keyword_set(savename) then savname=savepath+'aia_deprojected_annulus_'+date+'_'+passband+'.sav' $
  else savname=savepath+savename
  save,filename=savname,projdata,ring_width,thrang,passband,ang_step,res,ind_arr,new_theta,rrang
  

;START PLOTTING
     if keyword_set(plot) then begin
        
; Sort out image colour
        ;if keyword_set(run) then loadct, 0
       ;else aia_lct, r, g, b, wavelnth = passband, /load
        loadct,0,/silent
        ;if keyword_set(run) then loadct,8,/silent else loadct,0,/silent
        tvlct, r, g, b, /get
        col=0
        bckg=255
        if not keyword_set(run) then begin
           tvlct,reverse(r),reverse(g),reverse(b)
           col=255
           bckg=0
        endif
; Plot the image
        plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
                    ytitle = '!5Radius [arcsec from Sun centre]', $
                    charsize = chsize, title = img_tit, max = int_range[1], $
                    origin = [x_pos[0], y_pos], charthick = chthick, $
                    scale = [ang_step, res/ind.cdelt1], $
                    pos = [0.1, 0.1, 0.95, 0.95], min = int_range[0]
; Save the image as a PNG
        
        snapshot = TVRD(/true)
;     image = bytarr(3, img_size[0], img_size[1])
;     image[0,*,*] = r[snapshot]
;     image[1,*,*] = g[snapshot]
;     image[2,*,*] = b[snapshot]
        
        if keyword_set(run) then begin
              folder='arun/'
              prefix='annplot_'
              postfix='run'
        endif else begin
           if keyword_set(base) then begin
              folder='abase/'
              prefix='annplot_'
              postfix='base'
           endif else begin
              folder='araw/'
              prefix='annplot_'
              postfix='raw'
           endelse
        endelse
        if not dir_exist(savepath+folder+wav+'/') then spawn,'mkdir '+savepath+folder+wav+'/'

        write_png, savepath + folder + wav +'/' + $
                   prefix + date + '_' + event.label+'_'+wav+'_'+postfix+'_'+img_strind+'.png', $
                   snapshot,r,g,b
     endif

;END PLOTTING
endfor

  set_plot, 'x'
  

  if keyword_set(annulus_data) then annulus_data=projdata

end
