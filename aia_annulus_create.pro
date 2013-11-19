pro test_aia_annulus_create

;You can run for one event, like this.
  one=0
  if one eq 1 then begin
     event=load_events_info(label='110511_01')
     rrange=[0.9,1.25]          ;radial range from Sun center in Rs
     aia_annulus_create,event,/run,/plot,rrange=rrange,/force
     aia_annulus_create,event,/base,/plot,rrange=rrange
     aia_annulus_create,event,/plot,rrange=rrange
  endif


;Alternatively, run for all events
  all=1
  if all eq 1 then begin
     events=load_events_info()
     rrange=[0.9,1.25]          ;radial range from Sun center in Rs
     wavelengths=['193','211']
;n_elements(events)-1
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_annulus_create,event,/run,/plot,rrange=rrange,wav=wavelength,/force
           aia_annulus_create,event,/base,/plot,rrange=rrange,wav=wavelength
           aia_annulus_create,event,/plot,rrange=rrange,wav=wavelength
        endfor
     endfor
  endif
end


pro aia_annulus_create, event, wav=wav, run=run, base=base, plot=plot, norm=norm, centerlat=centerlat, ring_width=ring_width,thrange=thrange,rrange=rrange,datascale=datascale,savename=savename,savepath=savepath,annulus_data=annulus_data,full=full,force=force
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
;   2013/11/13, Kamen Kozarev - Integrated the event structure
  
  tmp=strsplit(event.date,'/',/extract)
  date=tmp[0]+tmp[1]+tmp[2]
  if not keyword_set(wav) then passband = '193' else passband = wav
  if not keyword_set(savepath) then savepath=event.savepath+'annulusplot/'
  fls=aia_file_search(event.st,event.et,passband,missing=locmissing,/remove_aec)
  if not keyword_set(centerlat) then begin
     if event.arlon lt 0.0 then arlat=270.0+event.arlat $
     else arlat=90.0-event.arlat
  endif else arlat=centerlat
  
  img_size = [1200., 800.]
  ang_step = 0.2                ; Angle step size (degree)
  res = 0.5                     ; Height step size (arcsec)

; Width of ring (effectively distance from limb to edge of aperture in
; arcsec)
  if not keyword_set(ring_width) then ring_width = 500d
  tot_ang = 360.                ; Looking at 360 degrees in steps of ang_step
  if not keyword_set(thrange) then thrang=[arlat-60.,arlat+60.] else thrang=thrange
  if keyword_set(full) then thrang=[0.,360.]
  
  if thrang[0] lt 0. then thrang[0]=0.
  if thrang[1] gt 360. then thrang[1]=360.
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

; If differencing normal cadence images, use running window of N
; images.
  lwr_img = 0
  if keyword_set(run) or keyword_set(base) then lwr_img = 3
  
;Create the base difference image here
if keyword_set(base) then begin
   for img_no = 0, lwr_img - 1 do begin
      aia_prep, fls[img_no], -1, i_tmp, dat_tmp
      tmp=dat_tmp/i_tmp.exptime
      if img_no eq 0 then dat_init = tmp else dat_init = dat_init+tmp
   endfor
   dat_init=dat_init/(1.0*lwr_img)
endif   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN TIME STEP LOOP!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  for img_no = lwr_img, size(fls, /n_elements)-1 do begin
     img_strind=strtrim(string(img_no),2)
     if img_strind lt 100 then img_strind='0'+img_strind
     if img_strind lt 10 then img_strind='0'+img_strind
     
; First image
     aia_prep, fls[img_no-lwr_img], -1, i_0, dat_0
     dat_0 = dat_0/i_0.exptime
     d_img = dat_0

; Second image     
;    Running difference
     if keyword_set(run) then begin
        aia_prep, fls[img_no], -1, i_1, dat_1
        dat_1 = dat_1/i_1.exptime
; Difference image    
        d_img = dat_1 - dat_0
     endif
     

;    Base difference 
     if keyword_set(base) then begin
        d_img = dat_0 - dat_init
     endif
          
; Remove pixels that we're not interested in.
     msk = d_img
     msk[arc_out] = 1.
     msk[arc_in] = 0.
     
     diff_img = d_img*msk
     tmpimg=diff_img^0.225
     if keyword_set(run) or keyword_set(base) then tmpimg = diff_img
     diff_img=tmpimg
     
; Define x and y zero points for plotting     
     x_pos = thrang[0]*180./!PI
     y_pos = r_in-1.
     

; Interpolate the annulus to a square plot
     img = shift(reverse(bilinear(diff_img, xpol, ypol)), 90./ang_step)

  ;Select the angular coverage of the image
     tmp=where(new_theta[*,0] ge thrang[0] and new_theta[*,0] le thrang[1])
     plotimg=img[tmp,*]
     
  ;Save the data for later
     if img_no eq lwr_img then begin
        projdata=dblarr(size(fls, /n_elements)-lwr_img,n_elements(img[*,0]),n_elements(img[0,*]))
        subprojdata=dblarr(size(fls, /n_elements)-lwr_img,n_elements(plotimg[*,0]),n_elements(plotimg[0,*]))
     endif
     print,img_no-lwr_img
     projdata[img_no-lwr_img,*,*]=img
     subprojdata[img_no-lwr_img,*,*]=plotimg
          
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;START PLOTTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     if keyword_set(plot) then begin
    
; Define image title      
     if keyword_set(run) then begin
        img_tit = '!5RDIFF image AIA/'+passband+'A, t = '+event.date+' '+num2str(anytim(i_1.date_d$obs, /yohkoh, /time_only, /sec, /trun))
     endif else begin
        if keyword_set(base) then begin
           img_tit = '!5BDIFF image AIA/'+passband+'A, t = '+event.date+' '+num2str(anytim(i_0.date_d$obs, /yohkoh, /time_only, /sec, /trun))
        endif else begin
           img_tit = '!5Raw image AIA/'+passband+'A, t = '+event.date+' '+num2str(anytim(i_0.date_d$obs, /yohkoh, /time_only, /sec, /trun))
        endelse
     endelse
  
; Sort out image scaling
     if keyword_set(run) or keyword_set(base) then begin
        case passband of
           '94': int_range = [-5., 5]
           '304': int_range = [-5., 5]
           '335': int_range = [-5., 5]
           '131': int_range = [-5., 5]
           '171': int_range = [-5., 5]
           '193': int_range = [-40, 50]
           '211': int_range = [-40, 50]
           else: int_range = [min(plotimg), max(plotimg)]
        endcase
     endif else begin
        int_range = [min(plotimg), max(plotimg)]
     endelse
   
; Setup Z-buffer for plotting
        if img_no eq lwr_img then begin
           set_plot, 'z'
           Device, Set_Resolution=[img_size[0], img_size[1]], set_pixel_depth=24, decomposed = 0
           ;!p.multi = 0
           !P.font=0
           chsize=1.4
           chthick=1.2
           
           loadct,0,/silent
           tvlct, r, g, b, /get
           tvlct,reverse(r),reverse(g),reverse(b)
           col=0
           bckg=255
           
           if keyword_set(run) or keyword_set(base) then begin
              loadct,0,/silent
              tvlct, r, g, b, /get
              col=255
              bckg=0
           endif
        endif

; Plot the image
        plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
                    ytitle = '!5Radius [arcsec from Sun centre]', $
                    charsize = chsize, title = img_tit, max = int_range[1], $
                    origin = [x_pos[0], y_pos], charthick = chthick, $
                    scale = [ang_step, res/ind.cdelt1], $
                    pos = [0.1, 0.1, 0.95, 0.95], min = int_range[0]
; Save the image as a PNG
              
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
        if not dir_exist(savepath+folder+passband+'/') then spawn,'mkdir '+savepath+folder+passband+'/'

        write_png, savepath + folder + passband +'/' + $
                   prefix + date + '_' + event.label+'_'+passband+'_'+postfix+'_'+img_strind+'.png', $
                   TVRD(/true),r,g,b
     endif                       ;END PLOTTING
    
  endfor  ;ENDFOR TIMESTEP LOOP
  
  set_plot, 'x'
  
  ;SAVE THE DATA!
  if not keyword_set(savename) then savname=savepath+'aia_deprojected_annulus_'+date+'_'+event.label+'_'+passband+'.sav' $
  else savname=savepath+savename

  if file_exist(savname) then begin 
     if keyword_set(force) then save,filename=savname,projdata,subprojdata,ring_width,thrang,$
                                     passband,ang_step,res,ind_arr,new_theta,rrang
  endif else begin
     save,filename=savname,projdata,subprojdata,ring_width,thrang,passband,ang_step,res,ind_arr,$
          new_theta,rrang
  endelse
  

  if keyword_set(annulus_data) then annulus_data=projdata
end
