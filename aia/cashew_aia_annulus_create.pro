pro test_cashew_aia_annulus_create

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     labels=['120307_01','120405_01','151104_01','151104_02','151104_03']
     labels=['120806_01','140220_02']
     labels=['151104_01','151104_02','151104_03']
     labels=['140708_01','131119_01', '140225_01', '130501_01', '120915_01', '120526_01', '120405_01', '110809_01', '110808_01', '110804_01', '110327_01', '110307_02', '110211_02', '110125_01', '100613_01']
     labels=['151104_01','151104_02', '140217_01', '140108_01', '131207_01', '131107_01', '131105_01', '130517_01', '121007_01', '120728_01', '120307_01', '111020_01', '110904_01', '110515_01', '110308_01', '110224_01', '101231_01', '101103_02', '100612_02']
labels=['110511_01']
labels=['160316_01','151223_01','151219_01','151204_01','151109_01','151029_01','150920_01','150725_01','150625_01','150601_01','150512_01','150509_01','150421_01','150309_01','150303_01','150209_01','141205_01','141105_02','141105_01','140901_01']
labels=['120405_01']
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        cashew_aia_annulus_create,event,wav='193',/force,/remove_aec
        
     endfor
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     wavelengths=['193']
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           cashew_aia_annulus_create,event,wav=wavelength
        endfor
     endfor
  endif
end
;-============================================================================



;+============================================================================
pro cashew_aia_annulus_create, event, wav=wav, centerlat=centerlat, ring_width=ring_width,thrange=thrange,rrange=rrange,$
                               datascale=datascale,savename=savename,savepath=savepath,annulus_data=annulus_data,full=full,$
                               force=force,remove_aec=remove_aec,_extra=_extra
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
;   FILES - The list of files to read for the deprojected images.
;   WAV - Passband being studied. Required for labelling and to
;             identify data.
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

  date=event.date
  if not keyword_set(wav) then wav = '193'
  if not keyword_set(savepath) then savepath=event.annuluspath
  if not keyword_set(centerlat) then begin
     if event.arlon lt 0.0 then arlat=270.0+event.arlat $
     else arlat=90.0-event.arlat
  endif else arlat=centerlat
  
  
  maxrad=1350.                  ;maximum radial distance
  img_size = [1200., 800.]
  ang_step = 0.1                ; Angle step size (degree)
  res = 0.5                     ; Height step size (arcsec)
  
  
  tot_ang = 360.                ; Looking at 360 degrees in steps of ang_step
  if not keyword_set(thrange) then thrang=[arlat-90.,arlat+90.] else thrang=thrange
  if keyword_set(full) then thrang=[0.,360.]
  
  if thrang[0] lt 0. then thrang[0]=0.
  if thrang[1] gt 360. then thrang[1]=360.
  thrang*=!PI/180.0
  
; If differencing normal cadence images, use running window of N
; images.
  lwr_img = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CHECK FOR EXISTING SAVE FILE, ELSE LOAD DATA FROM FITS FILES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  infile=replace_string(event.annplot.savename,'WAV',wav)
  restored=0
  
  if (file_exist(savepath+infile)) and (not keyword_set(force)) then begin
     restore, savepath+infile
     restored=1                 ;Flag to know that we've restored a datacube file
     ;nsteps=n_elements(projdata[*,0,0])
     ;if not keyword_set(ring_width) then ring_width = 400.
                                ;Convert the radial range to arcsecs.
     ;if keyword_set(rrange) then begin
     ;   rrang=(rrange-1)*ind_arr.rsun_obs
     ;endif else begin
     ;   rrang=[0.0,ring_width]
     ;endelse
     
                                ; Define x and y zero points for plotting
     ;x_pos = thrang[0]*180./!PI
     ;y_pos = r_in-1.

                                ;  if keyword_set(base) then begin
                                ;     for img_no = 0, lwr_img - 1 do begin
                                ;        dat_tmp=projdata[img_no,*,*]
                                ;        if img_no eq 0 then dat_init = dat_tmp else dat_init = dat_init+dat_tmp
                                ;     endfor
                                ;     dat_init=reform(dat_init/(1.0*lwr_img))
                                ;  endif
     
  endif else begin
     if keyword_set(remove_aec) then $
        fls=aia_file_search(event.st,event.et,wav,missing=locmissing,event=event,/remove_aec) $
     else $
        fls=aia_file_search(event.st,event.et,wav,missing=locmissing,event=event)
     
     nsteps=size(fls,/n_elements)
     
     read_sdo, fls, ind_arr, /nodata
     
                                ;aia_load_data,event.st,event.et,wav,event=event,index=ind_arr,data=data,/nodata
     
; Width of ring (effectively distance from limb to edge of aperture in
; arcsec)
     outer_radius=1468.
     if not keyword_set(ring_width) then ring_width = 400. ;outer_radius-ind_arr.rsun_obs + 20
;Convert the radial range to arcsecs.
     if keyword_set(rrange) then begin
        rrang=(rrange-1)*ind_arr.rsun_obs
     endif else begin
        rrang=[0.0,ring_width]
     endelse
     
; Define inner and outer radius
     r_in = ind_arr[0].rsun_obs+rrang[0]    ; Inner radius
     r_out = ind_arr[0].rsun_obs + rrang[1] ; Outer radius
     if r_out gt maxrad then r_out=maxrad
     
                                ; Define x and y zero points for plotting
     x_pos = thrang[0]*180./!PI
     y_pos = r_in-1.
     

;DEBUG - use the already downloaded data!

; Get image coordinates
     aia_prep, fls[0], -1, ind, dat
     wcs = fitshead2wcs(ind)
     crd = wcs_get_coord(wcs)
     sz = size(dat)

;DEBUG - use the already downloaded data!
     
     if keyword_set(datascale) then datascale=[ang_step, res/ind.cdelt1]
     
; Get the height of each image pixel from Sun centre
     h = float(reform(sqrt((crd[0,*,*] - 0.)^2. + (crd[1,*,*] - 0.)^2.), sz[1], sz[2]))  
     
; Identify pixels that we're interested in
     arc_out = where((h gt (r_in-1.))) ; and (h le r_out-1.0))
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
     ;Select the angular coverage of the image
     thetaind = where(new_theta[*,0] ge thrang[0] and new_theta[*,0] le thrang[1])
     thcoords = new_theta[thetaind,0]
     new_r = rebin(transpose(a), tot_ang/ang_step, n_elements(a))
     rcoords = reform(new_r[0,*]*ind.cdelt1/ind.rsun_obs)
     
; Sort out new x- and y-coordinates
     p = wcs_get_pixel(wcs, [0,0])  
     xpol = new_r*cos(new_theta)+p[0]  
     ypol = new_r*sin(new_theta)+p[1]
     
;Create the base difference image here
                                ;   if keyword_set(base) then begin
                                ;      for img_no = 0, lwr_img - 1 do begin
                                ;         aia_prep, fls[img_no], -1, i_tmp, dat_tmp
                                ;         tmp=dat_tmp/i_tmp.exptime
                                ;         if img_no eq 0 then dat_init = tmp else dat_init = dat_init+tmp
                                ;      endfor
                                ;      dat_init=dat_init/(1.0*lwr_img)
                                ;   endif
     
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN TIME STEP LOOP!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
     for img_no = lwr_img, nsteps-1 do begin
        
        img_strind=strtrim(string(img_no),2)
        if img_strind lt 100 then img_strind='0'+img_strind
        if img_strind lt 10 then img_strind='0'+img_strind
        
        if restored then begin
           plotimg=reform(projdata[img_no-lwr_img,*,*])
                                ;if keyword_set(base) then begin
                                ;   plotimg=reform(plotimg-dat_init)
                                ;   endif else begin
                                ;      if keyword_set(run) then begin
                                ;        plotimg=reform(projdata[img_no,*,*]-plotimg)
                                ;     endif
                                ;   endelse
        endif else begin
           
; First image
           aia_prep, fls[img_no-lwr_img], -1, i_0, dat_0
           dat_0 = dat_0/i_0.exptime
           d_img = dat_0

; Second image
; Remove pixels that we're not interested in.
           msk = dat_0
           msk[arc_out] = 1.
           msk[arc_in] = 0.
           raw_img=dat_0*msk
           
; Interpolate the annulus to a rectangular area
           img = shift(reverse(bilinear(raw_img, xpol, ypol)), 90./ang_step)
           plotimg=img[thetaind,*]
           
                                ;Save the data for later
           if img_no eq lwr_img then begin
              projdata=dblarr(size(fls, /n_elements)-lwr_img,n_elements(plotimg[*,0]),n_elements(plotimg[0,*]))
           endif
           projdata[img_no-lwr_img,*,*]=plotimg
           
        endelse                 ;END OF RESTORED IF/ELSE STATEMENT
        
     endfor                     ;ENDFOR TIMESTEP LOOP
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
     
     ;SAVE THE DATA!
     if not keyword_set(savename) then savname=savepath+replace_string(event.annplot.savename,'WAV',wav) $
     else savname=savepath+savename
     dr=rcoords[1]-rcoords[0]
     dth=thcoords[1]-thcoords[0]
     annulus_info={wav:wav,thrange:minmax(thcoords),rrange:minmax(rcoords),$
                   dtheta:dth,dr:dr,thcoords:thcoords,rcoords:rcoords}
     if (not file_exist(savname)) or keyword_set(force) then $ 
        save,filename=savname,projdata,annulus_info,ind_arr
  endelse
  annulus_data=projdata
  
end
;-============================================================================
