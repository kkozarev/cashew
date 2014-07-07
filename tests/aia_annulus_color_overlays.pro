pro test_aia_annulus_color_overlays
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     rrange=[1.11,1.34]
     event=load_events_info(label='test')
     aia_annulus_color_overlays,event,wave=wav,rrange=rrange ;,/interactive
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     rrange=[1.1,1.34]
;n_elements(events)-1
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_annulus_overlays,event,wave=wavelength,rrange=rrange ;,/interactive
        endfor
     endfor
  endif

end



pro aia_annulus_color_overlays,event,datapath=datapath,savepath=savepath,thrange=thrange,interactive=interactive,wave=wave,rrange=rrange
;PURPOSE:
;Procedure to analyze the speeds of radial and lateral expansion of a
;wave and/or a filament.
;Uses output from aia_annulus_plot.pro, a procedure deprojecting AIA
;data onto a rectangular grid, where the X-axis is latitude along the
;limb, and the Y-axis is radial distance.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      EVENT - load an event information structure
;
;KEYWORDS:
;      DATAPATH:
;      SAVEPATH:
;      THRANGE:
;      INTERACTIVE:
;      WAVE:
;
;OUTPUTS:
;
; ;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 08/07/2013
;   2013/11/19, Kamen Kozarev - Integrated the event structure,
;               updated and streamlined the procedure

;Restore the file with the deprojected data
  date=event.date
  label=event.label
  
  if not keyword_set(wave) then wav='193' else wav=wave
  if not keyword_set(savepath) then savepath=event.savepath+'annulusplot/' ;'./'
  if not keyword_set(datapath) then datapath=savepath ;'./'
  
  fname='aia_deprojected_annulus_'+date+'_'+event.label+'_'+wav+'.sav'
  restore, datapath+fname
  
  nsteps=n_elements(projdata[*,0,0])
  ncols=n_elements(projdata[0,*,0])
  nrows=n_elements(projdata[0,0,*])
  projdata=0.0
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  
  if not keyword_set(interactive) then begin
     nsteps=n_elements(subprojdata[*,0,0])
     ncols=n_elements(subprojdata[0,*,0])
     nrows=n_elements(subprojdata[0,0,*])
  endif
  
;Pixel coordinates in arcseconds from the center of the sun. How do I convert them to km?
  y_arcsec_array=res/ind_arr[0].cdelt1*findgen(nrows)+r_in
  y_rsun_array=y_arcsec_array/ind_arr[0].rsun_obs
  
;The X-angular array (distance along the limb from the pole).
  if keyword_set(interactive) then x_deg_array=findgen(ncols)*ang_step $
  else x_deg_array=findgen(ncols)*ang_step+thrang[0]*180./!PI
  
;The relative times
  DT=anytim(ind_arr[1].date_obs)-anytim(ind_arr[0].date_obs) ;The imaging cadence

  rad_times=findgen(nsteps)*dt/60./60.
  lat_times=findgen(nsteps)*dt/60.
  
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position[3]=0.8
  !p.position[1]=0.1
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  wdef,0,1200,900
  
;pick the time steps for which to create the RGB image
  s2p=fix([nsteps/6.,nsteps/3.,nsteps*2./3.])
  print,s2p
  
  img=fltarr(ncols,nrows,3)  
  img[*,*,0]=reform(subprojdata[s2p[0],*,*]-subprojdata[s2p[0]-1,*,*])
  img[*,*,1]=reform(subprojdata[s2p[1],*,*]-subprojdata[s2p[1]-1,*,*])
  img[*,*,2]=reform(subprojdata[s2p[2],*,*]-subprojdata[s2p[2]-1,*,*])

     plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 title = 'Annulusplot, AIA/'+wav+' '+date, min = -10, max = 50, $
                 origin = [thrang[0]*180./!PI,r_in], charthick = 1.2, charsize=2,$
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.1, 0.1, 0.95, 0.95], font_name='Hershey 5'
stop

fname='annplot_'+date+'_'+label+'_timecolor_plot.png'
write_png,savepath+fname,tvrd(/true)

  
end
