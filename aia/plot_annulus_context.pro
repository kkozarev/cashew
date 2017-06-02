;+============================================================================
pro plot_annulus_context,projdata,annulus_info,ind_arr,event,lat_heights,x_good_lats,diff=diff
;Plot the annulus plot for context
  nsteps=n_elements(projdata[*,0,0])
  context_step=fix(nsteps/2.)
  index=ind_arr[0]
  x_deg_array = annulus_info.thcoords*180./!PI
  arlon=get_polar_angle(event)
  nlatmeas=n_elements(lat_heights)
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position=[0.2,0.2,0.9,0.9]
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  
  wdef,26,1200,800
  if keyword_set(diff) then $
     img=reform(projdata[context_step,*,*]) $
  else $
     img=reform(projdata[context_step,*,*]-projdata[context_step-1,*,*])
  plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
              ytitle = '!5Radial distance from Sun center [R!DS!N]', $
              title = 'Context annulus plot, AIA/'+annulus_info.wav+' '+event.date, max =50, $
              origin = [annulus_info.thcoords[0]*180./!PI,annulus_info.rcoords[0]], charthick = 1.2, charsize=3,$
              scale = [annulus_info.dtheta*180./!PI, annulus_info.dr], $
              pos = [0.16, 0.16, 0.95, 0.9], min = -50
  
;For the plotting, find the radial limits of the data, and overplot them.
  y_rsun_limits=aia_rad_height_limits(index,degarray=x_deg_array,/rsun)
  oplot,x_deg_array,y_rsun_limits,color=255,thick=2
  
;Overplot the radial measurement location
  yradlimit=aia_rad_height_limits(index,angle=arlon,/rsun)
  oplot,[arlon,arlon],[annulus_info.rcoords[0],yradlimit],thick=2
  
;Overplot the lateral measurements location
  for ii=0,nlatmeas-1 do $
     plots,x_deg_array,x_good_lats[ii,*],psym=1,thick=2,symsize=0.2
  
;Overplot the limb position
;oplot,thrang*180./!PI,[limb,limb],linestyle=2,thick=2,color=255 
  
;Save the overview plot
  write_png,event.annuluspath+replace_string(event.annplot.overviewplot_savename,'WAV',annulus_info.wav),$
            tvrd(/true),ct_rr,ct_gg,ct_bb
  wait,1
  wdel,26
end
;-============================================================================
