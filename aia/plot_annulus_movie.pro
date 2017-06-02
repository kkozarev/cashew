;+============================================================================
pro plot_annulus_movie,projdata,annulus_info,ind_arr,event,lat_heights,goodlats,diff=diff
;Plot the annulus plot for context
  nsteps=n_elements(projdata[*,0,0])
  index=ind_arr[0]
  x_deg_array = annulus_info.thcoords*180./!PI
  arlon=get_polar_angle(event) 
  time=get_time(ind_arr.date_obs)
  nlatmeas=n_elements(lat_heights)
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position=[0.2,0.2,0.9,0.9]
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  
  
  y_rsun_limits=aia_rad_height_limits(index,degarray=x_deg_array,/rsun)
  yradlimit=aia_rad_height_limits(index,angle=arlon,/rsun)
  
  wdef,26,1200,800
  for nplot=0,0 do begin
     for tt=0,nsteps-1 do begin
        if keyword_set(diff) then img=reform(projdata[tt,*,*]) $
        else $
           img=reform(projdata[tt,*,*]-projdata[0,*,*])
        plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                    ytitle = '!5Radial distance from Sun center [R!DS!N]', $
                    title = 'Annulus data, AIA/'+annulus_info.wav+' '+time[tt].cashew_time, max =50, $
                    origin = [annulus_info.thcoords[0]*180./!PI,annulus_info.rcoords[0]], charthick = 1.2, charsize=3,$
                    scale = [annulus_info.dtheta*180./!PI, annulus_info.dr], $
                    pos = [0.16, 0.16, 0.95, 0.9], min = -50
        
;For the plotting, find the radial limits of the data, and overplot them.
        oplot,x_deg_array,y_rsun_limits,color=255,thick=2
        
;Overplot the radial measurement location
        oplot,[arlon,arlon],[annulus_info.rcoords[0],yradlimit],thick=2
        
;Overplot the lateral measurements location
        for ii=0,nlatmeas-1 do $
           plots,x_deg_array,goodlats[ii,*],psym=1,thick=2,symsize=0.2
        wait,0.1
     endfor
  endfor
  wdel,26
;Overplot the limb position
                                ;oplot,thrang*180./!PI,[limb,limb],linestyle=2,thick=2,color=255 
end
;-============================================================================
