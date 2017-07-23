pro test_aia_update_source_position_manual
  ;Test the procedure select_arlat
  event=load_events_info(label='151104_01')
  aia_inspect_source_position_manual, event
end


pro aia_inspect_source_position_manual, event, reset=reset
  ;KEYWORDS:
  ;reset - resets the local json file by copying from the global json with all events.
  
  !p.multi=0
  !p.background=255
  !p.color=0
  
  wav='193'
  date=event.date
  savepath=event.annuluspath
  datapath=savepath
  fname=replace_string(event.annplot.savename,'WAV',wav)
  restore, datapath+fname
  
  ;Plot the annulus plot for context
  nsteps=n_elements(projdata[*,0,0])
  context_step=fix(nsteps/2.)
  index=ind_arr[0]
  y_rsun_array = annulus_info.rcoords
  x_deg_array = annulus_info.thcoords*180./!PI
  y_rsun_limits=aia_rad_height_limits(ind_arr[0],degarray=x_deg_array,/rsun)

  arlon=get_polar_angle(event)
  newarlon=arlon
  arlonind=min(where(x_deg_array ge arlon))
  limb=1                        ;Limb position in Rsun
  limbind=min(where(y_rsun_array ge limb))
  rel_rad_positions=[0.1,0.51]
  lat_heights=(limb+rel_rad_positions*(max(y_rsun_limits)-limb))
  
;For the tangential measurements, get a map of the good tangential
;pixel values - those where there is data. This will only be relevant
;if taking tangential measurements high up.
  x_good_lats=aia_deg_tan_limits(x_deg_array,y_rsun_array,y_rsun_limits,lat_heights)


;Show a movie first
  print,''
  valid=0
  uinput=''
  read,uinput,prompt='Do you want to see a context movie of the event? (y/n)?:'
  if uinput eq 'y' or uinput eq 'n' then valid=1
  if uinput eq 'y' then plot_annulus_movie,projdata,annulus_info,ind_arr,event,lat_heights,x_good_lats


;Find the Y-radial outward limit/edge for which there's data
  yradlimit=aia_rad_height_limits(ind_arr[0],angle=arlon,/rsun)
  yradlimind=max(where(y_rsun_array le yradlimit))
  
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
  img=reform(projdata[context_step,*,*]-projdata[context_step-1,*,*])
  
  replotcontext:
  plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
              ytitle = '!5Radial distance from Sun center [R!DS!N]', $
              title = 'Context annulus plot, AIA/'+annulus_info.wav+' '+event.date, max =50, $
              origin = [annulus_info.thcoords[0]*180./!PI,annulus_info.rcoords[0]], $
              charthick = 1.2, charsize=3,$
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
  
  valid=0
  uinput=''
  while valid eq 0 do begin
     print,''
     read,uinput,prompt='Do you want to select a different radial position? Press "y" for yes, "n" for no: '
     if uinput eq 'y' or uinput eq 'n' then valid=1
     
  endwhile
  if uinput eq 'y' then begin
     print,''
     print,'Select the new Polar Angle location'
     cursor,newarlon,y,/down
     ;Regularize the choice to a grid value
     regind=where(x_deg_array-newarlon gt 0)
     reglon=x_deg_array[regind[0]-1]
     print,regind[0]-1
     newarlon=reglon
     arlon=newarlon
     goto,replotcontext
  endif
  
  if newarlon ne get_polar_angle(event) then begin
     valid=0
     uinput=''
     while valid eq 0 do begin
        print,''
        read,uinput,prompt='Really save the new Polar Angle as default? Press "y" for yes, "n" for no: '
        if uinput eq 'y' or uinput eq 'n' then valid=1
     endwhile
     if uinput eq 'y' then begin
        if event.arlon lt 0.0 then newarlat=newarlon-270. $
        else newarlat=90.-newarlon
        tmp=hel2arcmin(newarlat,event.arlon,date=event.date)*60.
        ;;;tmp=hel2arcmin(newarlat,newarlon,date=event.date)*60.
        coordx=fix(tmp[0])
        coordy=fix(tmp[1])
        
        if keyword_set(reset) then begin
           update_events_info_json,GETENV('CORWAV_TRUNK')+'dat/events.json',$
                                   GETENV('CORWAV_TRUNK')+'dat/events.json',$
                                   label=event.label,coordx=coordx,coordy=coordy
        endif else begin
           update_events_info_json,event.json_savename,event.json_savename,$
                                   label=event.label,coordx=coordx,coordy=coordy
        endelse
        print,'Saved updated JSON event file.'
     endif
  endif
  wdel,26
end



