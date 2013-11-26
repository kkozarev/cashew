pro test_aia_annulus_analyze
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     event=load_events_info(label='110511_01')
     aia_annulus_analyze,event,wave=wav ;,/interactive
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
;n_elements(events)-1
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_annulus_analyze,event,wave=wavelength ;,/interactive
        endfor
     endfor
  endif


end



pro aia_annulus_analyze,event,datapath=datapath,savepath=savepath,thrange=thrange,interactive=interactive,wave=wave
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
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 08/07/2013
;   2013/11/19, Kamen Kozarev - Integrated the event structure,
;               updated and streamlined the procedure

;Restore the file with the deprojected data
  tmp=strsplit(event.date,'/',/extract)
  date=tmp[0]+tmp[1]+tmp[2]

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
  rad_times=findgen(nsteps)*24./60./60.
  lat_times=findgen(nsteps)*24./60.
  
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position[3]=0.8
  !p.position[1]=0.1
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  wdef,0,1200,800
  if keyword_set(interactive) then begin
  
     img=reform(projdata[fix(nsteps/2),*,*]-projdata[fix(nsteps/2)-1,*,*])
     plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 title = 'Annulusplot, AIA/'+wav+' '+date, max = 50, $
                 origin = [0,0], charthick = 1.2, charsize=4, $
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.1, 0.1, 0.95, 0.95], min = -40
  endif else begin
     
     img=reform(subprojdata[fix(nsteps/3),*,*]-subprojdata[fix(nsteps/3)-1,*,*])
     plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 title = 'Annulusplot, AIA/'+wav+' '+date, max = 50, $
                 origin = [thrang[0]*180./!PI,r_in], charthick = 1.2, charsize=4,$
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.1, 0.1, 0.95, 0.95], min = -40, font_name='Hershey 5'
  endelse
  
  
  htlimits=rad_height_limits(degarray=x_deg_array)
  oplot,x_deg_array,htlimits,color=255,thick=2;,/data
  
  limb=ind_arr[0].rsun_obs ;Limb position in arcsec
  limbind=min(where(y_arcsec_array ge limb))
  ;oplot,thrang*180./!PI,[limb,limb],linestyle=2,thick=2,color=255 ;Overplot the limb position
  
  if not keyword_set(thrange) then begin
     if keyword_set(interactive) then begin
        print,''
        print,'On the image, select the MIN(Theta) to include'
        cursor,x,y,/down,/data
        oplot,[x,x],[0,nrows-1],thick=2
        thrang[0]=x
        print,x
        
        print,''
        print,'On the image, select the MAX(Theta) to include'
        cursor,x,y,/down,/data
        oplot,[x,x],[0,nrows-1],thick=2
        thrang[1]=x
        print,x
     endif    
  endif else begin
     thrang=thrange
  endelse
  
  if not keyword_set(interactive) then begin
     if not keyword_set(centerlat) then begin
        if event.arlon lt 0.0 then arlat=270.0+event.arlat $
        else arlat=90.0-event.arlat
     endif else arlat=centerlat
     
     ;Find the radial outward limit/edge for which there's data
     yradlimit=rad_height_limits(angle=arlat)
     yradlimind=min(where(y_arcsec_array ge yradlimit))
     
     oplot,[arlat,arlat],[ind_arr[0].rsun_obs,yradlimit],thick=2
     
     ;The index of the AR latitude
     arxcentind=min(where(x_deg_array ge arlat))
     
     lat_heights=(limb+[0.25,0.55,0.85]*(yradlimit-limb))/ind_arr[0].rsun_obs
     nlatmeas=n_elements(lat_heights)
     htind=intarr(nlatmeas)
     goodlats=fltarr(nlatmeas,ncols)-10.
     for ii=0,nlatmeas-1 do begin
        ;Find the indices of the lateral measurement heights
        htind[ii]=min(where(y_rsun_array ge lat_heights[ii]))
        
        ;Find where the lat_heights are larger than the radial limits 'htlimits'.
        tt=where(htlimits-lat_heights[ii]*ind_arr[0].rsun_obs ge 0.)
        if tt[0] ne -1 then goodlats[ii,tt]=y_arcsec_array[htind[ii]]
        plots,x_deg_array,goodlats[ii,*],psym=1,thick=1,symsize=0.1
        ;Overplot the lateral measurements location
        ;oplot,thrang*180./!PI,[y_arcsec_array[htind[ii]],y_arcsec_array[htind[ii]]],thick=2
     endfor
     
    
  endif else begin
     print,''
     print,'On the image, select the latitude of the AR center:'
     cursor,x,y,/down,/data
     oplot,[x,x],[0,nrows-1],thick=2
     print,x
     arcenter=x
     
     uinput=0.0
     while uinput le 0.0 or uinput gt 5 do begin
        read,uinput,prompt='How many heights for lateral measurements (5 max)? '
        wait,0.1
     endwhile
     nlatmeas=uinput
     
     ;Arrays that hold the measurement heights/indices thereof
     lat_heights=dblarr(nlatmeas)
     htind=dblarr(nlatmeas)
     goodlats=fltarr(nlatmeas,ncols)-10.

     for ii=0,nlatmeas-1 do begin
        print,''
        print,'Select height for lateral measurement #'+strtrim(string(ii+1),2)
        cursor,x,y,/down,/data
        print,y
        ;oplot,[0,ncols-1],[y,y],thick=2
        lat_heights[ii]=y
        htind[ii]=min(where(yangular_array ge lat_heights[ii]))

        ;Find where the lat_heights are larger than the radial limits 'htlimits'.
        tt=where(htlimits-lat_heights[ii]*ind_arr[0].rsun_obs ge 0.)
        if tt[0] ne -1 then goodlats[ii,tt]=y_arcsec_array[htind[ii]]
        plots,x_deg_array,goodlats[ii,*],psym=1,thick=1,symsize=0.1

     endfor
     
  endelse
;Save the overview plot
  write_png,savepath+'annplot_'+date+'_'+event.label+'_'+wav+'_overview_plot.png',tvrd(/true),ct_rr,ct_gg,ct_bb
  wdel,0

  
  if keyword_set(interactive) then begin
     subfovind=where(new_theta[*,0] ge thrang[0]*!PI/180.0 and new_theta[*,0] le thrang[1]*!PI/180.0)
     plotimg=img[subfovind,*]
     
;  plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
;              ytitle = '!5Radius [arcsec from Limb]', $
;              charsize = 1.5, title = 'AIA deprojected image', max = 50, $
;              origin = [thrang[0]*180./!PI,r_in], charthick = 1.1, $
;              scale = [ang_step, res/ind_arr[0].cdelt1], $
;              pos = [0.1, 0.1, 0.95, 0.95], min = -40
     
     
;The index of the AR center in the SUB-FOV images
     arxcentind=min(where(new_theta[*,0] ge (arlat-thrang[0])*!PI/180.0))

;Get just the data to analyze
     subfovdata=projdata[*,subfovind,*]
     subncols=n_elements(subfovdata[0,*,0])
     andata=projdata[*,subfovind,htind]
  endif else begin
     subfovdata=subprojdata
     subncols=ncols
     andata=subprojdata[*,*,htind]
  endelse
  
  
;Set the Lateral data
  tmp=subfovdata[*,*,htind]
  for ii=0,nlatmeas-1 do begin
     tt=where(goodlats[ii,*] lt 0.)
     if tt[0] ne -1 then tmp[*,tt,ii]=-1000.
  endfor
  left_lat_data=reform(tmp[*,0:arxcentind-1,*])
  right_lat_data=reform(tmp[*,arxcentind:subncols-1,*])
  ;left_lat_data=reform(subfovdata[*,0:arxcentind-1,htind])
  ;right_lat_data=reform(subfovdata[*,arxcentind:subncols-1,htind])
  
 
  ;Save the Lateral data into a structure 
  lat_data={$
           left:{data:left_lat_data,bdiff:left_lat_data,winind:1,$
                 savename:'annplot_'+date+'_'+event.label+'_'+wav+'_lateral_left.png',$
                 plotinfo:replicate({p:!P, x:!X, y:!Y},nlatmeas),$
                 maxinds:intarr(nlatmeas,nsteps)},$
           right:{data:right_lat_data,bdiff:right_lat_data,winind:2,$
                  savename:'annplot_'+date+'_'+event.label+'_'+wav+'_lateral_right.png',$
                  plotinfo:replicate({p:!P, x:!X, y:!Y},nlatmeas),$
                  maxinds:intarr(nlatmeas,nsteps)},$
           time:lat_times/60.0,$
           fitrange:[0,0],$  ;The indices of the start/end fitting times
           scale:[24./60.,ang_step],origin:[0,0],$
           xtitle:'Minutes since start',ytitle:'Degrees from AR center',$
           winsize:[1200,800],multi:[0,nlatmeas,1]$
           }
  
  

  tmp=subfovdata[*,arxcentind-2:arxcentind+2,*]
  tmp=total(tmp,2)
  rad_data={$
           data:tmp,bdiff:tmp,rdiff:tmp,$
           scale:[24./60./60.,res/ind_arr[0].cdelt1/ind_arr[0].rsun_obs],$
           origin:[0,y_rsun_array[0]],winsize:[800,600],multi:0,winind:3,$
           difforigin:[0,y_rsun_array[htind[0]]],fitrange:[0,0],$
           plotinfo:{p:!P, x:!X, y:!Y},maxinds:intarr(n_elements(tmp[*,0])),$
           xtitle:'Hours since start',ytitle:'Rsun from Sun center',$
           savename:'annplot_'+date+'_'+event.label+'_'+wav+'_radial.png',$
           time:rad_times/3600.0$
           }
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;JMAP PLOTTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Radial Data!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  !p.multi=rad_data.multi
;Plot the Radial positions
  wdef,rad_data.winind,rad_data.winsize[0],rad_data.winsize[1]
  
  tmpdata=rad_data.data
  base=total(tmpdata[0:4,*,*],1)/5.0
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
;DESPIKE THE IMAGE
  tmp=despike_gen(tmpdata)
  rad_data.bdiff=tmp
  
  
  plot_image,rad_data.bdiff[*,htind[0]:yradlimind],scale=rad_data.scale,$
             min=-40,max=50,charsize=1.6,$
             charthick=1.2,origin=rad_data.difforigin, $
             title='AIA/'+wav+' BDiff Radial Positions !C Start at '+ind_arr[0].date_obs,$
             font_name='Hershey 5',$
             xtitle=rad_data.xtitle,ytitle=rad_data.ytitle
  
  ;Fit the RADIAL maxima and overplot them...
  jmap_find_maxima,rad_data.bdiff,rad_times,y_rsun_array,$
                   mymaxima=mymaxima,allmaxima=allmaxima,$
                   yrange=[1.1,1.35]
  rad_data.maxinds=reform([mymaxima[0,*].ind])
  loadct,8,/silent
  oplot,rad_times,y_rsun_array[rad_data.maxinds],psym=1,color=200,thick=4,symsize=2
  
  print,''
  print,'Select starting point:'
  cursor,x,y,/down,/data
  plots,x,y,psym=5,symsize=2,thick=2,color=100
  oplot,[x,x],[y_rsun_array[htind[0]],y_rsun_array[yradlimind]],color=255
  sp=min(where((rad_times-x) gt 0.0))
  
  print,'Select ending point:'
  cursor,x,y,/down,/data
  plots,x,y,psym=5,symsize=2,thick=2,color=200
  oplot,[x,x],[y_rsun_array[htind[0]],y_rsun_array[yradlimind]],color=255
  ep=min(where((rad_times-x) gt 0.0))
  
  ;Search for the edges of the wave
  tmp={val:0.0D,ind:0L}
  wave_frontedge=replicate(tmp,ep-sp+1)
  wave_backedge=replicate(tmp,ep-sp+1)
  
  ;Record the start/end time indices in the data structures
  lat_data.fitrange=[sp,ep]
  rad_data.fitrange=[sp,ep]
  
  for ii=sp,ep do begin
;Find the front edge of the wave
     y=reform(rad_data.bdiff[ii,mymaxima[0,ii].ind:*])
     tmp=min(where(y le 0.2*max(y)))
     wave_frontedge[ii-sp].val=y_rsun_array[mymaxima[0,ii].ind+tmp]
     wave_frontedge[ii-sp].ind=mymaxima[0,ii].ind+tmp
     
;Find the back edge of the wave
     y=reform(rad_data.bdiff[ii,0:mymaxima[0,ii].ind])
     tmp=max(where(y le 0.1*max(y)))
     wave_backedge[ii-sp].val=y_rsun_array[tmp]
     wave_backedge[ii-sp].ind=tmp
     oplot,[rad_times[ii],rad_times[ii]],$
           [wave_backedge[ii-sp].val,wave_frontedge[ii-sp].val],$
           color=200,thick=2
  endfor

;Do second order polynomial fitting for the maximum
  print,''
  print,'Fitting a second-order polynomial to the wave peak positions...'
  raddist=reform(y_rsun_array[mymaxima[0,sp:ep].ind])
  times=rad_times[sp:ep]*3600.
  bootstrap_sdo,raddist,times,fit_line, p1, p2, p3, s1, s2, s3
  print,''
  print,''  
  
  wave_fits=replicate({main:0.0D,min:0.0D,max:0.0D},n_elements(times))
  wave_fits.main=p1[0] + p2[0] * (times)+ 0.5 * p3[0] * (times)^2
  
  oplot,rad_times[sp:ep],wave_fits.main,thick=3
  loadct,0,/silent
  
  ;Print out the results of the fitting
;initial height
  r0=y_rsun_array[mymaxima[0,sp].ind]
  tmpstr='R!D0!N = '+strtrim(string(r0,format='(f9.5)'),2)+' +/-'+strtrim(string(s1[0],format='(f9.5)'),2)+' R!Ds!N'
  print,tmpstr
  xyouts,!P.position[0]+0.12,!P.position[3]-0.04,tmpstr,/norm,charsize=2,color=255
  
;final height
  rf=y_rsun_array[mymaxima[0,ep].ind]
  tmpstr='R!D1!N = '+strtrim(string(rf,format='(f9.5)'),2)+' R!Ds!N'
  print,tmpstr
  xyouts,!P.position[0]+0.12,!P.position[3]-2*0.04,tmpstr,/norm,charsize=2,color=255
  
;initial speed
  v0=p2[0]*RSUN
  errv0=s2[0]*RSUN
  tmpstr='V!D0!N = '+strtrim(string(v0,format='(f9.2)'),2)+' +/-'+strtrim(string(errv0,format='(f9.2)'),2)+' km/s'
  print,tmpstr
  xyouts,!P.position[0]+0.12,!P.position[3]-3*0.04,tmpstr,/norm,charsize=2,color=255
  
;final speed
  tmp=p2[0]+p3[0]*(rad_times[sp:ep]-rad_times[sp])*3600.
  vf=tmp[n_elements(tmp)-1]*RSUN
  tmpstr='V!D1!N = '+strtrim(string(vf,format='(f9.2)'),2)+ ' km/s'
  print,tmpstr
  xyouts,!P.position[0]+0.12,!P.position[3]-4*0.04,tmpstr,/norm,charsize=2,color=255
  
;acceleration
  accel=p3[0]*RSUN*1000.0
  erraccel=s3[0]*RSUN*1000.0
  tmpstr='a = '+strtrim(string(accel,format='(f9.2)'),2)+' +/-'+strtrim(string(erraccel,format='(f9.2)'),2)+' m/s!U2!N'
  print,tmpstr
  print,''
  xyouts,!P.position[0]+0.12,!P.position[3]-5*0.04,tmpstr,/norm,charsize=2,color=255 
  

  write_png,savepath+rad_data.savename,tvrd(/true),ct_rr,ct_gg,ct_bb
  rad_data.plotinfo.p=!P
  rad_data.plotinfo.x=!X
  rad_data.plotinfo.y=!Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Left Lateral Data!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  !p.multi=lat_data.multi
  tmpdata=lat_data.left.data
  nbase=5
  base=total(tmpdata[0:nbase-1,*,*],1)/(nbase*1.0)
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
  lat_deg_array=findgen(arxcentind)*ang_step ;The lateral angle positions
  
;Plot the Left Lateral positions
  wdef,lat_data.left.winind,lat_data.winsize[0],lat_data.winsize[1]
  for rr=0,nlatmeas-1 do begin
     tmp=reform(tmpdata[*,*,rr])
     ;REVERSE THE IMAGE COLUMNS SO THE AR IS ALWAYS IN THE BOTTOM
     tmp=reverse(tmp,2)
     ;DESPIKE THE IMAGE
     tmp=despike_gen(tmp)
     tmpdata[*,*,rr]=tmp
     height=strtrim(string(lat_heights[rr],format='(f5.3)'),2)
     lat_data.left.bdiff[*,*,rr]=tmp
     glind=where(goodlats[rr,*] ge 0.0)
     
    ;The middle height index
     midind=fix(median(indgen(nlatmeas)))
     if rr eq midind then $
        imgtit='AIA/'+wav+' Left Lateral Positions | Start at '+$
               ind_arr[0].date_obs+'!C R = '+height+' R!Ds!N' $
     else imgtit='R = '+height+' R!Ds!N'
     
     
     plot_image,tmp,scale=lat_data.scale,min=-40,max=50,charsize=4,charthick=1.2,$
                origin=lat_data.origin, $
                title=imgtit, font_name='Hershey 5',$
                xtitle=lat_data.xtitle,ytitle=lat_data.ytitle
     ;To restore the plot information and overplot on them, do
     ;!p=lat_data.left.plotinfo[rr].p &
     ;!x=lat_data.left.plotinfo[rr].x &
     ;!y=lat_data.left.plotinfo[rr].y
     lat_data.left.plotinfo[rr].p=!P
     lat_data.left.plotinfo[rr].x=!X
     lat_data.left.plotinfo[rr].y=!Y
     
  ;Fit the LATERAL maxima and overplot them...
  jmap_find_maxima,reform(lat_data.left.bdiff[*,*,rr]),lat_times,lat_deg_array,$
                   mymaxima=mymaxima
  lat_data.left.maxinds[rr,*]=reform([mymaxima[0,*].ind])
  loadct,8,/silent
  oplot,lat_times,lat_deg_array[lat_data.left.maxinds[rr,*]],psym=1,color=200,thick=4,symsize=2
  loadct,0,/silent
  endfor
  
  write_png,savepath+lat_data.left.savename,tvrd(/true),ct_rr,ct_gg,ct_bb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Right Lateral Data!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  !p.multi=lat_data.multi
  tmpdata=lat_data.right.data
  base=total(tmpdata[0:4,*,*],1)/5.0
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
;testdata[tt,*,*]=reform(testdata[tt,*,*]-base)
;    testdata[tt,*,*]=reform(testdata[tt,*,*]-testdata[0,*,*])

;Plot the Right Tangential positions
  wdef,lat_data.right.winind,lat_data.winsize[0],lat_data.winsize[1]
  for rr=0,nlatmeas-1 do begin
     tmp=reform(tmpdata[*,*,rr])
     ;DESPIKE THE IMAGE
     tmp=despike_gen(tmp)
     lat_data.right.bdiff[*,*,rr]=tmp
     height=strtrim(string(lat_heights[rr],format='(f5.3)'),2)
     
    ;The middle
     midind=fix(median(indgen(nlatmeas)))
     if rr eq midind then $
        imgtit='AIA/'+wav+' Right Lateral Positions | Start at '+ind_arr[0].date_obs+'!C R = '+height+' R!Ds!N' $
     else imgtit='R = '+height+' R!Ds!N'
     
     plot_image,tmp,scale=lat_data.scale,min=-40,max=50,charsize=4,charthick=1.2,origin=lat_data.origin,$
                title=imgtit,font_name='Hershey 5',$
                xtitle=lat_data.xtitle,ytitle=lat_data.ytitle
     ;if rr eq 0 then rightplotinfo=replicate({p:!P, x:!X, y:!Y},nlatmeas)
     lat_data.right.plotinfo[rr]={p:!P, x:!X, y:!Y}
     ;To restore the plot information and overplot on them, do
     lat_data.right.plotinfo[rr].p=!P
     lat_data.right.plotinfo[rr].x=!X
     lat_data.right.plotinfo[rr].y=!Y
     
  ;Fit the LATERAL maxima and overplot them...
     jmap_find_maxima,reform(lat_data.right.bdiff[*,*,rr]),lat_times,lat_deg_array,$
                      mymaxima=mymaxima
     lat_data.right.maxinds[rr,*]=reform([mymaxima[0,*].ind])
     loadct,8,/silent
     oplot,lat_times,lat_deg_array[lat_data.right.maxinds[rr,*]],psym=1,color=200,thick=4,symsize=2
     loadct,0,/silent
     
  endfor

  write_png,savepath+lat_data.right.savename,tvrd(/true),ct_rr,ct_gg,ct_bb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    
    save,filename='radlat_data.sav',rad_data,lat_data,ind_arr,nsteps,ncols,nrows,wav,htind,yradlimind
 end




function rad_height_limits,angle=angle,degarray=degarray
;Calculate the limits to which the radial heights are physical in the
;images. This is useful when creating the j-maps

;If the keyword angle is supplied (in degrees from solar north),
;return the height for that angle.
;You can optionally supply the degrees array (again in degrees)
;Returns the height limit in arcsec

;Angle array at 1 arcmin resolution
  degarr=findgen(360.*60.)/60.*!PI/180.
                                         
  ndeg=n_elements(degarr)
;Corresponding height array
  htarr=fltarr(ndeg)
  
;Pick some radius of the circle and the size of the square
  n=1170.                       ;n is half the side of the square, in arcsec
  r=1370.                       ;r is the radius of the outer circle of good data
  
;calculate special degrees
  degs=fltarr(8)
  anr=acos(n/r)
  degs[0]=anr
  degs[1]=!pi/2.-anr
  degs[2]=!pi/2.+anr
  degs[3]=!pi-anr
  degs[4]=!pi+anr
  degs[5]=(3./2.)*!pi-anr
  degs[6]=(3./2.)*!pi+anr
  degs[7]=(2.)*!pi-anr

;Find their corresponding indices in the angle array
  dginds=intarr(8)
  for i=0,7 do dginds[i]=min(where(degarr ge degs[i]))
  
;Calculate the heights in the 12 different regions
  htarr[0:dginds[0]-1]=n/cos(degarr[0:dginds[0]-1]) ; region I
  htarr[dginds[0]:dginds[1]-1]=r
  htarr[dginds[1]:dginds[2]-1]=n/sin(degarr[dginds[1]:dginds[2]-1]) ; regions II and III
  htarr[dginds[2]:dginds[3]-1]=r
  htarr[dginds[3]:dginds[4]-1]=n/abs(cos(degarr[dginds[3]:dginds[4]-1])) ; regions IV and V
  htarr[dginds[4]:dginds[5]-1]=r
  htarr[dginds[5]:dginds[6]-1]=n/abs(sin(degarr[dginds[5]:dginds[6]-1])) ; regions VI and VII
  htarr[dginds[6]:dginds[7]-1]=r
  htarr[dginds[7]:ndeg-1]=n/abs(cos(degarr[dginds[7]:ndeg-1])) ; region VIII
  
  if keyword_set(angle) then begin
     ang=angle
     while ang lt 0. do ang+=360.
     while ang gt 360. do ang-=360.
     ang=ang*!pi/180.     
     angind=min(where(degarr ge ang))
     tmp=htarr[angind]
     
     return, tmp[0]
  endif
  
  if keyword_set(degarray) then begin
     numinds=n_elements(degarray)
     beg=min(where(degarr ge degarray[0]*!pi/180.))
     fin=min(where(degarr ge degarray[numinds-1]*!pi/180.))
     newhts=congrid(htarr[beg:fin],numinds)
     return,newhts
  endif
  
  return, htarr
end
