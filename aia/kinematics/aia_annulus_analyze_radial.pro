;+============================================================================
pro test_aia_annulus_analyze_radial
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     rrange=[1.1,1.34]
     event=load_events_info(label='110511_01')
     aia_annulus_analyze_radial,event,wave=wav,/constrain ;,rrange=rrange ;,/interactive
  endif
    
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     rrange=[1.1,1.37]
;n_elements(events)-1
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_annulus_analyze_radial,event,wave=wavelength,rrange=rrange,/constrain ;,/interactive
        endfor
     endfor
  endif

end
;-============================================================================



;+============================================================================
pro aia_annulus_analyze_radial,event,datapath=datapath,savepath=savepath,thrange=thrange,wave=wave,rrange=rrange,constrain=constrain
;PURPOSE:
;Procedure to analyze the speeds of radial expansion of a
;wave and/or a filament.
;Uses output from aia_annulus_plot.pro, a procedure deprojecting AIA
;data onto a rectangular grid, where the X-axis is latitude along the
;limb, and the Y-axis is radial distance.
;This procedure is the same as aia_annulus_analyze, but for just
;radial.
;NB! The geometric correction factor is not applied at this
;stage. It will be applied after this analysis, since it is a simple
;multiplicative factor.
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
;      CONSTRAIN - if set, constrain the wave to move with physical
;                  speeds (10-1500 km/s)
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 05/13/2014
;


;----------------------------
;Restore the file with the deprojected data
  date=event.date
  
  if not keyword_set(wave) then wav='193' else wav=wave
  if not keyword_set(savepath) then savepath=event.savepath+'annulusplot/' ;'./'
  if not keyword_set(datapath) then datapath=savepath ;'./'

  fname='aia_deprojected_annulus_'+event.date+'_'+event.label+'_'+wav+'.sav'
  restore, datapath+fname
  ;Create the time structure
  time=get_time(ind_arr.date_obs)
  nsteps=n_elements(subprojdata[*,0,0])
  ncols=n_elements(subprojdata[0,*,0])
  nrows=n_elements(subprojdata[0,0,*])
  projdata=0.0
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  
;Pixel coordinates in arcseconds from the center of the sun. How do I convert them to km?
  y_arcsec_array=(res/ind_arr[0].cdelt1*findgen(nrows)+r_in)
  y_rsun_array=y_arcsec_array/ind_arr[0].rsun_obs
  
;The X-angular array (distance along the limb from the pole).
  x_deg_array=findgen(ncols)*ang_step+thrang[0]*180./!PI
  
;The relative times
;  rad_times=findgen(nsteps)*dt/60./60.
;  lat_times=findgen(nsteps)*dt/60.
  


;----------------------------------------------------
;1. Plot the annulus plot for context
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position=[0.2,0.2,0.9,0.9]
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  wdef,0,1200,800
  
  img=reform(subprojdata[fix(nsteps/3),*,*]-subprojdata[fix(nsteps/3)-1,*,*])
  plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
              ytitle = '!5Radius [arcsec from Sun center]', $
              title = 'Annulusplot, AIA/'+wav+' '+date, max = 50, $
              origin = [thrang[0]*180./!PI,r_in], charthick = 1.2, charsize=3,$
              scale = [ang_step, res/ind_arr[0].cdelt1], $
              pos = [0.16, 0.16, 0.95, 0.9], min = -40
  
  htlimits=aia_rad_height_limits(degarray=x_deg_array)
  oplot,x_deg_array,htlimits,color=255,thick=2 ;,/data  
  
  if not keyword_set(centerlat) then begin
     if event.arlon lt 0.0 then arlat=270.0+event.arlat $
     else arlat=90.0-event.arlat
  endif else arlat=centerlat
  
;Find the radial outward limit/edge for which there's data
  yradlimit=aia_rad_height_limits(angle=arlat)
  yradlimind=min(where(y_arcsec_array ge yradlimit))
  
  oplot,[arlat,arlat],[ind_arr[0].rsun_obs,yradlimit],thick=2

;The index of the AR latitude
  arxcentind=min(where(x_deg_array ge arlat))
  limb=ind_arr[0].rsun_obs      ;Limb position in arcsec
  limbind=min(where(y_arcsec_array ge limb))
  ;oplot,thrang*180./!PI,[limb,limb],linestyle=2,thick=2,color=255 ;Overplot the limb position
  lat_heights=(limb+[0.25,0.55,0.85]*(yradlimit-limb))/ind_arr[0].rsun_obs
  
  ;Find the indices of the lateral measurement heights
  nlatmeas=n_elements(lat_heights)
  htind=intarr(nlatmeas)
  goodlats=fltarr(nlatmeas,ncols)-10.
  for ii=0,nlatmeas-1 do begin
     htind[ii]=min(where(y_rsun_array ge lat_heights[ii]))
  ;Find where the lat_heights are larger than the radial limits 'htlimits'.
     tt=where(htlimits-lat_heights[ii]*ind_arr[0].rsun_obs ge 0.)
     if tt[0] ne -1 then goodlats[ii,tt]=y_arcsec_array[htind[ii]]
     plots,x_deg_array,goodlats[ii,*],psym=1,thick=1,symsize=0.1
                                ;Overplot the lateral measurements location
                                ;oplot,thrang*180./!PI,[y_arcsec_array[htind[ii]],y_arcsec_array[htind[ii]]],thick=2
  endfor
;Save the overview plot
  write_png,savepath+'annplot_'+date+'_'+event.label+'_'+wav+'_overview_plot.png',tvrd(/true),ct_rr,ct_gg,ct_bb
 ; wdel,0
;----------------------------------------------------

  

;----------------------------------------------------
;Create the radial data structure
  ;make sure there are no negative values in the data.
  ind=where(subprojdata le 0.0)
  if ind[0] ne -1 then subprojdata[ind]=1.0e-10 ;Set to very nearly zero  
  
  tmp=subprojdata[*,arxcentind-2:arxcentind+2,*]
  tmp=total(tmp,2)
  tmp[*,yradlimind:*]=-1000.
  
  rad_data={$
           type:'radial',$
           wav:wav,$
           lat_pos:arlat,$
           data:tmp,bdiff:tmp,rdiff:tmp,$
           scale:[12.,res/ind_arr[0].cdelt1/ind_arr[0].rsun_obs],$
           origin:[0,y_rsun_array[0]],winsize:[1000,800],multi:[0,0,0],winind:3,$
           difforigin:[0,y_rsun_array[htind[0]]],xfitrange:[0,0],yfitrange:[htind[0],yradlimind],$
           plotinfo:{p:!P, x:!X, y:!Y},$
           kinquantity:['R!D0!N','R!D1!N','V!DR,0!N','V!DR,1!N','a'],$
           kinunit:[' R!DS!N',' R!DS!N',' km/s',' km/s',' km/s!U2!N'],$
           kinvalue:replicate({front:0.0, max:0.0, back:0.0},1,5),$
           kinsigma:replicate({front:0.0, max:0.0, back:0.0},1,5),$
           fitparams:replicate({front:0.0, max:0.0, back:0.0},1,3),$
           fitsigma:replicate({front:0.0, max:0.0, back:0.0},1,3),$
           maxinds:intarr(1,nsteps),$
           frontinds:intarr(1,nsteps),$
           backinds:intarr(1,nsteps),$
           xtitle:'Time of '+event.date,ytitle:'R!Dsun!N',$
           imgtit:'AIA/'+wav+' BDiff Radial Positions',$
           savename:'annplot_'+date+'_'+event.label+'_'+wav+'_radial.png',$
           time:time.relsec,$
           date_obs:ind_arr.date_obs $
           } 
  if keyword_set(rrange) then begin
     rind=min(where(y_rsun_array gt rrange[0]))
     if rind ne -1 then rad_data.yfitrange[0]=rind
     rind=min(where(y_rsun_array gt rrange[1]))
     if rind ne -1 then rad_data.yfitrange[1]=rind
     rad_data.difforigin[1]=rrange[0]
  endif
;----------------------------------------------------



;----------------------------------------------------
;JMAP PLOTTING
;----------------------------------------------------

;Plot the Radial positions  
  tmpdata=rad_data.data
  base=total(tmpdata[0:4,*,*],1)/5.0
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
;DESPIKE THE IMAGE
  tmp=despike_gen(tmpdata)
  rad_data.bdiff=tmp
  
  ;All the rest of the work is done here.
  annulus_fit_maxima_radial,event,rad_data.bdiff,rad_data,time,y_rsun_array,constrain=constrain
  
  rad_data.plotinfo.p=!P
  rad_data.plotinfo.x=!X
  rad_data.plotinfo.y=!Y
  write_png,savepath+rad_data.savename,tvrd(/true),ct_rr,ct_gg,ct_bb
;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;
  
 save,filename=savepath+'annplot_'+date+'_'+event.label+'_'+wav+'_analyzed_radial.sav',rad_data,ind_arr,nsteps,$
         ncols,nrows,wav,htind,yradlimind,y_arcsec_array,$
         y_rsun_array,time



end
;-============================================================================



;+============================================================================
pro annulus_fit_maxima_radial,event,indata,datastruct,time,yarr,lateral=lateral,constrain=constrain

  RSUN=6.96e5  ;Solar radius in km.
  nmeas=n_elements(datastruct.imgtit)
  nt=n_elements(time)
  yarray=yarr
  
  
  ;The info to pass to the position fitting routine for the fitting parameters
  parinfo = replicate({value:0.D, limited:[0,0], limits:[0.D,0.D]}, 3)
  
  xmargin=0.03
  ymargin=0.05
  chars=2
  TIME_FACTOR=3600.
  DIST_FACTOR=RSUN
  parinfo[0].value=1.0*DIST_FACTOR
  parinfo[0].limited=[1,1]
  parinfo[0].limits=[1.01,1.2]*DIST_FACTOR
  
  
  parinfo[1].value=100.
  parinfo[1].limited=[1,1]
  parinfo[1].limits=[0.0,2000.0]
  parinfo[2].value=10.
  parinfo[2].limited[0]=1
  parinfo[2].limits[0]=-1500.0
  
  yrng=datastruct.yfitrange
  !p.multi=datastruct.multi
  device,window_state=win_open
  
  
;LOOP OVER MEASUREMENTS!
  for mind=0,nmeas-1 do begin
  data=indata[*,*,mind]
  height=1.0
  ht_km=yarr*DIST_FACTOR*height
  if mind eq 0 then wdef,datastruct.winind,datastruct.winsize[0],datastruct.winsize[1]

  
  !P.position=[0.18,0.17,0.9,0.9]
  fitrange=intarr(2)
  aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],data[*,yrng[0]:yrng[1]],$
                     min=-40,max=50,fitrange=fitrange,$
                     title=datastruct.imgtit[mind],$
                     xtitle=datastruct.xtitle,ytitle=datastruct.ytitle
  
  datastruct.xfitrange=fitrange
  sp=datastruct.xfitrange[0]
  ep=datastruct.xfitrange[1]
                                ;Search for the edges of the wave
  wave_frontedge=replicate({rad:0.0D,ind:0L},ep-sp+1)
  wave_backedge=wave_frontedge
  
  ;To restore the plot information and overplot on them, do
  datastruct.plotinfo[mind].p=!P
  datastruct.plotinfo[mind].x=!X
  datastruct.plotinfo[mind].y=!Y
  
  ;Fit the maxima and overplot them...
  aia_jmap_find_maxima,data,time.relsec,yarray,mymaxima=mymaxima,allmaxima=allmaxima,$
                   yrange=[yarr[datastruct.yfitrange[0]],yarr[datastruct.yfitrange[1]]],$
                   numplotmax=3
  tmp=reform(mymaxima[0,*].ind)
  datastruct.maxinds[mind,*]=reform(mymaxima[0,*].ind)
  
;Filter the maxima positions here for physicality
  if keyword_set(constrain) then begin
     maxinds=jmap_filter_maxima_radial(time.relsec,ht_km,allmaxima,fitrange=datastruct.xfitrange) ;,outliers=outliers
     mymaxima=maxinds
  endif
 
  device,window_state=win_open
  oplot,time.jd,reform(yarray[datastruct.maxinds[mind,*]]),psym=1,color=200,thick=4,symsize=2
  loadct,8,/silent
  oplot,time[sp:ep].jd,reform(mymaxima[mind,sp:ep].rad),psym=1,color=200,thick=4,symsize=2
  
;  loadct,8,/silent
  ;oplot,time[sp:ep],yarray[datastruct.mymaxima[mind,sp:ep]],psym=1,color=200,thick=4,symsize=2
;  oplot,time[sp+good_ind_pos].jd,yarray[datastruct.mymaxima[mind,sp+good_ind_pos]],psym=1,color=200,thick=4,symsize=2
  ;oplot,[time[sp].jd,time[sp].jd],[yarray[0],yarray[n_elements(yarray)-1]],color=255
  ;oplot,[time[ep].jd,time[ep].jd],[yarray[0],yarray[n_elements(yarray)-1]],color=255
  
  for ii=sp,ep do begin
        
;+--------------------------------------------------------------
;Find the front edge of the wave
        oldv=1
        if oldv gt 0 then begin
;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
           y=reform(datastruct.bdiff[ii,mymaxima[mind,ii].ind:*])
           y=smooth(y,2,/edge_truncate)
           np=n_elements(y)
           tmp=min(where(y le 0.25*max(y)))
           if tmp[0] eq -1 then tmp=np-1
           wave_frontedge[ii-sp].rad=yarray[mymaxima[mind,ii].ind+tmp]
           wave_frontedge[ii-sp].ind=mymaxima[mind,ii].ind+tmp
           datastruct.frontinds[mind,ii]=wave_frontedge[ii-sp].ind
        endif
        
        newv=0
        if newv gt 0 then begin
;NEW VERSION,SEARCHING UP FROM BACKGROUND 
           maxind=mymaxima[mind,ii].ind-datastruct.yfitrange[0]
           ylim=datastruct.yfitrange[1]
           y=reform(datastruct.bdiff[ii,maxind:ylim])
           ;y=smooth(y,4,/edge_truncate)
           np=n_elements(y)
           y=reverse(y)
           ;y[where(y le 0.0)]=1.0e-10
           bind=20
           bckg=abs(avg(y[0:bind-1]))
           tmp=min(where(y gt (y[np-1]-bckg)*0.2)) ;look for 25% increase above background
           tmp=np-tmp                              ;since data is reversed, reverse the index as well.
           if tmp[0] eq -1 then tmp=0
           wave_frontedge[ii-sp].rad=yarray[mymaxima[mind,ii].ind+tmp]
           wave_frontedge[ii-sp].ind=mymaxima[mind,ii].ind+tmp
           datastruct.frontinds[mind,ii]=mymaxima[mind,ii].ind+tmp
        endif
;---------------------------------------------------------------
        
        
;+--------------------------------------------------------------
;Find the back edge of the wave
        oldv=1
        if oldv gt 0 then begin
;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
           y=reform(datastruct.bdiff[ii,0:mymaxima[mind,ii].ind])
           ;y=smooth(y,4,/edge_truncate)
           np=n_elements(y)
           y=reverse(y,1)       ;reverse the array so the search is the same
           tmp=min(where(y le 0.3*max(y)))
           if tmp[0] eq -1 then rad=yarray[mymaxima[mind,ii].ind-tmp]
           wave_backedge[ii-sp].ind=mymaxima[mind,ii].ind-tmp
           datastruct.backinds[mind,ii]=mymaxima[mind,ii].ind-tmp
        endif
        
        newv=0
        if newv gt 0 then begin    
;NEW VERSION, SEARCHING UP FROM BACKGROUND
           y=reform(datastruct.bdiff[ii,0:maxind])
           y=reverse(y,1)
           y=smooth(y,4,/edge_truncate)
           np=n_elements(y)
           tmp=min(where(y gt 2.*bckg)) ;look for 20% increase above background
           stop
           if tmp[0] eq -1 then tmp=np-1
           wave_backedge[ii-sp].rad=yarray[mymaxima[mind,ii].ind+tmp]
           wave_backedge[ii-sp].ind=mymaxima[mind,ii].ind+tmp
           datastruct.backinds[mind,ii]=mymaxima[mind,ii].ind+tmp
        endif
;---------------------------------------------------------------        
 
;DEBUG    
;For now, don't overplot the back edge  
        oplot,[time[ii].jd,time[ii].jd],[mymaxima[mind,ii].rad,wave_frontedge[ii-sp].rad],$
              color=200,thick=2
;END DEBUG

  endfor
  loadct,0,/silent
  
  
;--------------------------------------------------
;Do second order polynomial fitting for the wave fronts edges
  print,''
  print,'Fitting a second-order polynomial to the wave front edge positions...'
  dist=reform(wave_frontedge[0:ep-sp].rad)*DIST_FACTOR*height;*event.geomcorfactor
  time_good=time[sp:ep].relsec-time[sp].relsec
  dist=smooth(dist,4,/edge_truncate)
  bootstrap_sdo,dist,time_good,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo
  print,''
  print,''
  wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
  wave_fits=wave_fits/DIST_FACTOR/height
  datastruct.fitparams[mind,0].front=p1[0]
  datastruct.fitparams[mind,1].front=p2[0]
  datastruct.fitparams[mind,2].front=p3[0]
  datastruct.fitsigma[mind,0].front=s1[0]
  datastruct.fitsigma[mind,1].front=s2[0]
  datastruct.fitsigma[mind,2].front=s3[0]

;  wave_rads=rad_data.fitparams[0,0].front+rad_data.fitparams[0,1].front*wave_times+0.5*(rad_data.fitparams[0,2].front)^2
;--------------------------------------------------
  oplot,time[sp:ep].jd,wave_fits,thick=3
  

;--------------------------------------------------
;Do second order polynomial fitting for the maxima
  print,''
  print,'Fitting a second-order polynomial to the wave peak positions...'
  dist=reform(ht_km[mymaxima[mind,sp:ep].ind]);*event.geomcorfactor
  time_good=time[sp:ep].relsec-time[sp].relsec
  dist=smooth(dist,4,/edge_truncate)
  ;bootstrap_sdo,dist,time_good,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo
  print,''
  print,''
  wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
  wave_fits=wave_fits/RSUN;*180./!PI
  datastruct.fitparams[mind,0].max=p1[0]
  datastruct.fitparams[mind,1].max=p2[0]
  datastruct.fitparams[mind,2].max=p3[0]
  datastruct.fitsigma[mind,0].max=s1[0]
  datastruct.fitsigma[mind,1].max=s2[0]
  datastruct.fitsigma[mind,2].max=s3[0]
;--------------------------------------------------
 ; oplot,time[sp:ep].jd,wave_fits,thick=3
  
  
;--------------------------------------------------
;Do second order polynomial fitting for the wave back edges
  print,''
  print,'Fitting a second-order polynomial to the wave back edge positions...'
  dist=reform(wave_backedge[0:ep-sp].rad)*DIST_FACTOR*height*event.geomcorfactor
  time_good=time[sp:ep].relsec-time[sp].relsec
  dist=smooth(dist,4,/edge_truncate)
  ;bootstrap_sdo,dist,time_good,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo
  print,''
  print,''
  wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
  wave_fits=wave_fits/DIST_FACTOR/height
  datastruct.fitparams[mind,0].back=p1[0]
  datastruct.fitparams[mind,1].back=p2[0]
  datastruct.fitparams[mind,2].back=p3[0]
  datastruct.fitsigma[mind,0].back=s1[0]
  datastruct.fitsigma[mind,1].back=s2[0]
  datastruct.fitsigma[mind,2].back=s3[0]
;--------------------------------------------------
 ; oplot,time[sp:ep].jd,wave_fits,thick=3
  

;--------------------------------------------------  
  ;Print out the results of the fitting
;initial position angle/Radial position
;  r0=mymaxima[mind,sp].ind.rad
  r0=datastruct.fitparams[mind,0].front/DIST_FACTOR/height
  sr0=datastruct.fitsigma[mind,0].front/DIST_FACTOR/height
  tmpstr=datastruct.kinquantity[0]+' = '+strtrim(string(r0,format='(f9.2)'),2)+datastruct.kinunit[mind,0]     ;+' +/-'+strtrim(string(sr0,format='(f11.3)'),2)+datastruct.kinunit[mind,0]
  datastruct.kinvalue[mind,0].max=r0
  datastruct.kinsigma[mind,0].max=s1[0]
  print,tmpstr
  xyouts,!x.window[0]+xmargin,!P.position[3]-ymargin,tmpstr,/norm,charsize=3,color=255,charthick=2
  
;final height
;  rf=yarray[mymaxima[mind,ep].ind]
  rf=wave_frontedge[ep-sp].rad
  tmpstr=datastruct.kinquantity[1] +' = '+strtrim(string(rf,format='(f9.2)'),2)+datastruct.kinunit[1]
  datastruct.kinvalue[mind,1].max=rf
  xyouts,!x.window[0]+xmargin,!P.position[3]-2*ymargin,tmpstr,/norm,charsize=3,color=255,charthick=2

  
;initial speed
  v0=datastruct.fitparams[mind,1].front
  errv0=datastruct.fitsigma[mind,1].front
  
  tmpstr=datastruct.kinquantity[2]+' = '+strtrim(string(v0,format='(f9.2)'),2)+datastruct.kinunit[2];' +/-'+strtrim(string(errv0,format='(f9.2)'),2)+datastruct.kinunit[2]
  datastruct.kinvalue[mind,2].max=v0
  datastruct.kinsigma[mind,2].max=errv0
  print,tmpstr
  xyouts,!x.window[0]+xmargin,!P.position[3]-3*ymargin,tmpstr,/norm,charsize=3,color=255,charthick=2
  
;final speed
  accel=datastruct.fitparams[mind,2].front
  vf=(v0+accel*(time[ep].relsec-time[sp].relsec))
;  if keyword_set(lateral) then vf=tmp[n_elements(tmp)-1]*height*RSUN*!PI/180. $
;  else vf=tmp[n_elements(tmp)-1]*RSUN
  tmpstr=datastruct.kinquantity[3]+' = '+strtrim(string(vf,format='(f9.2)'),2)+datastruct.kinunit[3]
  datastruct.kinvalue[mind,3].max=vf
  print,tmpstr
  xyouts,!x.window[0]+xmargin,!P.position[3]-4*ymargin,tmpstr,/norm,charsize=3,color=255,charthick=2
  
;acceleration
  accel=accel
  erraccel=datastruct.fitsigma[mind,2].front
  tmpstr=datastruct.kinquantity[4]+' = '+strtrim(string(accel,format='(f9.2)'),2)+datastruct.kinunit[4] ;' +/-'+strtrim(string(erraccel,format='(f9.2)'),2)+datastruct.kinunit[4]
  datastruct.kinvalue[mind,4].max=accel
  datastruct.kinsigma[mind,4].max=erraccel
  print,tmpstr
  print,''
  xyouts,!x.window[0]+xmargin,!P.position[3]-5*ymargin,tmpstr,/norm,charsize=3,color=255,charthick=2
endfor

end
;-============================================================================



;+============================================================================
function jmap_filter_maxima_radial,time,height,mymaxima,fitrange=fitrange
;PURPOSE:
; Filters the position maxima to make them smooth and physical
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      TIME - time array, must be in seconds!
;      HEIGHT - the y-array in the jmap, must be in km!
;      MYMAXIMA - the array of maxima indices from aia_jmap_find_maxima
;      
;
;KEYWORDS:
;      FITRANGE - the index range the user picks for fitting
;                  kinematics
;OUTPUTS:
;      Returns the  
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 11/29/2013
  RSUN=6.96e5                   ;Solar radius in km.
  vlimit=[10,1500.0]                 ;Position derivative max limit, in km/s.
  maxinds=mymaxima
  
  ;Set the time range so we only look at the user-selected time range.
  if keyword_set(fitrange) then tr=fitrange else tr=[0,n_elements(time)-1]
  rind=reform(maxinds[0,tr[0]:tr[1]].ind)

;Get the ranges of the time and radial distance, as well as their changes
  ht=height[rind]
  nr=n_elements(ht)
  dr=ht[1:nr-1]-ht[0:nr-2]

  tm=time[tr[0]:tr[1]]
  nt=n_elements(tm)
  dt=tm[1:nt-1]-tm[0:nt-2]
  
  ;Here, go through all the points, and check the speeds, fixing the
  ;positions if necessary
  change=0
  for tt=0,nt-2 do begin
     v=(ht[tt+1]-ht[tt])/(tm[tt+1]-tm[tt])
     if v lt vlimit[0] then begin
        ht[tt+1]=ht[tt]+(tm[tt+1]-tm[tt])*vlimit[0]
        change=1
     endif else begin
        if v ge vlimit[1] then begin
           ht[tt+1]=ht[tt]+(tm[tt+1]-tm[tt])*vlimit[1]
           change=1
        endif
     endelse
     maxinds[0,fitrange[0]+tt+1].rad=height[maxinds[0,fitrange[0]+tt+1].ind]/RSUN
     
;If there has been a change in the position, find the nearest larger height and
;assign its index to the maxinds structure.
     if change ne 0 then begin
        maxinds[0,fitrange[0]+tt+1].ind=min(where(height-ht[tt+1] gt 0.))
        maxinds[0,fitrange[0]+tt+1].rad=ht[tt+1]/RSUN
     endif
     
    change=0 
  endfor
  
  return,maxinds
end
;-============================================================================
