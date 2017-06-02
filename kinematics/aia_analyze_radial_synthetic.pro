;+============================================================================
pro test_aia_analyze_radial_synthetic
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     labels=['151104_01','151104_02','151104_03']
     labels=['151104_01']
     kinparams=[[1.05,715.,0.225],[1.05,681.,0.335],[1.05,726.,-89]]
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        kinetic_parameters=reform(kinparams[*,ev])
        aia_analyze_radial_synthetic,event,kinetic_parameters=kinetic_parameters
        stop
     endfor
  endif
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_analyze_radial_synthetic,event,wave=wavelength
        endfor
     endfor
  endif
end
;-============================================================================


;+============================================================================
pro aia_analyze_radial_synthetic,event,centerlat=centerlat,wave=wave,savepath=savepath,$
                                 datapath=datapath,thrange=thrange,rrange=rrange,$
                                 kinetic_parameters=kinetic_parameters
 ;PURPOSE:
;Procedure to analyze the speeds of radial expansion of a
;wave and/or a filament. This procedure generates a synthetic kinematic measurements dataset.
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
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 03/15/2016
;


;----------------------------
;Restore the file with the deprojected data
  date=event.date
  
  if not keyword_set(wave) then wav='193' else wav=wave
  if not keyword_set(savepath) then savepath=event.annuluspath
  if not keyword_set(datapath) then datapath=savepath
  if not keyword_set(kinetic_parameters) then kinetic_parameters=[1.05,400.,0.1]
  
  

  fname=replace_string(event.annplot.savename,'WAV',wav)
  restore, datapath+fname

  ;Create the time structure
  time=get_time(ind_arr.date_obs)
  nsteps=n_elements(subprojdata[*,0,0])
  ncols=n_elements(subprojdata[0,*,0])
  nrows=n_elements(subprojdata[0,0,*])
  projdata=0.0
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  kinetic_parameters[0]*=RSUN
  print,kinetic_parameters
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  
;Pixel coordinates in arcseconds from the center of the sun. How do I convert them to km?
  y_arcsec_array=(res/ind_arr[0].cdelt1*findgen(nrows)+r_in)
  y_rsun_array=y_arcsec_array/ind_arr[0].rsun_obs
  
;The X-angular array (distance along the limb from the pole).
  x_deg_array=findgen(ncols)*ang_step+thrang[0]*180./!PI
  htlimits=aia_rad_height_limits(degarray=x_deg_array)
  
  if not keyword_set(centerlat) then begin
     ;if event.arlon lt 0.0 then arlat=270.0+event.arlat $
     ;else arlat=90.0-event.arlat
     sourcepos=get_coordinates(event.coordx,event.coordy,/hpc,date=event.date)
     arlon=sourcepos.hprln
  endif else arlat=centerlat
  
;Find the radial outward limit/edge for which there's data
  yradlimit=aia_rad_height_limits(angle=arlat)
  yradlimind=min(where(y_arcsec_array ge yradlimit))

;The latitudinal index of the AR
  arxcentind=min(where(x_deg_array ge arlat))
  limb=ind_arr[0].rsun_obs      ;Limb position in arcsec
  limbind=min(where(y_arcsec_array ge limb))
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
  endfor
  

;----------------------------------------------------
;Create the radial data structure
  ;make sure there are no negative values in the data.
  ind=where(subprojdata le 0.0)
  if ind[0] ne -1 then subprojdata[ind]=1.0e-10 ;Set to very nearly zero  
  
  tmp=subprojdata[*,arxcentind-2:arxcentind+2,*]
  tmp=total(tmp,2)
  tmp[*,yradlimind:*]=-1000.
  
  bdiff=tmp
  for t=0,nsteps-1 do bdiff[t,*]-=tmp[0,*]
  
  rad_data={$
           type:'radial',$
           wav:wav,$
           lat_pos:arlat,$
           data:tmp,bdiff:bdiff,rdiff:tmp,$
           scale:[12.,res/ind_arr[0].cdelt1/ind_arr[0].rsun_obs],$
           origin:[0,y_rsun_array[0]],winsize:[1000,800],multi:[0,0,0],winind:3,$
           difforigin:[0,y_rsun_array[htind[0]]],xfitrange:[5,nsteps-1],yfitrange:[htind[0],yradlimind],$
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
           wavethick:fltarr(1, nsteps),$
           avgIntense:fltarr(1, nsteps),$
           xtitle:'Time of '+event.date,ytitle:'R!Dsun!N',$
           imgtit:'AIA/'+wav+' BDiff Radial Positions',$
           savename:replace_string(replace_string(event.annplot.analyzed.radial.plot_savename,'WAV',wav),'SSSSS','_synthetic'),$
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
  yrng=rad_data.yfitrange
  aia_jmap_find_maxima,rad_data.bdiff,time.relsec,y_rsun_array,mymaxima=mymaxima,allmaxima=allmaxima,$
                       yrange=[y_rsun_array[rad_data.yfitrange[0]],y_rsun_array[rad_data.yfitrange[1]]],$
                       numplotmax=3
  find_start_end, rad_data.bdiff[*, yrng[0]:yrng[1]], time, y_rsun_array, startInd=startInd, endInd=endInd
  rad_data.xfitrange=[startInd,endInd]

;-============================================================================
;HERE GENERATE THE SYNTHETIC KINEMATICS
;For the current implementation, only need to specify x0 (km), v0
;(km/s), and a (km/s^2)
;-============================================================================
  
  rad_data.fitparams[0,0].front=kinetic_parameters[0]
  rad_data.fitparams[0,1].front=kinetic_parameters[1]
  rad_data.fitparams[0,2].front=kinetic_parameters[2]
  
  
  for ii = 0, n_elements(wave_frontedge)-1 do begin
     datastruct.wavethick[sp+ii] = wave_frontedge[ii].rad - wave_backedge[ii].rad
  endfor
;-============================================================================
  
   save,filename=savepath+replace_string(event.annplot.analyzed.radial.savename,'WAV',wav),rad_data,ind_arr,nsteps,$
         ncols,nrows,wav,htind,yradlimind,y_arcsec_array,$
         y_rsun_array,time
  
end
