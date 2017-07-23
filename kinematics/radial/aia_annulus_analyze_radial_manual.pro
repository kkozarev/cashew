;+============================================================================
pro test_aia_annulus_analyze_radial_manual
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     labels=['110511_01']
     labels=['120806_01','140220_02']
     labels=['120307_01','120405_01','151104_01','151104_02','151104_03']
     ;GOOD NEW EVENTS:
     labels=['151109_01','151029_01','150920_01','150601_01','150509_01','150421_01','150303_01','141205_01','141105_02','141105_01','140901_01']
     
     labels=['110607_01']
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        aia_annulus_analyze_radial_manual,event,wave=wav,/fitdata ;,rrange=rrange ;,/interactive
     endfor
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
           aia_annulus_analyze_radial_manual,event,wave=wavelength,rrange=rrange;,/interactive
        endfor
     endfor
  endif
  
end
;-============================================================================



;+============================================================================
pro annulus_get_radial_edges, event, rad_data, annulus_info, plotinfo, exit_status

  RSUN=6.96e5                   ;Solar radius in km.
  TIME_FACTOR=3600.
  DIST_FACTOR=RSUN
  y_rsun_array=rad_data.y_rsun_array
  time=rad_data.time
  nt=n_elements(time)
  yrng=rad_data.radfitrange
  ;Define the exit statuses here:
  NORMAL_STATUS=0
  CONTINUE_STATUS=1
  QUIT_STATUS=2
  exit_status=NORMAL_STATUS
  
  
;Find the starting and ending time indices for which we believe we have a wave to fit.
  find_start_end_radial_manual, event, rad_data, plotinfo, exit_status
  
  if rad_data.timefitrange[0] eq -1 and rad_data.timefitrange[1] eq -1 then begin
     print,''
     uinput=''
     valid=0
     while valid eq 0 do begin
        read,uinput,prompt='NO WAVE PROFILE IS DETECTED. Do you want to continue anyway (y/n)?:'
        if uinput eq 'y' or uinput eq 'n' then valid=1
     endwhile
     if uinput eq 'n' then begin
        exit_status=CONTINUE_STATUS
        return
     endif
     if uinput eq 'y' then begin
        rad_data.timefitrange=[0,n_elements(time)-1]
     endif
  endif
  startInd=rad_data.timefitrange[0]
  endInd=rad_data.timefitrange[1]
  print, "Initial start index: ", startInd
  print, "Start Time: ", time[startInd].cashew_time
  print, "Initial end index: ", endInd
  print, "End Time: ", time[endInd].cashew_time
  
  ;Find the local intensity maxima in the J-map data
  ;aia_jmap_find_maxima,rad_data,yrange=[y_rsun_array[yrng[0]],y_rsun_array[yrng[1]]]
  
;Filter the maxima positions here for physicality
  ;if keyword_set(constrain) then begin
  ;maxinds=jmap_filter_maxima_radial(rad_data)
  ;mymaxima=maxinds
  ;endif
  
; Find the front and back edges of the wave at each time step,
; using gaussian MPFITPEAK routine.
  ;maxRadIndex = min(where(data[0,*] eq 0.0))
  find_wave_edge_radial_manual, event, rad_data, plotinfo, exit_status

end
;-============================================================================




;+============================================================================
pro aia_annulus_analyze_radial_manual,event,datapath=datapath,savepath=savepath,thrange=thrange,fitdata=fitdata,$
                                  wave=wave,rrange=rrange,constrain=constrain, gradient=gradient, auto=auto
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
!p.multi=0
!p.background=255
!p.color=0
;----------------------------
;Restore the file with the deprojected data
  date=event.date
  
  if not keyword_set(wave) then wav='193' else wav=wave
  if not keyword_set(savepath) then savepath=event.annuluspath
  if not keyword_set(datapath) then datapath=savepath
  
  fname=replace_string(event.annplot.savename,'WAV',wav)
  restore, datapath+fname
  
  print,''
  print,'Runnning the manual radial kinematics analysis tool for Event '+event.label
  print,''

  

  ;Check what has been run and display to user
  runfile_searchname=replace_string(event.annplot.analyzed.radial.savename,'SSSSS','_m*')
  runfile_searchname=replace_string(runfile_searchname,'WAV',wav)
  search_result=file_search(event.annuluspath+runfile_searchname)
  if search_result[0] ne '' then begin
     print,'The following measurement runs exist for this event:'
     for ii=0,n_elements(search_result)-1 do print,'    '+file_basename(search_result[ii])
  endif
  
  analysis_run_id=0
  print,''
  read,analysis_run_id,prompt='Please enter run # (1 for first): '
  analysis_run_id--
  ;Check that the radial measurement position is good. Show a movie,
;optionally.
  if analysis_run_id le 1 then begin
     aia_inspect_source_position_manual, event
     label=event.label
     event=load_events_info(label=label)
  endif
  analysis_run_id = strtrim(string(analysis_run_id),2)
  if analysis_run_id lt 10 then analysis_run_id='0'+analysis_run_id
;----------------------------
  

  
;----------------------------  
;Create the time structure and set parameters
  time=get_time(ind_arr.date_obs)
  ntimes=n_elements(time)
  nsteps=n_elements(projdata[*,0,0])
  context_step=fix(nsteps/3.)
  ncols=n_elements(projdata[0,*,0])
  nrows=n_elements(projdata[0,0,*])
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
;----------------------------
  

;----------------------------
;Create the radial and angular arrays, find the radial height data limits.
;
;The radial pixel coordinates, in Rsun and arcseconds.
  y_rsun_array = annulus_info.rcoords               ;y_arcsec_array/ind_arr[0].rsun_obs
  y_arcsec_array = y_rsun_array*ind_arr[0].rsun_obs ;(res/ind_arr[0].cdelt1*findgen(nrows)+r_in)
  
;The X-angular array (distance along the limb from the pole).
  x_deg_array = annulus_info.thcoords*180./!PI ;findgen(ncols)*ang_step+thrang[0]*180./!PI
  
;The array of outer radial limits of the data, in Rsun
  y_rsun_limits=aia_rad_height_limits(ind_arr[0],degarray=x_deg_array,/rsun)
;----------------------------
  
  
;----------------------------
;If this is to be run for only the central source location, find it!
;The position angle of the source location 
  if not keyword_set(centerlat) then begin
     arlon=get_polar_angle(event)
  endif else arlon=centerlat
  
;The latitudinal index of the AR
  arlonind=min(where(x_deg_array ge arlon))
  limb=1                        ;Limb position in Rsun
  limbind=min(where(y_rsun_array ge limb))
  ;rel_rad_positions=[0.05,0.15,0.43,0.53]
  rel_rad_positions=[0.1,0.51]
  lat_heights=(limb+rel_rad_positions*(max(y_rsun_limits)-limb))
  
;For the tangential measurements, get a map of the good tangential
;pixel values - those where there is data. This will only be relevant
;if taking tangential measurements high up.
  x_good_lats=aia_deg_tan_limits(x_deg_array,y_rsun_array,y_rsun_limits,lat_heights)

;Find the Y-radial outward limit/edge for which there's data
  yradlimit=aia_rad_height_limits(ind_arr[0],angle=arlon,/rsun)
  yradlimind=max(where(y_rsun_array le yradlimit))
;----------------------------  

 

;----------------------------
  see_context=0
  if see_context gt 0 then begin
;Plot the annulus for context
     plot_annulus_context,projdata,annulus_info,ind_arr,event,lat_heights,x_good_lats
     valid=0
     showit=''
     while valid eq 0 do begin
        print,'Would you like to see a movie of the annulus data for context?'
        read,showit,prompt='Please type "y" for yes, "n" for no:'
        if showit eq 'y' or showit eq 'n' then valid=1
     endwhile
     if showit eq 'y' then begin
        print,''
        print,'Showing the annulus data for context. Please note the wave start/end times.'
        plot_annulus_movie,projdata,annulus_info,ind_arr,event,lat_heights,x_good_lats
                                ;aia_update_source_position_manual, event
     endif
  endif
;----------------------------


;----------------------------
;Create the radial data structure

  plotinfo={p:!P, x:!X, y:!Y, $
            origin:[0,y_rsun_array[0]], $
            winsize:[1000,800], $
            multi:[0,0,0], $
            winind:3,$
            difforigin:[0,0],$
            kinquantity:['R!D0!N','R!D1!N','V!DR,0!N','V!DR,1!N','a'],$
            kinunit:[' R!DS!N',' R!DS!N',' km/s',' km/s',' km/s!U2!N'],$
            xtitle:'Time of '+event.date,ytitle:'R!Dsun!N',$
            imgtit:'AIA/'+wav+' BDiff Radial, event '+event.label,$
            savename:replace_string(replace_string(event.annplot.analyzed.radial.plot_savename,'WAV',wav),'SSSSS','_auto')}
  
  rad_data_template={type:'radial',$
                     label:event.label,$
                     wav:wav,$
                     time:time,$
                     y_rsun_array:y_rsun_array,$
                     y_arcsec_array:y_arcsec_array,$
                     x_deg_array:x_deg_array,$
                     data:dblarr(ntimes,nrows),$
                     origdata:dblarr(ntimes,nrows),$
                     diffdata:dblarr(ntimes,nrows),$
                     radfitrange:[0,yradlimind],$
                     timefitrange:[0,0],$
                     fitparams:replicate({front:0.0, peak:0.0, back:0.0},3),$
                     fitsigma:replicate({front:0.0, peak:0.0, back:0.0},3),$
                     kinfittimerange:{front:[0,0],peak:[0,0],back:[0,0]},$
                     savgolfits:{front:replicate({speed:0.0,accel:0.0},nsteps),$
                                 peak:replicate({speed:0.0,accel:0.0},nsteps),$
                                 back:replicate({speed:0.0,accel:0.0},nsteps)},$                   
                     maxinds:intarr(1,nsteps),$
                     wave_frontedge:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wave_backedge:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wave_peak:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wavethick:fltarr(nsteps),$
                     avgIntense:fltarr(nsteps)}
  
  ;Make sure there are no negative values in the data.
  ind=where(projdata le 0.0d)
  if ind[0] ne -1 then projdata[ind]=1.0e-20 ;Set to very nearly zero
  
  ;If the user has supplied a hard radial range, apply it.
  if keyword_set(rrange) then begin
     rind=min(where(y_rsun_array gt rrange[0]))
     if rind ne -1 then rad_data.radfitrange[0]=rind
     rind=min(where(y_rsun_array gt rrange[1]))
     if rind ne -1 then rad_data.radfitrange[1]=rind
     rad_data.difforigin[1]=rrange[0]
  endif
  
  rad_data=rad_data_template
  
  
;----------------------------
;Create the base difference data
;Create a radial profile by averaging the radial profile at the
;presumed source location.
;This would be a great place to put the radial kinematics scanning functionality.
  origdata=total(projdata[*,arlonind-3:arlonind+3,*],2)/7.
  rad_data.origdata=origdata
  
  diffdata=origdata
  base=total(total(projdata[0:4,arlonind-3:arlonind+3,*],1)/5.0,1)/7.0
  for tt=1,nsteps-1 do begin
     diffdata[tt,yradlimind+1:*]=base[yradlimind+1:*]
     diffdata[tt,*]-=base
  endfor
  ;Despike the resulting image
  diffdata=despike_gen(diffdata)
  rad_data.diffdata=diffdata
;----------------------------

  
  
;----------------------------
;Generate the fit data by processing the base difference data
fitdata_prep_radial_2, rad_data, plotinfo
;----------------------------


  
;----------------------------
;Find the wave front, back, peak positions
  annulus_get_radial_edges, event, rad_data, annulus_info, plotinfo, exit_status
  ;Define the exit statuses here:
  NORMAL_STATUS=0
  CONTINUE_STATUS=1
  QUIT_STATUS=2
  
  if exit_status eq CONTINUE_STATUS then begin
     print, ''
     print,'No wave profile was found in the radial data.'
     return
  endif
  if exit_status eq QUIT_STATUS then begin
     print,'Quitting the measurement.'
     return
  endif
;----------------------------
  
  
;----------------------------
;Fit the kinematics of the front, peak, and back of the wave!
if keyword_set(fitdata) then begin
   fit_wave_kinematics_radial,rad_data,ind_arr,/front
   fit_wave_kinematics_radial,rad_data,ind_arr,/peak
   fit_wave_kinematics_radial,rad_data,ind_arr,/back
endif
;----------------------------


;----------------------------
;Plot some results!
  sp=rad_data.timefitrange[0]
  ep=rad_data.timefitrange[1]
  yrng=rad_data.radfitrange
  xrng=rad_data.timefitrange
  yarray=rad_data.y_rsun_array
  dt2=(rad_data.time[1].jd-rad_data.time[0].jd)/2.
  
  loadct,0,/silent
  wdef,0,1200,1000
  plotdiff=rad_data.diffdata[*,yrng[0]:yrng[1]]
  plotdiff=plotdiff-smooth(plotdiff,[14,1],/edge_truncate)
  aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],plotdiff,$
                     min=-10,max=20,title=plotinfo.imgtit,xtitle=plotinfo.xtitle,ytitle=plotinfo.ytitle
  
  oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3
  oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3,color=255,linestyle=2
  oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3
  oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3,color=255,linestyle=2
  if sp eq ep then begin
     print,''
     print,"Starting and ending times are the same - can't plot or save measurements."
  endif else begin
     loadct,39,/silent
     oplot,time[sp:ep].jd+dt2,rad_data.wave_frontedge[sp:ep].rad,psym=sym(3),symsize=2,color=130
     oplot,time[sp:ep].jd+dt2,rad_data.wave_frontedge[sp:ep].rad,psym=sym(8),symsize=2,color=0,thick=3
     oplot,time[sp:ep].jd+dt2,rad_data.wave_peak[sp:ep].rad,psym=sym(1),symsize=2,color=130
     oplot,time[sp:ep].jd+dt2,rad_data.wave_peak[sp:ep].rad,psym=sym(6),symsize=2,color=0,thick=3
     oplot,time[sp:ep].jd+dt2,rad_data.wave_backedge[sp:ep].rad,psym=sym(2),symsize=2,color=130
     oplot,time[sp:ep].jd+dt2,rad_data.wave_backedge[sp:ep].rad,psym=sym(7),symsize=2,color=0,thick=3
     loadct,0,/silent

     
     pa=get_polar_angle(event)
     strpa=strtrim(string(pa,format='(f6.2)'),2)
     save_fname=replace_string(event.annplot.analyzed.radial.savename,'SSSSS','_pa'+strpa+'_m'+analysis_run_id)
     ;save_fname=replace_string(event.annplot.analyzed.radial.savename,'SSSSS','_m'+analysis_run_id)
     save_fname=replace_string(save_fname,'WAV',wav)
     
     plot_fname=replace_string(event.annplot.analyzed.radial.plot_savename,'SSSSS','_pa'+strpa+'_m'+analysis_run_id)
     ;plot_fname=replace_string(event.annplot.analyzed.radial.plot_savename,'SSSSS','_m'+analysis_run_id)
     plot_fname=replace_string(plot_fname,'WAV',wav)
     
     if file_exist(savepath+save_fname) then begin
        uinput=''
        print,''
        print,'File '+save_fname+' already exists.'
        read,uinput,prompt='Overwrite? (y for yes, n for no): '
        if uinput eq 'y' then begin
           write_png,savepath+plot_fname,tvrd(/true)
           save,filename=savepath+save_fname,rad_data,ind_arr,annulus_info,event
           print,''
           print,'Saved plot file '+savepath+save_fname
        endif
     endif else begin
        write_png,savepath+plot_fname,tvrd(/true)
        save,filename=savepath+save_fname,rad_data,ind_arr,annulus_info,event
        print,''
        print,'Saved plot file '+savepath+save_fname
     endelse
     wait,1
     wdel,0
  endelse
;----------------------------





end
;-============================================================================









