;+============================================================================
pro test_cashew_annulus_analyze_lateral_manual
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     labels=['110511_01']
     labels=['120806_01','140220_02']
     labels=['120307_01','120405_01','151104_01','151104_02','151104_03']
     labels=['110607_01']
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
                                ;rrange=[1.1,1.34]
        cashew_annulus_analyze_lateral_manual,event,wave=wav,/constrain,/fitdata;,/auto ;,rrange=rrange ;,/interactive
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
           cashew_annulus_analyze_lateral_manual,event,wave=wavelength,rrange=rrange,/constrain ;,/interactive
        endfor
     endfor
  endif
end
;-============================================================================




;+============================================================================
pro annulus_get_lateral_edges, event, lat_data,annulus_info, plotinfo, exit_status
  RSUN=6.96e5                   ;Solar radius in km.
  TIME_FACTOR=3600.
  DIST_FACTOR=RSUN
  y_deg_array=lat_data.x_deg_array
  time=lat_data.time
  nt=n_elements(time)
  yrng=lat_data.latfitrange
  ;Define the exit statuses here:
  NORMAL_STATUS=0
  CONTINUE_STATUS=1
  QUIT_STATUS=2
  exit_status=NORMAL_STATUS
  
;Find the starting and ending time indices for which we believe we have a wave to fit.
  find_start_end_lateral_manual, event, lat_data, plotinfo, exit_status
  
  if lat_data.timefitrange[0] eq -1 and lat_data.timefitrange[1] eq -1 then begin
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
        lat_data.timefitrange=[0,n_elements(time)-1]
     endif
  endif
  startInd=lat_data.timefitrange[0]
  endInd=lat_data.timefitrange[1]
  print, "Initial start index: ", startInd
  print, "Start Time: ", time[startInd].cashew_time
  print, "Initial end index: ", endInd
  print, "End Time: ", time[endInd].cashew_time
  
;Find the local intensity maxima in the J-map data
 ; aia_jmap_find_maxima_lateral,lat_data,yrange=[y_deg_array[yrng[0]],y_deg_array[yrng[1]]]
  
;Filter the maxima positions here for physicality
  ;maxinds=jmap_filter_maxima_lateral(lat_data)
  ;mymaxima=maxinds
  
; Find the front and back edges of the wave at each time step,
; using gaussian MPFITPEAK routine.
  find_wave_edge_lateral_manual, event, lat_data, plotinfo, exit_status
end
;-============================================================================




;+============================================================================
pro cashew_annulus_analyze_lateral_manual,event,datapath=datapath,savepath=savepath,thrange=thrange,fitdata=fitdata,$
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
  analysis_run_id=0
  read,analysis_run_id,prompt='Please enter run #: '
  analysis_run_id = strtrim(string(analysis_run_id),2)
  if analysis_run_id lt 10 then analysis_run_id='0'+analysis_run_id
;----------------------------
  
  
;----------------------------  
;Create the time structure and set parameters
  time=get_time(ind_arr.date_obs)
  ntimes=n_elements(time)
  nsteps=n_elements(projdata[*,0,0])
  context_step=fix(nsteps/3.)
  nlateral=n_elements(projdata[0,*,0])
  nradial=n_elements(projdata[0,0,*])
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
;----------------------------


;----------------------------
;Create the radial and angular arrays, find the radial height data limits.
;
;The radial pixel coordinates, in Rsun and arcseconds.
  y_rsun_array = annulus_info.rcoords               ;y_arcsec_array/ind_arr[0].rsun_obs
  y_arcsec_array = y_rsun_array*ind_arr[0].rsun_obs ;(res/ind_arr[0].cdelt1*findgen(nradial)+r_in)
  
;The X-angular array (distance along the limb from the pole).
  x_deg_array = annulus_info.thcoords*180./!PI ;findgen(nlateral)*ang_step+thrang[0]*180./!PI
  
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
  rel_rad_positions=[0.15,0.5]
  lat_heights=(limb+rel_rad_positions*(max(y_rsun_limits)-limb))
  nlatmeas=n_elements(lat_heights)
  radinds=intarr(nlatmeas)
  for ls=0,nlatmeas-1 do radinds[ls]=min(where(y_rsun_array ge lat_heights[ls]))

;For the lateral measurements, get a map of the good lateral
;pixel values - those where there is data. This will only be relevant
;if taking lateral measurements high up.
  x_good_lats=aia_deg_tan_limits(x_deg_array,y_rsun_array,y_rsun_limits,lat_heights)
  
;Find the Y-radial outward limit/edge for which there's data
  yradlimit=aia_rad_height_limits(ind_arr[0],angle=arlon,/rsun)
  yradlimind=max(where(y_rsun_array le yradlimit))
;----------------------------  


;----------------------------
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
  endif
;----------------------------


;----------------------------
;Create the lateral data structure

  plotinfo={p:!P, x:!X, y:!Y, $
            origin:[0,x_deg_array[0]], $
            winsize:[1000,800], $
            multi:[0,0,0], $
            winind:3,$
            difforigin:[0,0],$
            kinquantity:['R!D0!N','R!D1!N','V!DR,0!N','V!DR,1!N','a'],$
            kinunit:[' R!DS!N',' R!DS!N',' km/s',' km/s',' km/s!U2!N'],$
            xtitle:'Time of '+event.date,ytitle:'Degrees',$
            imgtit:'AIA/'+wav+' BDiff Lateral Positions, event '+event.label,$
            savename:replace_string(replace_string(event.annplot.analyzed.lateral.leftplot_savename,'WAV',wav),'SSSSS','_m')}
  
  lat_data_template={type:'lateral',$
                     status:-1,$
                     label:event.label,$
                     radius:0.0,$
                     latmeasind:0,$
                     wav:wav,$
                     time:time,$
                     y_rsun_array:y_rsun_array,$
                     y_arcsec_array:y_arcsec_array,$
                     x_deg_array:x_deg_array,$
                     data:dblarr(ntimes,nlateral),$
                     origdata:dblarr(ntimes,nlateral),$
                     diffdata:dblarr(ntimes,nlateral),$
                     latfitrange:[0,0],$
                     timefitrange:[0,0],$
                     fitparams:replicate({front:0.0, peak:0.0, back:0.0},3),$
                     fitsigma:replicate({front:0.0, peak:0.0, back:0.0},3),$
                     kinfittimerange:{front:[0,0],peak:[0,0],back:[0,0]},$
                     savgolfits:{front:replicate({speed:0.0,accel:0.0},nsteps),$
                                 peak:replicate({speed:0.0,accel:0.0},nsteps),$
                                 back:replicate({speed:0.0,accel:0.0},nsteps)},$
                     maxinds:intarr(1,nsteps),$
                     wave_frontedge:replicate({lat:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wave_backedge:replicate({lat:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wave_peak:replicate({lat:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wavethick:fltarr(nsteps),$
                     avgIntense:fltarr(nsteps),$
                     mymaxima:replicate({val:0.0D,ind:0L,lat:0.0D,gfit:dblarr(5),nmax:0,metric:0.D},3,ntimes)}
  
                                ;Make sure there are no negative values in the data.
  ind=where(projdata le 0.0d)
  if ind[0] ne -1 then projdata[ind]=1.0e-20 ;Set to very nearly zero
  
  lat_data={left:lat_data_template,right:lat_data_template}
  lat_data.left.type='lateral_left'
  lat_data.right.type='lateral_right'
  
  if nlatmeas gt 1 then total_lat_data=replicate(lat_data,nlatmeas) else total_lat_data=lat_data
;----------------------------
  
  
;----------------------------
;Create the base difference data
;Create a radial profile by averaging the lateral profile at the presumed source location.
;This would be a great place to put the lateral kinematics scanning functionality.
;The lateral cut index we will run. This can later be changed to a
;for-loop.
                                ;In this version, the system has only two radial heights, hard-coded relative to the overall radial range.
  for latind=0,nlatmeas-1 do begin
     lat_data=total_lat_data[latind]
     
     print,''
     print,'--------------------------------'
     print,'Measuring LATERAL profiles'
     print,'Radial height is '+strtrim(string(lat_heights[latind]),2) + ' Rs.'
     print,''
     
     ;latind=1
     radind=radinds[latind]
     lat_data.left.radius=lat_heights[latind]
     lat_data.right.radius=lat_heights[latind]

     
     origdata=total(projdata[*,*,radind-2:radind+2],3)/5.
     diffdata=origdata
     base=total(total(projdata[0:4,*,radind-2:radind+2],1)/5.0,2)/5.0
     for tt=1,nsteps-1 do begin
        diffdata[tt,*]-=base
     endfor
     
;DESPIKE THE IMAGE
     diffdata=despike_gen(diffdata)
;----------------------------

     
     
;Assign the data
     lls=0
     lle=arlonind
     lat_data.left.latfitrange=[lls,lle]
     lat_data.left.origdata[*,lls:lle]=reverse(origdata[*,lls:lle],2)
     lat_data.left.data[*,lls:lle]=reverse(diffdata[*,lls:lle],2)
     lat_data.left.diffdata[*,lls:lle]=reverse(diffdata[*,lls:lle],2) 
;reverse(tmpdata[*,nlateral-1-arlonind:nlateral-1],2)
     
     rls=0
     rle=nlateral-1-arlonind
     lat_data.right.latfitrange=[rls,rle]
     lat_data.right.origdata[*,rls:rle]=origdata[*,arlonind:nlateral-1]
     lat_data.right.data[*,rls:rle]=diffdata[*,arlonind:nlateral-1]
     lat_data.right.diffdata[*,rls:rle]=diffdata[*,arlonind:nlateral-1]
;----------------------------
     
     
;----------------------------
;PERFORM THE SAME ANALYSIS FOR THE LEFT AND RIGHT LATERAL DIRECTIONS
     
     for lateral_side=0,1 do begin
        
        if lateral_side eq 0 then begin
           lat_data_struct=lat_data.left
           
           plotinfo.imgtit='LEFT '+plotinfo.imgtit
           print,''
           print,'Measuring the LEFT LATERAL data.'
           print,'--------------------------------'
        endif
        if lateral_side eq 1 then begin
           lat_data_struct=lat_data.right
           plotinfo.imgtit='RIGHT '+plotinfo.imgtit
           print,''
           print,'Measuring the RIGHT LATERAL data.'
           print,'--------------------------------'
        endif
        
        lat_data_struct.radius=lat_heights[latind]
        lat_data_struct.latmeasind=latind+1
        
;----------------------------
;Generate the fit data by processing the base difference data
        fitdata_prep_lateral_2, lat_data_struct, plotinfo
;----------------------------



;----------------------------
;Find the wave front, back, peak positions
        annulus_get_lateral_edges, event, lat_data_struct, annulus_info, plotinfo,exit_status
          ;Define the exit statuses here:
        NORMAL_STATUS=0
        CONTINUE_STATUS=1
        QUIT_STATUS=2
        
        if exit_status eq CONTINUE_STATUS then begin
           print, ''
           if lateral_side eq 0 then begin
              print,'No wave profile was found in the LEFT lateral data.'
              print,'Continuing on to the next measurement.'
              continue
           endif else begin
              print,'No wave profile was found in the RIGHT lateral data.'
              print,'Continuing on to the next measurement.'
              continue
           endelse
        endif
        if exit_status eq QUIT_STATUS then begin
           print,'Quitting the measurement.'
           return
        endif
;----------------------------
        
        
;----------------------------
;Fit the kinematics of the front, peak, and back of the wave!
        if keyword_set(fitdata) then begin
           fit_wave_kinematics_lateral,lat_data_struct,ind_arr,/front
           fit_wave_kinematics_lateral,lat_data_struct,ind_arr,/peak
           fit_wave_kinematics_lateral,lat_data_struct,ind_arr,/back
        endif
;----------------------------


;----------------------------
;Plot some results!
        sp=lat_data_struct.timefitrange[0]
        ep=lat_data_struct.timefitrange[1]
        yrng=lat_data_struct.latfitrange
        xrng=lat_data_struct.timefitrange
        yarray=x_deg_array
        dt2=(lat_data_struct.time[1].jd-lat_data_struct.time[0].jd)/2.
        
        loadct,0,/silent
        wdef,0,1200,1000
        aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],lat_data_struct.data[*,yrng[0]:yrng[1]],$
                           min=-20,max=50,title=plotinfo.imgtit,xtitle=plotinfo.xtitle,ytitle=plotinfo.ytitle
        oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3
        oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3,color=255,linestyle=2
        oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3
        oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3,color=255,linestyle=2
        if sp eq ep then begin
           print,''
           print,"Starting and ending times are the same - can't plot or save measurements."
        endif else begin
           loadct,39,/silent
           oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_frontedge[sp:ep].lat,psym=sym(3),symsize=2,color=130
           oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_frontedge[sp:ep].lat,psym=sym(8),symsize=2,color=0,thick=3
           oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_peak[sp:ep].lat,psym=sym(1),symsize=2,color=130
           oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_peak[sp:ep].lat,psym=sym(6),symsize=2,color=0,thick=3
           oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_backedge[sp:ep].lat,psym=sym(2),symsize=2,color=130
           oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_backedge[sp:ep].lat,psym=sym(7),symsize=2,color=0,thick=3
           loadct,0,/silent
           
           strh=strtrim(string(latind+1),2)
           if strh lt 10 then strh='0'+strh
           if lateral_side eq 0 then begin
              plot_fname=replace_string(event.annplot.analyzed.lateral.leftplot_savename,'SSSSS','_r'+strh+'_m'+analysis_run_id)
              plot_fname=replace_string(plot_fname,'WAV',wav)
           endif 
           if lateral_side eq 1 then begin
              plot_fname=replace_string(event.annplot.analyzed.lateral.rightplot_savename,'SSSSS','_r'+strh+'_m'+analysis_run_id)
              plot_fname=replace_string(plot_fname,'WAV',wav)
           endif
           write_png,savepath+plot_fname,tvrd(/true)
           wait,1
           wdel,0
;----------------------------
        endelse
        
;Write back into the main structure
        lat_data_struct.status=1
        if lateral_side eq 0 then lat_data.left=lat_data_struct
        if lateral_side eq 1 then lat_data.right=lat_data_struct

     endfor ;END LOOP OVER LATERAL_SIDE
     
     total_lat_data[latind]=lat_data
  endfor ;END LOOP OVER NLATMEAS
  

  save_fname=replace_string(event.annplot.analyzed.lateral.savename,'SSSSS','_m'+analysis_run_id)
  save_fname=replace_string(save_fname,'WAV',wav)  
  
  if file_exist(savepath+save_fname) then begin
     uinput=''
     print,''
     print,'File '+save_fname+' already exists.'
     read,uinput,prompt='Overwrite? (y for yes, n for no): '
     if uinput eq 'n' then return
  endif
  save,filename=savepath+save_fname,total_lat_data,ind_arr,annulus_info,event
end
;-============================================================================

