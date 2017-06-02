;+============================================================================
pro test_cashew_annulus_analyze_lateral
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     labels=['110511_01']
     labels=['120806_01','140220_02']
     labels=['120307_01','120405_01','151104_01','151104_02','151104_03']
     labels=['test_cashew']
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
                                ;rrange=[1.1,1.34]
        cashew_annulus_analyze_lateral,event,wave=wav,/constrain,/auto ;,rrange=rrange ;,/interactive
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
           cashew_annulus_analyze_lateral,event,wave=wavelength,rrange=rrange,/constrain ;,/interactive
        endfor
     endfor
  endif
end
;-============================================================================



;+============================================================================
function jmap_filter_maxima_lateral,lat_data,fitrange=fitrange
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
  TIME_FACTOR=3600.
  DIST_FACTOR=(lat_data.radius-1.)*RSUN*2.*!PI/360. ;The km/deg at the current height
  vlimit=[10,1500.0]                                ;Position derivative max limit, in km/s.
  
  time=lat_data.time.relsec
  height=lat_data.x_deg_array*DIST_FACTOR
  wave_maxima=lat_data.mymaxima
  
                                ;Set the time range so we only look at the user-selected time range.
  if keyword_set(fitrange) then tr=fitrange else tr=lat_data.timefitrange
  rind=reform(wave_maxima[0,tr[0]:tr[1]].ind)
  if n_elements(rind) lt 2 then begin
     print, 'Not enough time steps to check the maxima for physicality. Quitting.'
     return,wave_maxima
  endif
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
     
     wave_maxima[0,tr[0]+tt+1].lat=height[wave_maxima[0,tr[0]+tt+1].ind]/DIST_FACTOR
     
;If there has been a change in the position, find the nearest larger height and
;assign its index to the maxinds structure.
     if change ne 0 then begin
        wave_maxima[0,tr[0]+tt+1].ind=min(where(height-ht[tt+1] gt 0.))
        wave_maxima[0,tr[0]+tt+1].lat=ht[tt+1]/DIST_FACTOR
     endif

     change=0
  endfor
  
  return,wave_maxima
end
;-============================================================================



;+============================================================================
pro annulus_get_lateral_edges,event,lat_data;,annulus_info,plotinfo
  RSUN=6.96e5                   ;Solar radius in km.
  TIME_FACTOR=3600.
  DIST_FACTOR=RSUN
  y_deg_array=lat_data.x_deg_array
  time=lat_data.time
  nt=n_elements(time)
  yrng=lat_data.latfitrange

  
;Find the starting and ending time indices for which we believe we have a wave to fit.
  find_start_end_lateral, lat_data

  startInd=lat_data.timefitrange[0]
  endInd=lat_data.timefitrange[1]
  print, "Initial start index: ", startInd
  print, "Start Time: ", time[startInd].cashew_time
  print, "Initial end index: ", endInd
  print, "End Time: ", time[endInd].cashew_time
  
;Find the local intensity maxima in the J-map data
  aia_jmap_find_maxima_lateral,lat_data,yrange=[y_deg_array[yrng[0]],y_deg_array[yrng[1]]]
  
;Filter the maxima positions here for physicality
  maxinds=jmap_filter_maxima_lateral(lat_data)
  mymaxima=maxinds
  
; Find the front and back edges of the wave at each time step,
; using gaussian MPFITPEAK routine.
  find_wave_edge_lateral, lat_data
end
;-============================================================================




;+============================================================================
pro cashew_annulus_analyze_lateral,event,datapath=datapath,savepath=savepath,thrange=thrange,$
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


;----------------------------
;Restore the file with the deprojected data
  date=event.date
  
  if not keyword_set(wave) then wav='193' else wav=wave
  if not keyword_set(savepath) then savepath=event.annuluspath
  if not keyword_set(datapath) then datapath=savepath
  
  fname=replace_string(event.annplot.savename,'WAV',wav)
  restore, datapath+fname
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
     if event.arlon lt 0.0 then arlat=270.0+event.arlat $
     else arlat=90.0-event.arlat
  endif else arlat=centerlat
  
;The latitudinal index of the AR
  arlatind=min(where(x_deg_array ge arlat))
  limb=1                        ;Limb position in Rsun
  limbind=min(where(y_rsun_array ge limb))
  lat_heights=(limb+[0.05,0.35,0.65]*(max(y_rsun_limits)-limb))
  nlatmeas=n_elements(lat_heights)
  radinds=intarr(3)
  for ls=0,nlatmeas-1 do radinds[ls]=min(where(y_rsun_array ge lat_heights[ls]))

                                ;For the lateral measurements, get a map of the good lateral
;pixel values - those where there is data. This will only be relevant
;if taking lateral measurements high up.
  x_good_lats=aia_deg_tan_limits(x_deg_array,y_rsun_array,y_rsun_limits,lat_heights)
  
;Find the Y-radial outward limit/edge for which there's data
  yradlimit=aia_rad_height_limits(ind_arr[0],angle=arlat,/rsun)
  yradlimind=max(where(y_rsun_array le yradlimit))
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
            imgtit:'AIA/'+wav+' BDiff Radial Positions, event '+event.label,$
            savename:replace_string(replace_string(event.annplot.analyzed.radial.plot_savename,'WAV',wav),'SSSSS','_auto')}
  
  lat_data_template={type:'lateral',$
                     label:event.label,$
                     radius:0.0,$
                     wav:wav,$
                     time:time,$
                     y_rsun_array:y_rsun_array,$
                     y_arcsec_array:y_arcsec_array,$
                     x_deg_array:x_deg_array,$
                     data:dblarr(ntimes,nlateral),$
                     corrected_data:dblarr(ntimes,nlateral),$
                     latfitrange:[0,0],$
                     timefitrange:[0,0],$
                     fitparams:replicate({front:0.0, peak:0.0, back:0.0},3),$
                     fitsigma:replicate({front:0.0, peak:0.0, back:0.0},3),$
                     kinfittimerange:{front:[0,0],peak:[0,0],back:[0,0]},$
                     savgolfits:{front:replicate({speed:0.0,accel:0.0},nsteps),$
                                 peak:replicate({speed:0.0,accel:0.0},nsteps),$
                                 back:replicate({speed:0.0,accel:0.0},nsteps)},$
                     maxinds:intarr(1,nsteps),$
                     wave_frontedge:replicate({lat:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wave_backedge:replicate({lat:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wave_peak:replicate({lat:0.0D, val:0.0D, yind:0L, xind:0L},nsteps),$
                     wavethick:fltarr(nsteps),$
                     avgIntense:fltarr(nsteps)}
  
                                ;Make sure there are no negative values in the data.
  ind=where(projdata le 0.0d)
  if ind[0] ne -1 then projdata[ind]=1.0e-20 ;Set to very nearly zero
  
  lat_data={left:lat_data_template,right:lat_data_template}
;----------------------------
  
  
;----------------------------
;Create the base difference data
;Create a radial profile by averaging the lateral profile at the presumed source location.
;This would be a great place to put the lateral kinematics scanning functionality.
                                ;The lateral cut index we will run. This can later be changed to a for-loop.
  latind=1
  radind=radinds[latind]
  lat_data.left.radius=lat_heights[latind]
  lat_data.right.radius=lat_heights[latind]
  
  tmpdata=total(projdata[*,*,radind-2:radind+2],3)/5.
  base=total(total(projdata[0:4,*,radind-2:radind+2],1)/5.0,2)/5.0
  for tt=1,nsteps-1 do begin
    ;tmpdata[tt,yradlimind+1:*]=base[yradlimind+1:*]
     tmpdata[tt,*]-=base
  endfor
  
;DESPIKE THE IMAGE
  tmp=despike_gen(tmpdata)
  tmpdata=tmp
;----------------------------

;Assign the data
  lls=0
  lle=arlatind
  lat_data.left.latfitrange=[lls,lle]
  lat_data.left.data[*,lls:lle]=reverse(tmpdata[*,lls:lle],2) ;reverse(tmpdata[*,nlateral-1-arlatind:nlateral-1],2)
  rls=0
  rle=nlateral-1-arlatind
  lat_data.right.latfitrange=[rls,rle]
  lat_data.right.data[*,rls:rle]=tmpdata[*,arlatind:nlateral-1]
;----------------------------
  

;----------------------------
;PERFORM THE SAME ANALYSIS FOR THE LEFT AND RIGHT LATERAL DIRECTIONS
  
  for ii=0,i do begin

     if ii eq 0 then lat_data_struct=lat_data.left
     if ii eq 1 then lat_data_struct=lat_data.right
;----------------------------
;Process the data further to remove constant features and enhance the
;signal.                          
     bin_aia_data,lat_data_struct,binData=binData
     make_gradient_map_lateral, lat_data_struct, binData, intensityData=intensityData
     mask_gradient_data, lat_data_struct, intensityData,correctData = correctData
     lat_data_struct.data=intensityData

     annulus_get_lateral_edges, event, lat_data_struct
;---------------------------- 


;----------------------------
;Find the wave front, back, peak positions
     annulus_get_lateral_edges, event, lat_data_struct ;, gradient=gradient
;----------------------------
     
     
;----------------------------
;Fit the kinematics of the front, peak, and back of the wave!
     fit_wave_kinematics,lat_data_struct,ind_arr,/front
     fit_wave_kinematics,lat_data_struct,ind_arr,/peak
     fit_wave_kinematics,lat_data_struct,ind_arr,/back
;----------------------------

;----------------------------
;Plot some results!
     sp=lat_data_struct.timefitrange[0]
     ep=lat_data_struct.timefitrange[1]
     yrng=lat_data_struct.latfitrange
     xrng=lat_data_struct.timefitrange
     yarray=lat_data_struct.y_rsun_array
     dt2=(lat_data_struct.time[1].jd-lat_data_struct.time[0].jd)/2.

     loadct,0,/silent
     wdef,0,1200,1000
     aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],lat_data_struct.data[*,yrng[0]:yrng[1]],min=-10,max=20,title=plotinfo.imgtit,xtitle=plotinfo.xtitle,ytitle=plotinfo.ytitle
     oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3
     oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3,color=255,linestyle=2
     oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3
     oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=3,color=255,linestyle=2
     loadct,39,/silent
     oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_frontedge[sp:ep].rad,psym=sym(3),symsize=2,color=130
     oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_frontedge[sp:ep].rad,psym=sym(8),symsize=2,color=0,thick=3
     oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_peak[sp:ep].rad,psym=sym(1),symsize=2,color=130
     oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_peak[sp:ep].rad,psym=sym(6),symsize=2,color=0,thick=3
     oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_backedge[sp:ep].rad,psym=sym(2),symsize=2,color=130
     oplot,time[sp:ep].jd+dt2,lat_data_struct.wave_backedge[sp:ep].rad,psym=sym(7),symsize=2,color=0,thick=3
     loadct,0,/silent
     fname_plot=replace_string(event.annplot.analyzed.radial.plot_savename,'WAV',wav)
     write_png,savepath+replace_string(fname,'SSSSS',''),tvrd(/true)
     
     ;Write back into the main structure
     if ii eq 0 then lat_data.left=lat_data_struct
     if ii eq 1 then lat_data.right=lat_data_struct
     stop
  endfor
;----------------------------
  

  save,filename=savepath+replace_string(event.annplot.analyzed.radial.savename,'WAV',wav),lat_data,ind_arr,annulus_info,event

end
;-============================================================================




;+============================================================================
pro plot_annulus_context,projdata,annulus_info,index,event,lat_heights,goodlats
;Plot the annulus plot for context
  nsteps=n_elements(projdata[*,0,0])
  context_step=fix(nsteps/3.)
  x_deg_array = annulus_info.thcoords*180./!PI
  arlat=90.0-event.arlat
  nlatmeas=n_elements(lat_heights)
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position=[0.2,0.2,0.9,0.9]
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  
  wdef,0,1200,800
  img=reform(projdata[context_step,*,*]-projdata[context_step-1,*,*])
  plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
              ytitle = '!5Radial distance from Sun center [R!DS!N]', $
              title = 'Annulusplot, AIA/'+annulus_info.wav+' '+event.date, max =50, $
              origin = [annulus_info.thcoords[0]*180./!PI,annulus_info.rcoords[0]], charthick = 1.2, charsize=3,$
              scale = [annulus_info.dtheta*180./!PI, annulus_info.dr], $
              pos = [0.16, 0.16, 0.95, 0.9], min = -50
  
;For the plotting, find the radial limits of the data, and overplot them.
  y_rsun_limits=aia_rad_height_limits(index,degarray=x_deg_array,/rsun)
  oplot,x_deg_array,y_rsun_limits,color=255,thick=2
  
;Overplot the radial measurement location
  yradlimit=aia_rad_height_limits(index,angle=arlat,/rsun)
  oplot,[arlat,arlat],[annulus_info.rcoords[0],yradlimit],thick=2
  
;Overplot the lateral measurements location
  for ii=0,nlatmeas-1 do $
     plots,x_deg_array,goodlats[ii,*],psym=1,thick=2,symsize=0.2

;Overplot the limb position
                                ;oplot,thrang*180./!PI,[limb,limb],linestyle=2,thick=2,color=255 
  
;Save the overview plot
  write_png,event.annuluspath+replace_string(event.annplot.overviewplot_savename,'WAV',annulus_info.wav),$
            tvrd(/true),ct_rr,ct_gg,ct_bb
end
;-============================================================================




;+============================================================================
pro fit_wave_kinematics,lat_data,ind_arr,front=front,peak=peak,back=back
;PERFORM THE TWO KINDS OF KINEMATICS FITTING PROCEDURES - 2nd ORDER
;POLYNOMIAL AND SAVITZKY-GOLAY!
  if (not keyword_set(front)) and (not keyword_set(peak)) and (not keyword_set(back)) then begin
     print,'No fitting done. Rerun with one of keywords /front, /peak, /back. Quitting.'
     return
  endif
                                ;This is the time to use for the fitting, in seconds
  sp=lat_data.timefitrange[0]
  ep=lat_data.timefitrange[1]
  time_good=lat_data.time[sp:ep].relsec-lat_data.time[sp].relsec
  dtsec=(lat_data.time[1].relsec-lat_data.time[0].relsec)
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  
                                ;The info to pass to the position fitting routine for the fitting parameters
  parinfo = replicate({value:0.D, limited:[0,0], limits:[0.D,0.D]}, 3)
  parinfo[0].value=1.0          ;*DIST_FACTOR
  parinfo[0].limited=[1,1]
  parinfo[0].limits=[1.01,1.2]  ;*DIST_FACTOR
  
  parinfo[1].value=100.
  parinfo[1].limited=[1,1]
  parinfo[1].limits=[0.0,2000.0]
  parinfo[2].value=10.
  parinfo[2].limited[0]=1
  parinfo[2].limits[0]=-1500.0
  
  if keyword_set(front) then begin
     print,''
     print,'Fitting a second-order polynomial and a Savitzky-Golay filter to the wave front edge positions...' 
                                ;Do the fitting of the front edge positions
     dist=reform(lat_data.wave_frontedge[sp:ep].rad)
  endif
  if keyword_set(peak) then begin
     print,''
     print,'Fitting a second-order polynomial and a Savitzky-Golay filter to the wave peak positions...' 
                                ;Do the fitting of the front edge positions
     dist=reform(lat_data.wave_peak[sp:ep].rad)
  endif
  if keyword_set(back) then begin
     print,''
     print,'Fitting a second-order polynomial and a Savitzky-Golay filter to the wave front edge positions...' 
                                ;Do the fitting of the front edge positions
     dist=reform(lat_data.wave_backedge[sp:ep].rad)
  endif
  
;Check if the values are zero or if there are repeating values
  zeros=where(dist eq 0,complement=nonzero)
  if zeros[0] ne -1 then begin
     dist=dist[nonzero]
     time_good=time_good[nonzero]
  endif
  repeating=where(dist eq shift(dist,1),complement=notrepeating)
  if repeating[0] ne -1 then begin
     dist=dist[notrepeating]
     time_good=time_good[notrepeating]
  endif
  
                                ;This is very important - record the overall time index range for this fit.
  if keyword_set(front) then begin
     lat_data.kinfittimerange.front=sp+minmax(notrepeating)
     stfit=lat_data.kinfittimerange.front[0]
     etfit=lat_data.kinfittimerange.front[1]
  endif
  if keyword_set(peak) then begin
     lat_data.kinfittimerange.peak=sp+minmax(notrepeating)
     stfit=lat_data.kinfittimerange.peak[0]
     etfit=lat_data.kinfittimerange.peak[1]
  endif
  if keyword_set(back) then begin
     lat_data.kinfittimerange.back=sp+minmax(notrepeating)
     stfit=lat_data.kinfittimerange.back[0]
     etfit=lat_data.kinfittimerange.back[1]
  endif
  
  if n_elements(dist) le 4 then begin
     print, "Not enough data to smooth, exiting..."
     return
  endif
  newtimegood=time_good
  bootstrap_sdo,dist*RSUN,newtimegood,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo
  wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
  wave_fits/=RSUN
  if keyword_set(front) then begin
     lat_data.fitparams[0].front=p1[0]
     lat_data.fitparams[1].front=p2[0]
     lat_data.fitparams[2].front=p3[0]
     lat_data.fitsigma[0].front=s1[0]
     lat_data.fitsigma[1].front=s2[0]
     lat_data.fitsigma[2].front=s3[0]
  endif
  if keyword_set(peak) then begin
     lat_data.fitparams[0].peak=p1[0]
     lat_data.fitparams[1].peak=p2[0]
     lat_data.fitparams[2].peak=p3[0]
     lat_data.fitsigma[0].peak=s1[0]
     lat_data.fitsigma[1].peak=s2[0]
     lat_data.fitsigma[2].peak=s3[0]
  endif
  if keyword_set(back) then begin
     lat_data.fitparams[0].back=p1[0]
     lat_data.fitparams[1].back=p2[0]
     lat_data.fitparams[2].back=p3[0]
     lat_data.fitsigma[0].back=s1[0]
     lat_data.fitsigma[1].back=s2[0]
     lat_data.fitsigma[2].back=s3[0]
  endif
  
                                ;DO SAVITZKY-GOLAY FITS!
  sgfpix=4
                                ;SPEED
  order=1
  sgfil_v = SAVGOL(sgfpix, sgfpix, order, 4)*(FACTORIAL(order)/(dtsec^order))
  speed=CONVOL(dist*RSUN, sgfil_v, /EDGE_TRUNCATE)
  
                                ;ACCELERATION
  order=2
  sgfil_a = SAVGOL(sgfpix, sgfpix, order, 4)*(FACTORIAL(order)/(dtsec^order))
  accel=CONVOL(dist*RSUN*1000., sgfil_a, /EDGE_TRUNCATE)

  if keyword_set(front) then begin
     lat_data.savgolfits.front[stfit:etfit].speed=speed
     lat_data.savgolfits.front[stfit:etfit].accel=accel
  endif 
  if keyword_set(peak) then begin
     lat_data.savgolfits.peak[stfit:etfit].speed=speed
     lat_data.savgolfits.peak[stfit:etfit].accel=accel
  endif
  if keyword_set(back) then begin
     lat_data.savgolfits.back[stfit:etfit].speed=speed
     lat_data.savgolfits.back[stfit:etfit].accel=accel
  endif 
end
;-============================================================================
;------------------------------
