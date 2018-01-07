pro test_calculate_lateral_kinematics_results
;A little program to test the calculate_radial_results procedure
  events=['100613_01','101103_02','101231_01','110125_01','110211_02','110327_01','110515_01','110607_01','110904_01','120307_01','120424_01','120526_01','120728_01','120915_01','121007_01','130423_01','130501_01','130517_01','131107_01','131207_01','131212_01','140217_01','140708_01','140901_01','141105_02','141205_01','150303_01','150421_01','150920_01','151109_01']
  events=['100613_01','101103_02']
  events=['110607_01','131212_01']
  events=['151104_01']
  ;events = ['131212_01']
  nevents=n_elements(events)
  for ev=0,nevents-1 do begin
     event_label=events[ev]
     event=load_events_info(label=event_label)
     calculate_lateral_kinematics_results,event,/ps;,/force
  endfor
end




pro calculate_lateral_kinematics_results,event,ps=ps,force=force

;PURPOSE:
;Analyze data from the 10 manually created runs for annulus data,
;and then create an averaged lateral position to plot, along
;with the kinematic information from the averaged values
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      n/a
;
;KEYWORDS:  
;
;OUTPUTS:
;Creates save file with data
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Celeste Keith, 07/18/2016
;Udated by Kamen Kozarev, 05/10/2017 - reorganized the code, created
;                                      nice plotting


;---------------Loads part of the event---------------------------------
  wav='193'
  savepath=event.annuluspath
  save_fname=replace_string(event.annplot.analyzed.lateral.savename,'WAV',wav)
  save_fname=replace_string(save_fname,'SSSSS','_mxxxx')
  

;--------------------------AVERAGES AND STANDARD DEVIATIONS------------------------
;nmeas needs to be = N-1 once every event has N meas to their names
  search_fname = event.annuluspath + 'annplot_'+strtrim(event.date)+'_'+strtrim(event.label)+'_'+wav+'_analyzed_lateral_m*.sav'
  fnames=file_search(search_fname)
  nmeas = n_elements(fnames)
  if nmeas lt 3 then begin
     print,''
     print,'Error: Not enough measurements to average for this event!'
     print,'Run a measurement with procedure cashew_annulus_analyze_lateral.'
     print,'Quitting...'
     print,''
     return
  endif
  ;Defining some values for later 
  restore, fnames[0]
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  numrad=n_elements(total_lat_data)
                                ;SEPARATE THE DIFFERENT DIRECTIONS AND RADII
  lat_data=total_lat_data[0].right
  alllat_data=replicate(lat_data,nmeas,numrad,2)
  lat_data_average=replicate(lat_data,numrad,2)
  
  LEFTDIR=0
  RIGHTDIR=1
  VERYSMALL=1.e-33
  numdirs=2
  
;Then you load the data for each measurement, and so you loop over
;measurements:
  resave=0
  for mm=0,nmeas-1 do begin
     restore, fnames[mm]
     for rr=0,numrad-1 do begin
        for dir=LEFTDIR,RIGHTDIR do begin
           if dir eq LEFTDIR then lat_data=total_lat_data[rr].left
           if dir eq RIGHTDIR then lat_data=total_lat_data[rr].right
           alllat_data[mm,rr,dir]=lat_data
           if lat_data.status eq -1 then continue
           if keyword_set(force) or (mean(lat_data.savgolfits.back.speed) eq 0. or $
              mean(lat_data.savgolfits.front.speed) eq 0. or $
              mean(lat_data.savgolfits.peak.speed) eq 0.) then begin
              fit_wave_kinematics_lateral,lat_data,ind_arr,/front
              fit_wave_kinematics_lateral,lat_data,ind_arr,/peak
              fit_wave_kinematics_lateral,lat_data,ind_arr,/back
              resave=1
           endif
           alllat_data[mm,rr,dir]=lat_data
           if resave gt 0 then begin
              if dir eq LEFTDIR then total_lat_data[rr].left=lat_data
              if dir eq RIGHTDIR then total_lat_data[rr].right=lat_data
           endif
        endfor
     endfor
     if resave gt 0 then save,filename=fnames[mm],total_lat_data,ind_arr,annulus_info
  endfor
  
  
  ntotaltimes=n_elements(alllat_data[0,0,0].data[*,0])
  stdintensity=fltarr(ntotaltimes)
  wav_thick_std=fltarr(ntotaltimes)
  stdbackaccel=fltarr(ntotaltimes)
  stdpeakaccel=fltarr(ntotaltimes)
  stdfrontaccel=fltarr(ntotaltimes)
  stdbackspeed=fltarr(ntotaltimes)
  stdpeakspeed=fltarr(ntotaltimes)
  stdfrontspeed=fltarr(ntotaltimes)
  lat_stdv=replicate({front:replicate({speed:0.,accel:0.},ntotaltimes),$
                      peak:replicate({speed:0.,accel:0.},ntotaltimes),$
                      back:replicate({speed:0.,accel:0.},ntotaltimes),$
                      intensity:fltarr(ntotaltimes),wav_thick:fltarr(ntotaltimes)},numrad,2)
  
  frontrad=fltarr(ntotaltimes)
  frontstdv=fltarr(ntotaltimes)
  peakrad=fltarr(ntotaltimes)
  peakstdv=fltarr(ntotaltimes)
  backrad=fltarr(ntotaltimes)
  backstdv=fltarr(ntotaltimes)
  rad_from_lat = replicate({front:frontrad, frontstdv:frontstdv, peak:peakrad, peakstdv:peakstdv, back:backrad, backstdv:backstdv},numrad,numdirs)
  
  for rr=0,numrad-1 do begin
     for dir=LEFTDIR,RIGHTDIR do begin
        lat_data=alllat_data[0,rr,dir]
        print,rr,dir,lat_data.radius
        lat_data_average[rr,dir]=lat_data
        ;print,lat_data_average[rr,dir].radius
        if lat_data.status eq -1 then continue
        xrng=lat_data.timefitrange
        sp=lat_data.timefitrange[0]
        ep=lat_data.timefitrange[1]
        yrng=lat_data.latfitrange
        time=lat_data.time
        yarray=lat_data.x_deg_array
        yarray_offset=yarray[0]
        yarray-=yarray[0]
        dt2=(lat_data.time[1].jd-lat_data.time[0].jd)/2.
        ntimes=n_elements(lat_data.data[*,0])
        
;Finds the mean and stddev of all the events
        
        for tt=0,ntimes-1 do begin
           lat_data_average[rr,dir].wave_frontedge[tt].lat=mean(alllat_data[*,rr,dir].wave_frontedge[tt].lat) - yarray_offset
           lat_data_average[rr,dir].wave_frontedge[tt].stdv=stddev(alllat_data[*,rr,dir].wave_frontedge[tt].lat)
           lat_data_average[rr,dir].wave_backedge[tt].lat=mean(alllat_data[*,rr,dir].wave_backedge[tt].lat) - yarray_offset
           lat_data_average[rr,dir].wave_backedge[tt].stdv=stddev(alllat_data[*,rr,dir].wave_backedge[tt].lat)
           lat_data_average[rr,dir].wave_peak[tt].lat=mean(alllat_data[*,rr,dir].wave_peak[tt].lat) - yarray_offset
           lat_data_average[rr,dir].wave_peak[tt].stdv=stddev(alllat_data[*,rr,dir].wave_peak[tt].lat)
           
           lat_data_average[rr,dir].avgIntense[tt]=mean(alllat_data[*,rr,dir].avgIntense[tt])
           stdintensity[tt]=stddev(alllat_data[*,rr,dir].avgIntense[tt])
           lat_data_average[rr,dir].wavethick[tt]=mean(alllat_data[*,rr,dir].wavethick[tt])
           wav_thick_std[tt]=stddev(alllat_data[*,rr,dir].wavethick[tt])
           
           lat_data_average[rr,dir].savgolfits.back[tt].accel=mean(alllat_data[*,rr,dir].savgolfits.back[tt].accel)
           stdbackaccel[tt]=stddev(alllat_data[*,rr,dir].savgolfits.back[tt].accel)
           lat_data_average[rr,dir].savgolfits.peak[tt].accel=mean(alllat_data[*,rr,dir].savgolfits.peak[tt].accel)
           stdpeakaccel[tt]=stddev(alllat_data[*,rr,dir].savgolfits.peak[tt].accel)
           lat_data_average[rr,dir].savgolfits.front[tt].accel=mean(alllat_data[*,rr,dir].savgolfits.front[tt].accel)
           stdfrontaccel[tt]=stddev(alllat_data[*,rr,dir].savgolfits.front[tt].accel)
           lat_data_average[rr,dir].savgolfits.back[tt].speed=mean(alllat_data[*,rr,dir].savgolfits.back[tt].speed)
           stdbackspeed[tt]=stddev(alllat_data[*,rr,dir].savgolfits.back[tt].speed)
           lat_data_average[rr,dir].savgolfits.peak[tt].speed=mean(alllat_data[*,rr,dir].savgolfits.peak[tt].speed)
           stdpeakspeed[tt]=stddev(alllat_data[*,rr,dir].savgolfits.peak[tt].speed)
           lat_data_average[rr,dir].savgolfits.front[tt].speed=mean(alllat_data[*,rr,dir].savgolfits.front[tt].speed)
           stdfrontspeed[tt]=stddev(alllat_data[*,rr,dir].savgolfits.front[tt].speed)
        endfor
        
        ;Record the standard deviations
        lat_stdv[rr,dir].front.speed = stdfrontspeed
        lat_stdv[rr,dir].front.accel = stdfrontaccel
        lat_stdv[rr,dir].peak.speed = stdpeakspeed
        lat_stdv[rr,dir].peak.accel = stdpeakaccel
        lat_stdv[rr,dir].back.speed = stdbackspeed
        lat_stdv[rr,dir].back.accel = stdbackaccel
        lat_stdv[rr,dir].intensity = stdintensity
        lat_stdv[rr,dir].wav_thick = wav_thick_std
        
        event=load_events_info(label=event.label)
        timesec = lat_data_average[rr,dir].time.relsec
        timejd=lat_data_average[rr,dir].time.jd
        avgintensity = lat_data_average[rr,dir].avgIntense
        wav_thick = lat_data_average[rr,dir].wavethick
        
;----------Convert the latitudinal positions to perceived radial distance-----------
        gamma = lat_data_average[rr,dir].wave_frontedge[sp:ep].lat * !PI/180.
        gammastdv = lat_data_average[rr,dir].wave_frontedge[sp:ep].stdv * !PI/180.
        radius = lat_data_average[rr,dir].radius
        frontrad[sp:ep] = radius*sqrt(2*(1-cos(gamma))); + 1.
        frontstdv[sp:ep] = abs( ( frontrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma)+VERYSMALL)))
        frontrad[sp:ep] +=1.
        
        gamma = lat_data_average[rr,dir].wave_peak[sp:ep].lat * !PI/180.
        gammastdv = lat_data_average[rr,dir].wave_peak[sp:ep].stdv * !PI/180.
        radius = lat_data_average[rr,dir].radius
        peakrad[sp:ep] = radius*sqrt(2*(1-cos(gamma))) ;+ 1.
        peakstdv[sp:ep] = abs( ( peakrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma)+VERYSMALL)))
        peakrad[sp:ep] +=1.
        
        gamma = lat_data_average[rr,dir].wave_backedge[sp:ep].lat * !PI/180.
        gammastdv = lat_data_average[rr,dir].wave_backedge[sp:ep].stdv * !PI/180.
        radius = lat_data_average[rr,dir].radius
        backrad[sp:ep] = radius*sqrt(2*(1-cos(gamma))) ;+ 1.
        backstdv[sp:ep] = abs( ( backrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma)+VERYSMALL)))
        backrad[sp:ep] +=1.
        
        rad_from_lat[rr,dir] = {front:frontrad, frontstdv:frontstdv, peak:peakrad, peakstdv:peakstdv, back:backrad, backstdv:backstdv}
        
        
        
        
;----------Finding plotting range for acceleration--------------------
        accel_array_front=lat_data_average[rr,dir].savgolfits.front[sp:ep].accel
        accel_array_front_max = max(accel_array_front+stdfrontaccel[sp:ep])
        accel_array_front_min = min(accel_array_front[where(accel_array_front ne 0.)]-stdfrontaccel[sp:ep])
        accel_array_peak = lat_data_average[rr,dir].savgolfits.peak[sp:ep].accel
        accel_array_peak_max = max(accel_array_peak+stdpeakaccel[sp:ep])
        accel_array_peak_min = min(accel_array_peak[where(accel_array_peak ne 0.)]-stdpeakaccel[sp:ep])
        accel_array_back = lat_data_average[rr,dir].savgolfits.back[sp:ep].accel
        accel_array_back_max = max(accel_array_back+stdbackaccel[sp:ep])
        accel_array_back_min = min(accel_array_back[where(accel_array_back ne 0.)]-stdbackaccel[sp:ep])
        max_array = [accel_array_front_max,accel_array_peak_max,accel_array_back_max]
        min_array = [accel_array_front_min, accel_array_peak_min, accel_array_back_min]
        yrnge_accel = [min(min_array), max(max_array)]
        
;----------Finding plotting range for speed--------------------
        speed_array_front =lat_data_average[rr,dir].savgolfits.front[sp:ep].speed
        speed_array_front_max = max(speed_array_front+stdfrontspeed[sp:ep])
        speed_array_front_min = min(speed_array_front[where(speed_array_front ne 0.)]-stdfrontspeed[sp:ep])
        speed_array_peak = lat_data_average[rr,dir].savgolfits.peak[sp:ep].speed
        speed_array_peak_max = max(speed_array_peak+stdpeakspeed[sp:ep])
        speed_array_peak_min = min(speed_array_peak[where(speed_array_peak ne 0.)]-stdpeakspeed[sp:ep])
        speed_array_back = lat_data_average[rr,dir].savgolfits.back[sp:ep].speed
        speed_array_back_max = max(speed_array_back+stdbackspeed[sp:ep])
        speed_array_back_min = min(speed_array_back[where(speed_array_back ne 0.)]-stdbackspeed[sp:ep])
        max_array = [speed_array_front_max,speed_array_peak_max,speed_array_back_max]
        min_array = [speed_array_front_min, speed_array_peak_min, speed_array_back_min]
        yrnge_speed = [min(min_array), max(max_array)]
        
;----------Finding plotting range for latitude position--------------------
        pos_array_peak = lat_data_average[rr,dir].wave_peak[sp:ep].lat
        pos_array_peak_max = max(pos_array_peak+lat_data_average[rr,dir].wave_peak[sp:ep].stdv)
        pos_array_peak_min = min(pos_array_peak[where(pos_array_peak ne 0.)]-lat_data_average[rr,dir].wave_peak[sp:ep].stdv)
        pos_array_front = lat_data_average[rr,dir].wave_frontedge[sp:ep].lat
        pos_array_front_max = max(pos_array_front+lat_data_average[rr,dir].wave_frontedge[sp:ep].stdv)
        pos_array_front_min = min(pos_array_front[where(pos_array_front ne 0.)]-lat_data_average[rr,dir].wave_frontedge[sp:ep].stdv)
        pos_array_back = lat_data_average[rr,dir].wave_backedge[sp:ep].lat
        pos_array_back_max = max(pos_array_back+lat_data_average[rr,dir].wave_backedge[sp:ep].stdv)
        pos_array_back_min = min(pos_array_back[where(pos_array_back ne 0.)]-lat_data_average[rr,dir].wave_backedge[sp:ep].stdv)
        max_array = [pos_array_front_max,pos_array_peak_max,pos_array_back_max]
        min_array = [pos_array_front_min, pos_array_peak_min, pos_array_back_min]
        
        
;----------Finding plotting range for radial position--------------------
        pos_array_peak = rad_from_lat.peak
        pos_array_peak_max = max(pos_array_peak+rad_from_lat.peakstdv)
        pos_array_peak_min = min(pos_array_peak[where(pos_array_peak ne 0.)]-rad_from_lat.peakstdv)
        pos_array_front = rad_from_lat.front
        pos_array_front_max = max(pos_array_front+rad_from_lat.frontstdv)
        pos_array_front_min = min(pos_array_front[where(pos_array_front ne 0.)]-rad_from_lat.frontstdv)
        pos_array_back = rad_from_lat.back
        pos_array_back_max = max(pos_array_back+rad_from_lat.backstdv)
        pos_array_back_min = min(pos_array_back[where(pos_array_back ne 0.)]-rad_from_lat.backstdv)
        max_array = [pos_array_front_max,pos_array_peak_max,pos_array_back_max]
        min_array = [pos_array_front_min, pos_array_peak_min, pos_array_back_min]      
        yrnge_rad = [min(min_array), max(max_array)]
        
;----------Creating the x range--------------
;This may need updating in the future
        xrnge = [lat_data.time[sp].jd, lat_data.time[ep].jd] 
        dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
        
;=====================================================================================
;           PLOT THE AVERAGED RADIAL POSITIONS GRAPH
;=====================================================================================
        set_plot,'x'
        if dir eq LEFTDIR then $
           plotfname=event.annuluspath+'avg_lat_pos_'+strtrim(event.label)+'_r'+strtrim(string(rr+1),2)+'_left'
        if dir eq RIGHTDIR then $
           plotfname=event.annuluspath+'avg_lat_pos_'+strtrim(event.label)+'_r'+strtrim(string(rr+1),2)+'_right'
        plotpng=plotfname+'.png'
        ploteps=plotfname+'.eps'
       
        
        if keyword_set(ps) then begin
           !p.position=[0.15,0.12,0.93,0.9]
           !p.background=255
           !p.color=0
           !p.font=0
           set_plot,'ps'
           device,file=ploteps,/inches,xsize=11,ysize=9,$
                  /encaps,/color,/helvetica
           
           !p.multi =[0, 5, 1]
           
        endif else begin
           !p.background=255
           !p.color=0
           !p.font=-1
           wdef,0,1200,1000
        endelse
        
        loadct,0,/silent
        if tag_exist(lat_data,'diffdata') then begin
           sm=smooth(lat_data.diffdata,[14,1],/edge_truncate)
           smoothdata=lat_data.diffdata-sm
        endif else begin
           diff=lat_data.origdata
           for ii=0,n_elements(diff[*,0])-1 do diff[ii,*] = lat_data.origdata[ii,*]-reform(lat_data.origdata[0,*])
           sm=smooth(diff,[14,1],/edge_truncate)
           smoothdata=diff-sm
        endelse
        
        
        avg_f=lat_data_average[rr,dir].wave_frontedge.lat
        stdv_f=lat_data_average[rr,dir].wave_frontedge.stdv
        avg_m=lat_data_average[rr,dir].wave_peak.lat
        stdv_m=lat_data_average[rr,dir].wave_peak.stdv
        avg_b=lat_data_average[rr,dir].wave_backedge.lat
        stdv_b=lat_data_average[rr,dir].wave_backedge.stdv
        
        aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],smoothdata[*,yrng[0]:yrng[1]],min=-10,max=20,$
                           title='',charsize=3,$
                           xtitle='Time of ' + strtrim(event.date, 2) ,ytitle='Relative angular distance'
        event=load_events_info(label=event.label)
        oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4
        oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4,color=255,linestyle=2
        oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4
        oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4,color=255,linestyle=2
        loadct,39,/silent
        oplot,time[sp:ep].jd+dt2,avg_f[sp:ep],psym=sym(3),symsize=2.4,color=50
        oplot,time[sp:ep].jd+dt2,avg_f[sp:ep],psym=sym(8),symsize=2.4,color=0,thick=3
        oplot,time[sp:ep].jd+dt2,avg_m[sp:ep],psym=sym(1),symsize=2.4,color=150
        oplot,time[sp:ep].jd+dt2,avg_m[sp:ep],psym=sym(6),symsize=2.4,color=0,thick=3
        oplot,time[sp:ep].jd+dt2,avg_b[sp:ep],psym=sym(2),symsize=2.4,color=200
        oplot,time[sp:ep].jd+dt2,avg_b[sp:ep],psym=sym(7),symsize=2.4,color=0,thick=3

;------------------Error on Graph----------------------------------
        oploterr, time[sp:ep].jd+dt2, avg_f[sp:ep], stdv_f[sp:ep], errcolor = 250, errthick = 10, /noconnect
        oploterr, time[sp:ep].jd+dt2, avg_b[sp:ep], stdv_b[sp:ep], errcolor = 250, errthick = 10, /noconnect
        oploterr, time[sp:ep].jd+dt2, avg_m[sp:ep], stdv_m[sp:ep], errcolor = 250, errthick = 10, /noconnect
        
;Elements defined to fill the structure
        nsteps = n_elements(lat_data.time)
        wav = '193'

;-----------Save the Averaged Radial Positions Graph-----------
        if keyword_set(ps) then begin
           device,/close
                                ;here do conversions to PNG files.
           exec='convert -flatten '+ploteps+' '+plotpng
           spawn,exec
           set_plot,'x'
        endif else begin
           write_png, plotpng, tvrd(/true)
           wdelete, 1
        endelse
;=====================================================================================
;=====================================================================================

        
        
        

;=====================================================================================
;           PLOT THE VARIOUS DATA TIME SERIES
;=====================================================================================

        if dir eq LEFTDIR then $
           plotfname=event.annuluspath+'lat_kinematics_line_'+strtrim(event.label)+'_r'+strtrim(string(rr+1),2)+'_left'
        if dir eq RIGHTDIR then $
           plotfname=event.annuluspath+'lat_kinematics_line_'+strtrim(event.label)+'_r'+strtrim(string(rr+1),2)+'_right'
        ploteps = plotfname+'.eps'
        plotpng = plotfname+'.png'
;-------------Window creation--------------
        if keyword_set(ps) then begin
                                ;!p.position=[0.15,0.12,0.93,0.9]
           !p.font=0
           set_plot,'ps'
           device,file=ploteps,/inches,xsize=9,ysize=12,$
                  /encaps,/color,/helvetica
           !p.multi =[0, 5, 1]
        endif else begin
           wdef, 1,1000, 1200
           loadct,0,/silent
           tvlct,rr,gg,bb,/get
           tvlct,reverse(rr),reverse(gg),reverse(bb)
           !p.multi =[0, 5, 1]
           !p.font=-1
        endelse

;Parameters of the plot panels
        npanels=5
        x0=0.16
        x1=0.96
        y1=0.96
        y0=0.06
        panelysize=(y1-y0)/(1.*npanels) ;0.17
        panelxsize=x1-x0
        strdy=0.02
        
        linethick=4
        
        ;colors
        green=150
        red=255
        blue=100
        cfront=red
        cpeak=green
        cback=blue
        
        
;----------------------PLOT THE AVERAGE WAVE INTENSITY---------------------------
        !p.position = [x0,y1-1*panelysize,x0+panelxsize,y1-0*panelysize]
        plot, timejd[sp:ep], avgintensity[sp:ep],/xstyle, thick = linethick, $
              title= "Tangential Wave Dynamics, "+strtrim(event.label,2),$
              charsize =3, color = 0, background = 255, ytitle ="Mean Intensity",$
              xrange = xrnge, /ynozero,/ystyle, xthick=linethick,ythick=linethick, XTICKFORMAT="(A1)", XTICKUNITS = ['Time']
        oplot,timejd[sp:ep], avgintensity[sp:ep],psym=sym(1),symsize=0.8,color=0
        oploterr, timejd[sp:ep], avgintensity[sp:ep], stdintensity[sp:ep], errcolor = 0, errthick = 4, /noconnect
        
;----------------------PLOT THE WAVE THICKNESS---------------------------
        !p.position = [0.11,0.61,0.94,0.79]
        !p.position = [x0,y1-2*panelysize,x0+panelxsize,y1-1*panelysize]
        plot, timejd[sp:ep], wav_thick[sp:ep],/xstyle, thick = linethick, color = 0, $
              background = 255, ytitle ="Thickness [deg]",$
              charsize =3, xrange = xrnge, /ynozero,/ystyle, xthick=linethick,ythick=linethick,$
              XTICKFORMAT="(A1)", XTICKUNITS = ['Time']
        oplot,timejd[sp:ep], wav_thick[sp:ep], psym=sym(1),symsize=0.8,color=0
        oploterr, timejd[sp:ep], wav_thick[sp:ep], wav_thick_std[sp:ep], errcolor = 0, errthick = 4, /noconnect
        
;----------------------PLOT THE WAVE ACCELERATION---------------------------
        !p.position = [0.11,0.41,0.94,0.59]
        !p.position = [x0,y1-3*panelysize,x0+panelxsize,y1-2*panelysize]
        plot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.front[sp:ep].accel, thick=linethick, $
              /xstyle, color = 0, background = 255, xthick=linethick,ythick=linethick,$
              ytitle ="Acceleration [m/s]", charsize = 3 ,/ystyle,$
              xrange = xrnge, /ynozero, /nodata, yrange = yrnge_accel, XTICKFORMAT="(A1)", XTICKUNITS = ['Time']
        loadct, 13, /silent
        xyouts, !p.position[0]+0.012, !p.position[3]-1*strdy, 'Front', color = cfront, /normal, charsize = 1.3, charthick = 1.5
        xyouts, !p.position[0]+0.012, !p.position[3]-2*strdy, 'Peak', color = cpeak, /normal, charsize = 1.3, charthick = 1.5
        xyouts, !p.position[0]+0.012, !p.position[3]-3*strdy, 'Back', color = cback, /normal, charsize = 1.3, charthick = 1.5
        
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.front[sp:ep].accel, thick=linethick, color = cfront
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.front[sp:ep].accel,psym=sym(1),symsize=0.8,color=cfront
        oploterr, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.front[sp:ep].accel, stdfrontaccel[sp:ep], errcolor = cfront, errthick = 4, /noconnect
        oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(lat_data_average[rr,dir].savgolfits.front[sp:ep].accel), thick=linethick, color = cfront,linestyle=0
        
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.peak[sp:ep].accel, thick=linethick, color = cpeak,linestyle=2
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.peak[sp:ep].accel,psym=sym(1),symsize=0.8,color=cpeak
        oploterr, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.peak[sp:ep].accel, stdpeakaccel[sp:ep], errcolor = cpeak, errthick = 4, /noconnect
        oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(lat_data_average[rr,dir].savgolfits.peak[sp:ep].accel), thick=linethick, color = cpeak,linestyle=2

        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.back[sp:ep].accel, thick=linethick, color = cback,linestyle=3
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.back[sp:ep].accel,psym=sym(1),symsize=0.8,color=cback
        oploterr, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.back[sp:ep].accel, stdbackaccel[sp:ep], errcolor = cback, errthick = 4, /noconnect
        oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(lat_data_average[rr,dir].savgolfits.back[sp:ep].accel), thick=linethick, color = cback,linestyle=3
        
        loadct, 0, /silent

        
;----------------------PLOT THE WAVE SPEEDS---------------------------
        !p.position = [0.11,0.21,0.94,0.39]
        !p.position = [x0,y1-4*panelysize,x0+panelxsize,y1-3*panelysize]
        plot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.back[sp:ep].speed, thick=linethick, /xstyle, color = 0, background = 255,$
              ytitle ="Speed [km/s]", charsize =3, xrange = xrnge, /ynozero, /nodata,/ystyle,$
              yrange = yrnge_speed, XTICKFORMAT="(A1)", XTICKUNITS = ['Time'], xthick=linethick,ythick=linethick
        loadct, 13, /silent
        xyouts, !p.position[0]+0.012, !p.position[3]-1*strdy, 'Front', color = cfront, /normal, charsize = 1.3, charthick = 1.5
        xyouts, !p.position[0]+0.012, !p.position[3]-2*strdy, 'Peak', color = cpeak, /normal, charsize = 1.3, charthick = 1.5
        xyouts, !p.position[0]+0.012, !p.position[3]-3*strdy, 'Back', color = cback, /normal, charsize = 1.3, charthick = 1.5
        
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.front[sp:ep].speed, thick=linethick, color = cfront
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.front[sp:ep].speed,psym=sym(1),symsize=0.8,color=cfront
        oploterr, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.front[sp:ep].speed, stdfrontspeed[sp:ep], errcolor = cfront, errthick = 4, /noconnect
        oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(lat_data_average[rr,dir].savgolfits.front[sp:ep].speed), thick=linethick, color = cfront,linestyle=0
        
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.peak[sp:ep].speed, thick=linethick, color = cpeak,linestyle=2
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.peak[sp:ep].speed,psym=sym(1),symsize=0.8,color=cpeak
        oploterr, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.peak[sp:ep].speed, stdpeakspeed[sp:ep], errcolor = cpeak, errthick = 4, /noconnect
        oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(lat_data_average[rr,dir].savgolfits.peak[sp:ep].speed), thick=linethick, color = cpeak,linestyle=2

        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.back[sp:ep].speed, thick=linethick, color = cback,linestyle=3
        oplot, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.back[sp:ep].speed,psym=sym(1),symsize=0.8,color=cback
        oploterr, timejd[sp:ep], lat_data_average[rr,dir].savgolfits.back[sp:ep].speed, stdbackspeed[sp:ep], errcolor = cback, errthick = 4, /noconnect
        oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(lat_data_average[rr,dir].savgolfits.back[sp:ep].speed), thick=linethick, color = cback,linestyle=3
        
        loadct, 0, /silent

        
;----------------------PLOT THE WAVE POSITIONS---------------------------
        !p.position = [0.11,0.035,0.94,0.19]
        !p.position = [x0,y1-5*panelysize,x0+panelxsize,y1-4*panelysize]
        ;plot, timejd[sp:ep], lat_data_average[rr,dir].wave_peak[sp:ep].lat,/xstyle, thick = linethick, color = 0, background = 255,$
        ;      ytitle="Front est. radial position R!Ds!N", xtitle = "Time, seconds", charsize = 3,/ystyle, xthick=linethick,ythick=linethick,$
        ;      xrange = xrnge, /nodata, yrange = yrnge_rad, XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE' ;, position = plot_pos
        plot, timejd[sp:ep], rad_from_lat[rr,dir].peak[sp:ep],/xstyle, thick = linethick, color = 0, background = 255,$
              ytitle="Est. radial position R!Ds!N", xtitle = "Time of "+event.date, charsize = 3,/ystyle, xthick=linethick,ythick=linethick,$
              xrange = xrnge, /nodata, yrange = yrnge_rad, XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE'
        
        loadct, 13, /silent
        xyouts, !p.position[0]+0.012, !p.position[3]-1*strdy, 'Front', color = cfront, /normal, charsize = 1.3, charthick = 1.5
        xyouts, !p.position[0]+0.012, !p.position[3]-2*strdy, 'Peak', color = cpeak, /normal, charsize = 1.3, charthick = 1.5
        xyouts, !p.position[0]+0.012, !p.position[3]-3*strdy, 'Back', color = cback, /normal, charsize = 1.3, charthick = 1.5
        
        ;oplot, timejd[sp:ep], lat_data_average[rr,dir].wave_frontedge[sp:ep].lat, thick=linethick,color = cfront
        ;oplot, timejd[sp:ep], lat_data_average[rr,dir].wave_frontedge[sp:ep].lat,psym=sym(1),symsize=0.8,color=cfront
        ;oploterr, timejd[sp:ep], lat_data_average[rr,dir].wave_frontedge[sp:ep].lat, lat_data_average[rr,dir].wave_frontedge[sp:ep].stdv, $
        ;          errcolor = cfront, errthick = 4, /noconnect
        oplot, timejd[sp:ep], rad_from_lat[rr,dir].front[sp:ep], thick=linethick,color = cfront
        oplot, timejd[sp:ep], rad_from_lat[rr,dir].front[sp:ep],psym=sym(1),symsize=0.8,color=cfront
        oploterr, timejd[sp:ep], rad_from_lat[rr,dir].front[sp:ep], rad_from_lat[rr,dir].frontstdv[sp:ep], $
                  errcolor = cfront, errthick = 4, /noconnect
        
        ;oplot, timejd[sp:ep],  lat_data_average[rr,dir].wave_peak[sp:ep].lat, thick=linethick,color = cpeak,linestyle=2 ;lat_data_average.wave_peak[sp:ep].lat
        ;oplot, timejd[sp:ep], lat_data_average[rr,dir].wave_peak[sp:ep].lat,psym=sym(1),symsize=0.8,color=cpeak ;lat_data_average.wave_peak[sp:ep].lat
        ;oploterr, timejd[sp:ep], lat_data_average[rr,dir].wave_peak[sp:ep].lat, lat_data_average[rr,dir].wave_peak[sp:ep].stdv, $
        ;          errcolor = cpeak, errthick = 4, /noconnect
        oplot, timejd[sp:ep],  rad_from_lat[rr,dir].peak[sp:ep], thick=linethick,color = cpeak,linestyle=2 ;lat_data_average.wave_peak[sp:ep].lat
        oplot, timejd[sp:ep], rad_from_lat[rr,dir].peak[sp:ep],psym=sym(1),symsize=0.8,color=cpeak ;lat_data_average.wave_peak[sp:ep].lat
        oploterr, timejd[sp:ep], rad_from_lat[rr,dir].peak[sp:ep], rad_from_lat[rr,dir].peakstdv[sp:ep], $
                  errcolor = cpeak, errthick = 4, /noconnect
        
        ;oplot, timejd[sp:ep], lat_data_average[rr,dir].wave_backedge[sp:ep].lat, thick=linethick, color = cback,linestyle=3
        ;oplot, timejd[sp:ep], lat_data_average[rr,dir].wave_backedge[sp:ep].lat,psym=sym(1),symsize=0.8,color=cback
        ;oploterr, timejd[sp:ep], lat_data_average[rr,dir].wave_backedge[sp:ep].lat, lat_data_average[rr,dir].wave_backedge[sp:ep].stdv, $
        ;          errcolor = cback, errthick = 4, /noconnect
        oplot, timejd[sp:ep], rad_from_lat[rr,dir].back[sp:ep], thick=linethick, color = cback,linestyle=3
        oplot, timejd[sp:ep], rad_from_lat[rr,dir].back[sp:ep],psym=sym(1),symsize=0.8,color=cback
        oploterr, timejd[sp:ep], rad_from_lat[rr,dir].back[sp:ep], rad_from_lat[rr,dir].backstdv[sp:ep], $
                  errcolor = cback, errthick = 4, /noconnect
        
        loadct, 0, /silent

;----------------------Save the 5 graph plot-----------------
        if keyword_set(ps) then begin
           device,/close
                                ;here do conversions to PNG files.
           exec='convert -flatten '+ploteps+' '+plotpng + '; rm -rf '+ploteps
           spawn,exec
           set_plot,'x'
        endif else begin
           write_png, plotpng, tvrd(/true)
           wdelete, 1           
        endelse
     endfor
  endfor
  
  ;SAVE THE AVERAGED MEASUREMENTS TO A SEPARATE FILE
  event=load_events_info(label=event.label)
  shockfile=event.annuluspath+replace_string(event.annplot.analyzed.lateral.avg_savename,'WAV',wav)
  lat_data=lat_data_average
  save,filename=shockfile,lat_data,ind_arr,annulus_info,rad_from_lat,lat_stdv
  print, "Done computing averages for event "+event.label
;=====================================================================================
;=====================================================================================
  
end
