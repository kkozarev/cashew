pro test_calculate_radial_kinematics_results
;A little program to test the calculate_radial_results procedure
  events=['100613_01','101103_02','101231_01','110125_01','110211_02','110327_01','110515_01','110607_01','110904_01','120307_01','120424_01','120526_01','120728_01','120915_01','121007_01','130423_01','130501_01','130517_01','131107_01','131207_01','131212_01','140217_01','140708_01','140901_01','141105_02','141205_01','150303_01','150421_01','150920_01','151109_01']
  events=['100613_01','101103_02']
  events=['151104_01']
  events=['110607_01','131212_01']
  nevents=n_elements(events)
  for ev=0,nevents-1 do begin
     event_label=events[ev]
     event=load_events_info(label=event_label)
     calculate_radial_kinematics_results,event,/ps
  endfor
end




pro calculate_radial_kinematics_results,event,ps=ps,force=force

;PURPOSE:
;Analyze data from the 10 manually created runs for annulus data,
;and then create an averaged radial position to plot, along
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
;Modified by Kamen Kozarev, 03/13/2017


  wav='193'
  ;Check if the averaged kinematics file already exists. If not, do the fitting.
  avgkinfile=event.annuluspath+replace_string(event.annplot.analyzed.radial.avg_savename,'WAV',wav)
  if not file_exist(avgkinfile) or keyword_set(force) then begin

;---------------Loads part of the event---------------------------------
  savepath=event.annuluspath
  save_fname=replace_string(event.annplot.analyzed.radial.savename,'WAV','193')
  save_fname=replace_string(save_fname,'SSSSS','_mxxxx')
  
  
;--------------------------AVERAGES AND STANDARD DEVIATIONS------------------------
;nmeas needs to be = N-1 once every event has N meas to their names
  search_fname = event.annuluspath + 'annplot_'+strtrim(event.date)+'_'+strtrim(event.label)+'_193_analyzed_radial*_m*.sav'
  fnames=file_search(search_fname)
  nmeas = n_elements(fnames)
  if nmeas lt 3 then begin
     print,''
     print,'Not enough measurements to average for this event. Quitting...'
     print,''
  endif
  restore, fnames[0]
  allrad_data=replicate(rad_data,nmeas)
  
;Defining some values for later
  sp=rad_data.timefitrange[0]
  ep=rad_data.timefitrange[1]
  yrng=rad_data.radfitrange
  time=rad_data.time
  xrng=rad_data.timefitrange
  yarray=rad_data.y_rsun_array
  dt2=(rad_data.time[1].jd-rad_data.time[0].jd)/2.
  ntimes=n_elements(rad_data.data[*,0])
  

;Then you load the data for each measurement, and so you loop over measurements:
  for mm=0,nmeas-1 do begin
     restore, fnames[mm]
     if mm eq 0 then begin
          rad_data_average={type:'radial',$
                     label:rad_data.label,$
                     wav:rad_data.wav,$
                     time:rad_data.time,$
                     y_rsun_array:rad_data.y_rsun_array,$
                     y_arcsec_array:rad_data.y_arcsec_array,$
                     x_deg_array:rad_data.x_deg_array,$
                     data:rad_data.data,$
                     origdata:rad_data.data,$
                     diffdata:rad_data.data,$
                     radfitrange:rad_data.radfitrange,$
                     timefitrange:rad_data.timefitrange,$
                     fitparams:rad_data.fitparams,$
                     fitsigma:rad_data.fitsigma,$
                     kinfittimerange:rad_data.kinfittimerange,$
                     savgolfits:rad_data.savgolfits,$                   
                     maxinds:rad_data.maxinds,$
                     wave_frontedge:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},ntimes),$
                     wave_backedge:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},ntimes),$
                     wave_peak:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},ntimes),$
                     wavethick:fltarr(ntimes),$
                     avgIntense:fltarr(ntimes)}
     endif
     if mean(rad_data.savgolfits.back.speed) eq 0. or $
        mean(rad_data.savgolfits.front.speed) eq 0. or $
        mean(rad_data.savgolfits.peak.speed) eq 0. then begin
        fit_wave_kinematics_radial,rad_data,ind_arr,/front
        fit_wave_kinematics_radial,rad_data,ind_arr,/peak
        fit_wave_kinematics_radial,rad_data,ind_arr,/back
        save,filename=fnames[mm],rad_data,ind_arr,annulus_info
     endif
     allrad_data[mm]=rad_data
  endfor
  
  
;Finds the mean and stddev of all the events
  stdintensity=fltarr(ntimes)
  wav_thick_std=fltarr(ntimes)
  stdbackaccel=fltarr(ntimes)
  stdpeakaccel=fltarr(ntimes)
  stdfrontaccel=fltarr(ntimes)
  stdbackspeed=fltarr(ntimes)
  stdpeakspeed=fltarr(ntimes)
  stdfrontspeed=fltarr(ntimes)
  
  for tt=0,ntimes-1 do begin
     rad_data_average.wave_frontedge[tt].rad=mean(allrad_data.wave_frontedge[tt].rad)
     rad_data_average.wave_frontedge[tt].stdv=stddev(allrad_data.wave_frontedge[tt].rad)
     rad_data_average.wave_backedge[tt].rad=mean(allrad_data.wave_backedge[tt].rad)
     rad_data_average.wave_backedge[tt].stdv=stddev(allrad_data.wave_backedge[tt].rad)
     rad_data_average.wave_peak[tt].rad=mean(allrad_data.wave_peak[tt].rad)
     rad_data_average.wave_peak[tt].stdv=stddev(allrad_data.wave_peak[tt].rad)
     
     rad_data_average.avgIntense[tt]=mean(allrad_data.avgIntense[tt])
     stdintensity[tt]=stddev(allrad_data.avgIntense[tt])
     rad_data_average.wavethick[tt]=mean(allrad_data.wavethick[tt])
     wav_thick_std[tt]=stddev(allrad_data.wavethick[tt])
     
     rad_data_average.savgolfits.back[tt].accel=mean(allrad_data.savgolfits.back[tt].accel)
     stdbackaccel[tt]=stddev(allrad_data.savgolfits.back[tt].accel)
     rad_data_average.savgolfits.peak[tt].accel=mean(allrad_data.savgolfits.peak[tt].accel)
     stdpeakaccel[tt]=stddev(allrad_data.savgolfits.peak[tt].accel)
     rad_data_average.savgolfits.front[tt].accel=mean(allrad_data.savgolfits.front[tt].accel)
     stdfrontaccel[tt]=stddev(allrad_data.savgolfits.front[tt].accel)
     rad_data_average.savgolfits.back[tt].speed=mean(allrad_data.savgolfits.back[tt].speed)
     stdbackspeed[tt]=stddev(allrad_data.savgolfits.back[tt].speed)
     rad_data_average.savgolfits.peak[tt].speed=mean(allrad_data.savgolfits.peak[tt].speed)
     stdpeakspeed[tt]=stddev(allrad_data.savgolfits.peak[tt].speed)
     rad_data_average.savgolfits.front[tt].speed=mean(allrad_data.savgolfits.front[tt].speed)
     stdfrontspeed[tt]=stddev(allrad_data.savgolfits.front[tt].speed)
  endfor
  
  ;Also record the second-order polynomial fits
  for iii=0,2 do rad_data_average.fitparams[iii].front=mean(allrad_data.fitparams[iii].front)
  for iii=0,2 do rad_data_average.fitparams[iii].peak=mean(allrad_data.fitparams[iii].peak)
  for iii=0,2 do rad_data_average.fitparams[iii].back=mean(allrad_data.fitparams[iii].back)
  
  sp=rad_data_average.timefitrange[0]
  ep=rad_data_average.timefitrange[1]
  timesec = rad_data_average.time.relsec
  
  timejd=rad_data_average.time.jd
  avgintensity=rad_data_average.avgIntense
  wav_thick = rad_data_average.wavethick
  
  ;SAVE THE AVERAGED MEASUREMENTS TO A SEPARATE FILE
  event=load_events_info(label=event.label)
  shockfile=event.annuluspath+replace_string(event.annplot.analyzed.radial.avg_savename,'WAV',wav)
  rad_data=rad_data_average
  save,filename=shockfile,rad_data,ind_arr,annulus_info,stdbackaccel,stdpeakaccel,$
       stdfrontaccel,stdbackspeed,stdpeakspeed,stdfrontspeed,stdintensity,wav_thick_std
  
endif else begin
   ;JUST DO THE PLOTTING
   shockfile=event.annuluspath+replace_string(event.annplot.analyzed.radial.avg_savename,'WAV',wav)
   restore,shockfile
   rad_data_average=rad_data
   sp=rad_data.timefitrange[0]
   ep=rad_data.timefitrange[1]
   yrng=rad_data.radfitrange
   time=rad_data.time
   xrng=rad_data.timefitrange
   yarray=rad_data.y_rsun_array
   dt2=(rad_data.time[1].jd-rad_data.time[0].jd)/2.
   ntimes=n_elements(rad_data.data[*,0])
   timejd=rad_data.time.jd
   avgintensity=rad_data.avgIntense
   wav_thick = rad_data.wavethick
endelse

;----------Finding plotting range for acceleration--------------------
  accel_array_front=rad_data_average.savgolfits.front[sp:ep].accel
  accel_array_front_max = max(accel_array_front+stdfrontaccel[sp:ep])
  accel_array_front_min = min(accel_array_front[where(accel_array_front ne 0.)]-stdfrontaccel[sp:ep])
  accel_array_peak = rad_data_average.savgolfits.peak[sp:ep].accel
  accel_array_peak_max = max(accel_array_peak+stdpeakaccel[sp:ep])
  accel_array_peak_min = min(accel_array_peak[where(accel_array_peak ne 0.)]-stdpeakaccel[sp:ep])
  accel_array_back = rad_data_average.savgolfits.back[sp:ep].accel
  accel_array_back_max = max(accel_array_back+stdbackaccel[sp:ep])
  accel_array_back_min = min(accel_array_back[where(accel_array_back ne 0.)]-stdbackaccel[sp:ep])
  max_array = [accel_array_front_max,accel_array_peak_max,accel_array_back_max]
  min_array = [accel_array_front_min, accel_array_peak_min, accel_array_back_min]
  yrnge_accel = [min(min_array), max(max_array)]

;----------Finding plotting range for speed--------------------
  speed_array_front =rad_data_average.savgolfits.front[sp:ep].speed
  speed_array_front_max = max(speed_array_front+stdfrontspeed[sp:ep])
  speed_array_front_min = min(speed_array_front[where(speed_array_front ne 0.)]-stdfrontspeed[sp:ep])
  speed_array_peak = rad_data_average.savgolfits.peak[sp:ep].speed
  speed_array_peak_max = max(speed_array_peak+stdpeakspeed[sp:ep])
  speed_array_peak_min = min(speed_array_peak[where(speed_array_peak ne 0.)]-stdpeakspeed[sp:ep])
  speed_array_back = rad_data_average.savgolfits.back[sp:ep].speed
  speed_array_back_max = max(speed_array_back+stdbackspeed[sp:ep])
  speed_array_back_min = min(speed_array_back[where(speed_array_back ne 0.)]-stdbackspeed[sp:ep])
  max_array = [speed_array_front_max,speed_array_peak_max,speed_array_back_max]
  min_array = [speed_array_front_min, speed_array_peak_min, speed_array_back_min]
  yrnge_speed = [min(min_array), max(max_array)]

;----------Finding plotting range for position--------------------
  pos_array_peak = rad_data_average.wave_peak[sp:ep].rad
  pos_array_peak_max = max(pos_array_peak+rad_data_average.wave_peak[sp:ep].stdv)
  pos_array_peak_min = min(pos_array_peak[where(pos_array_peak ne 0.)]-rad_data_average.wave_peak[sp:ep].stdv)
  pos_array_front = rad_data_average.wave_frontedge[sp:ep].rad
  pos_array_front_max = max(pos_array_front+rad_data_average.wave_frontedge[sp:ep].stdv)
  pos_array_front_min = min(pos_array_front[where(pos_array_front ne 0.)]-rad_data_average.wave_frontedge[sp:ep].stdv)
  pos_array_back = rad_data_average.wave_backedge[sp:ep].rad
  pos_array_back_max = max(pos_array_back+rad_data_average.wave_backedge[sp:ep].stdv)
  pos_array_back_min = min(pos_array_back[where(pos_array_back ne 0.)]-rad_data_average.wave_backedge[sp:ep].stdv)
  max_array = [pos_array_front_max,pos_array_peak_max,pos_array_back_max]
  min_array = [pos_array_front_min, pos_array_peak_min, pos_array_back_min]
  yrnge_rad = [min(min_array), max(max_array)]

;----------Creating the x range--------------
;This may need updating in the future
  xrnge = [rad_data.time[sp].jd, rad_data.time[ep].jd]
  
  
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
;=====================================================================================
;           PLOT THE AVERAGED RADIAL POSITIONS GRAPH
;=====================================================================================
  set_plot,'x'
  plotpng=event.annuluspath+'avg_rad_pos_'+strtrim(event.label)+'.png'
  ploteps=event.annuluspath+'avg_rad_pos_'+strtrim(event.label)+'.eps'
  
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
  if tag_exist(rad_data,'diffdata') then begin
     sm=smooth(rad_data.diffdata,[14,1],/edge_truncate)
     smoothdata=rad_data.diffdata-sm
  endif else begin
     diff=rad_data.origdata
     for ii=0,n_elements(diff[*,0])-1 do diff[ii,*] = rad_data.origdata[ii,*]-reform(rad_data.origdata[0,*])
     sm=smooth(diff,[14,1],/edge_truncate)
     smoothdata=diff-sm
  endelse
  
  
  avg_f=rad_data_average.wave_frontedge.rad
  stdv_f=rad_data_average.wave_frontedge.stdv
  avg_m=rad_data_average.wave_peak.rad
  stdv_m=rad_data_average.wave_peak.stdv
  avg_b=rad_data_average.wave_backedge.rad
  stdv_b=rad_data_average.wave_backedge.stdv
  
  aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],smoothdata[*,yrng[0]:yrng[1]],min=-10,max=20,$
                     title='',charsize=4,$
                     xtitle='Time of ' + strtrim(event.date, 2) ,ytitle='R!DS!N'
  event=load_events_info(label=event.label)
  oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4
  oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4,color=255,linestyle=2
  oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4
  oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4,color=255,linestyle=2
  loadct,39,/silent
  oplot,time[sp:ep].jd+dt2,avg_f[sp:ep],psym=sym(3),symsize=3,color=50
  oplot,time[sp:ep].jd+dt2,avg_f[sp:ep],psym=sym(8),symsize=3,color=0,thick=3
  oplot,time[sp:ep].jd+dt2,avg_m[sp:ep],psym=sym(1),symsize=3,color=150
  oplot,time[sp:ep].jd+dt2,avg_m[sp:ep],psym=sym(6),symsize=3,color=0,thick=3
  oplot,time[sp:ep].jd+dt2,avg_b[sp:ep],psym=sym(2),symsize=3,color=200
  oplot,time[sp:ep].jd+dt2,avg_b[sp:ep],psym=sym(7),symsize=3,color=0,thick=3

;------------------Error on Graph----------------------------------
  oploterr, time[sp:ep].jd+dt2, avg_f[sp:ep], stdv_f[sp:ep], errcolor = 250, errthick = 10, /noconnect
  oploterr, time[sp:ep].jd+dt2, avg_b[sp:ep], stdv_b[sp:ep], errcolor = 250, errthick = 10, /noconnect
  oploterr, time[sp:ep].jd+dt2, avg_m[sp:ep], stdv_m[sp:ep], errcolor = 250, errthick = 10, /noconnect
  
  
;Elements defined to fill the structure
  nsteps = n_elements(rad_data.time)
  wav = '193'
  
;----------------Save the Averaged Radial Positions Graph----------------------
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

  ploteps = event.annuluspath+'rad_kinematics_line_'+strtrim(event.label)+'.eps'
  plotpng = event.annuluspath+'rad_kinematics_line_'+strtrim(event.label)+'.png'
  
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
        title= "Radial Kinematics, "+strtrim(event.label,2),$
        charsize =3, color = 0, background = 255, ytitle ="Wave Mean Intensity",$
        xrange = xrnge, /ynozero,/ystyle, xthick=linethick,ythick=linethick, XTICKFORMAT="(A1)", XTICKUNITS = ['Time']
  oplot,timejd[sp:ep], avgintensity[sp:ep],psym=sym(1),symsize=0.8,color=0
  oploterr, timejd[sp:ep], avgintensity[sp:ep], stdintensity[sp:ep], errcolor = 0, errthick = 4, /noconnect
 
;----------------------PLOT THE WAVE THICKNESS---------------------------
  !p.position = [0.11,0.61,0.94,0.79]
  !p.position = [x0,y1-2*panelysize,x0+panelxsize,y1-1*panelysize]
  plot, timejd[sp:ep], wav_thick[sp:ep],/xstyle, thick = linethick, color = 0, $
        background = 255, ytitle ="Wave Thickness [R!Ds!N]",$
        charsize =3, xrange = xrnge, /ynozero,/ystyle, xthick=linethick,ythick=linethick,$
        XTICKFORMAT="(A1)", XTICKUNITS = ['Time']
  oplot,timejd[sp:ep], wav_thick[sp:ep], psym=sym(1),symsize=0.8,color=0
  oploterr, timejd[sp:ep], wav_thick[sp:ep], wav_thick_std[sp:ep], errcolor = 0, errthick = 4, /noconnect
  
;----------------------PLOT THE WAVE ACCELERATION---------------------------
  !p.position = [0.11,0.41,0.94,0.59]
  !p.position = [x0,y1-3*panelysize,x0+panelxsize,y1-2*panelysize]
  plot, timejd[sp:ep], rad_data_average.savgolfits.front[sp:ep].accel, thick=linethick, $
        /xstyle, color = 0, background = 255, xthick=linethick,ythick=linethick,$
        ytitle ="Acceleration [m/s]", charsize = 3 ,/ystyle,$
        xrange = xrnge, /ynozero, /nodata, yrange = yrnge_accel, XTICKFORMAT="(A1)", XTICKUNITS = ['Time']
  loadct, 13, /silent
  xyouts, !p.position[0]+0.012, !p.position[3]-1*strdy, 'Front', color = cfront, /normal, charsize = 1.3, charthick = 1.5
  xyouts, !p.position[0]+0.012, !p.position[3]-2*strdy, 'Peak', color = cpeak, /normal, charsize = 1.3, charthick = 1.5
  xyouts, !p.position[0]+0.012, !p.position[3]-3*strdy, 'Back', color = cback, /normal, charsize = 1.3, charthick = 1.5

  oplot, timejd[sp:ep], rad_data_average.savgolfits.front[sp:ep].accel, thick=linethick, color = cfront,linestyle=0
  oplot, timejd[sp:ep], rad_data_average.savgolfits.front[sp:ep].accel,psym=sym(1),symsize=0.8,color=cfront
  oploterr, timejd[sp:ep], rad_data_average.savgolfits.front[sp:ep].accel, stdfrontaccel[sp:ep], errcolor = cfront, errthick = 4, /noconnect
  oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(rad_data_average.savgolfits.front[sp:ep].accel), thick=linethick, color = cfront,linestyle=0

  oplot, timejd[sp:ep], rad_data_average.savgolfits.peak[sp:ep].accel, thick=linethick, color = cpeak,linestyle=2
  oplot, timejd[sp:ep], rad_data_average.savgolfits.peak[sp:ep].accel,psym=sym(1),symsize=0.8,color=cpeak
  oploterr, timejd[sp:ep], rad_data_average.savgolfits.peak[sp:ep].accel, stdpeakaccel[sp:ep], errcolor = cpeak, errthick = 4, /noconnect
  oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(rad_data_average.savgolfits.peak[sp:ep].accel), thick=linethick, color = cpeak,linestyle=2
  
  oplot, timejd[sp:ep], rad_data_average.savgolfits.back[sp:ep].accel, thick=linethick, color = cback,linestyle=3
  oplot, timejd[sp:ep], rad_data_average.savgolfits.back[sp:ep].accel,psym=sym(1),symsize=0.8,color=cback
  oploterr, timejd[sp:ep], rad_data_average.savgolfits.back[sp:ep].accel, stdbackaccel[sp:ep], errcolor = cback, errthick = 4, /noconnect
  oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(rad_data_average.savgolfits.back[sp:ep].accel), thick=linethick, color = cback,linestyle=3
  
  loadct, 0, /silent

;----------------------PLOT THE WAVE SPEEDS---------------------------
  !p.position = [0.11,0.21,0.94,0.39]
  !p.position = [x0,y1-4*panelysize,x0+panelxsize,y1-3*panelysize]
  plot, timejd[sp:ep], rad_data_average.savgolfits.back[sp:ep].speed, thick=linethick, /xstyle, color = 0, background = 255,$
        ytitle ="Speed [km/s]", charsize =3, xrange = xrnge, /ynozero, /nodata,/ystyle,$
        yrange = yrnge_speed, XTICKFORMAT="(A1)", xthick=linethick,ythick=linethick, XTICKUNITS = ['Time']
  loadct, 13, /silent
  xyouts, !p.position[0]+0.012, !p.position[3]-1*strdy, 'Front', color = cfront, /normal, charsize = 1.3, charthick = 1.5
  xyouts, !p.position[0]+0.012, !p.position[3]-2*strdy, 'Peak', color = cpeak, /normal, charsize = 1.3, charthick = 1.5
  xyouts, !p.position[0]+0.012, !p.position[3]-3*strdy, 'Back', color = cback, /normal, charsize = 1.3, charthick = 1.5

  oplot, timejd[sp:ep], rad_data_average.savgolfits.front[sp:ep].speed, thick=linethick, color = cfront,linestyle=0
  oplot, timejd[sp:ep], rad_data_average.savgolfits.front[sp:ep].speed,psym=sym(1),symsize=0.8,color=cfront
  oploterr, timejd[sp:ep], rad_data_average.savgolfits.front[sp:ep].speed, stdfrontspeed[sp:ep], errcolor = cfront, errthick = 4, /noconnect
  oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(rad_data_average.savgolfits.front[sp:ep].speed), thick=linethick, color = cfront,linestyle=0

  oplot, timejd[sp:ep], rad_data_average.savgolfits.peak[sp:ep].speed, thick=linethick, color = cpeak,linestyle=2
  oplot, timejd[sp:ep], rad_data_average.savgolfits.peak[sp:ep].speed,psym=sym(1),symsize=0.8,color=cpeak
  oploterr, timejd[sp:ep], rad_data_average.savgolfits.peak[sp:ep].speed, stdpeakspeed[sp:ep], errcolor = cpeak, errthick = 4, /noconnect
  oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(rad_data_average.savgolfits.peak[sp:ep].speed), thick=linethick, color = cpeak,linestyle=2
  
  oplot, timejd[sp:ep], rad_data_average.savgolfits.back[sp:ep].speed, thick=linethick, color = cback,linestyle=3
  oplot, timejd[sp:ep], rad_data_average.savgolfits.back[sp:ep].speed,psym=sym(1),symsize=0.8,color=cback
  oploterr, timejd[sp:ep], rad_data_average.savgolfits.back[sp:ep].speed, stdbackspeed[sp:ep], errcolor = cback, errthick = 4, /noconnect
  oplot,timejd[sp:ep],fltarr(ep-sp+1)+mean(rad_data_average.savgolfits.back[sp:ep].speed), thick=linethick, color = cback,linestyle=3
  
  loadct, 0, /silent

  
;----------------------PLOT THE WAVE POSITIONS---------------------------
  !p.position = [0.11,0.035,0.94,0.19]
  !p.position = [x0,y1-5*panelysize,x0+panelxsize,y1-4*panelysize]
  plot, timejd[sp:ep], rad_data_average.wave_peak[sp:ep].rad,/xstyle, thick = linethick, color = 0, background = 255,$
        ytitle="Radial position [R!Ds!N]", xtitle = "Time of "+event.date, charsize = 3,/ystyle, xthick=linethick,ythick=linethick,$
        xrange = xrnge, /nodata, yrange = yrnge_rad, XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE'
  loadct, 13, /silent
  xyouts, !p.position[0]+0.012, !p.position[3]-1*strdy, 'Front', color = cfront, /normal, charsize = 1.3, charthick = 1.5
  xyouts, !p.position[0]+0.012, !p.position[3]-2*strdy, 'Peak', color = cpeak, /normal, charsize = 1.3, charthick = 1.5
  xyouts, !p.position[0]+0.012, !p.position[3]-3*strdy, 'Back', color = cback, /normal, charsize = 1.3, charthick = 1.5
  
  oplot, timejd[sp:ep], rad_data_average.wave_frontedge[sp:ep].rad, thick=linethick,color = cfront
  oplot, timejd[sp:ep], rad_data_average.wave_frontedge[sp:ep].rad,psym=sym(1),symsize=0.8,color=cfront
  oploterr, timejd[sp:ep], rad_data_average.wave_frontedge[sp:ep].rad, rad_data_average.wave_frontedge[sp:ep].stdv, $
            errcolor = cfront, errthick = 4, /noconnect

  oplot, timejd[sp:ep],  rad_data_average.wave_peak[sp:ep].rad, thick=linethick,color = cpeak,linestyle=2
  oplot, timejd[sp:ep], rad_data_average.wave_peak[sp:ep].rad,psym=sym(1),symsize=0.8,color=cpeak
  oploterr, timejd[sp:ep], rad_data_average.wave_peak[sp:ep].rad, rad_data_average.wave_peak[sp:ep].stdv, $
            errcolor = cpeak, errthick = 4, /noconnect
  
  oplot, timejd[sp:ep], rad_data_average.wave_backedge[sp:ep].rad, thick=linethick, color = cback,linestyle=3
  oplot, timejd[sp:ep], rad_data_average.wave_backedge[sp:ep].rad,psym=sym(1),symsize=0.8,color=cback
  oploterr, timejd[sp:ep], rad_data_average.wave_backedge[sp:ep].rad, rad_data_average.wave_backedge[sp:ep].stdv, $
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
  
  print, "Done computing averages for event "+event.label
;=====================================================================================
;=====================================================================================
  
end
