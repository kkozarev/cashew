pro find_wave_radial_positions_manual, event, rad_data, plotinfo, exit_status
;PURPOSE
;Procedure to find the edges of the EUV wave manually.
;
;INPUTS
;     EVENT - the event information structure
;     RAD_DATA - annulus data structure
;     PLOTINFO - information needed to plot stuff
;     EXIT_STATUS - indicating how the process went.
;
;OUTPUTS - UPDATES TO:
;     WAVE_FRONTEDGE - sub-structure of rad_data holding information
;                      about the wave front edge
;     WAVE_PEAK      - sub-structure of rad_data holding information
;                      about the wave peak
;     WAVE_BACKEDGE - sub-structure of rad_data holding information
;                      about the wave back edge
;
;AUTHOR: Kamen Kozarev, 05/20/2017
;
  !p.multi=0
  plot = 1
  debug = 0
  ;Define the exit statuses here:
  NORMAL_STATUS=0
  CONTINUE_STATUS=1
  QUIT_STATUS=2

  loadct, 0,/silent
  ;mymaxima=rad_data.mymaxima
  timefitrange=rad_data.timefitrange
  sp=timefitrange[0]
  ep=timefitrange[1]
  if sp eq -1 and ep eq -1 then begin
     print,''
     uinput=''
     valid=0
     while valid eq 0 do begin
        read,uinput,prompt='NO WAVE IS DETECTED. Do you want to continue anyway (y/n)?:'
        if uinput eq 'y' or uinput eq 'n' then valid=1
     endwhile
     if uinput eq 'n' then begin
        return
     endif
  endif
  data=rad_data.data            ;The fit data
  diffdata=rad_data.diffdata    ;The base differenced data
  time=rad_data.time
  yarray=rad_data.y_rsun_array
  yrng=rad_data.radfitrange
  dt2=(time[1].jd-time[0].jd)/2.
  
  rad = yarray[yrng[0]:yrng[1]]
  dat = data[*,yrng[0]:yrng[1]]
  diffdat = diffdata[*,yrng[0]:yrng[1]]
  
  last_saved_start_end=event.annuluspath+'radial_wave_extent.last'

  mind=0
  nSigma = 1.5
  
  wave_frontedge=rad_data.wave_frontedge
  wave_backedge=rad_data.wave_backedge
  wave_peak=rad_data.wave_peak
  reached_front_edge=0          ;a flag to let us know the wave has reached the front edge
  

                                ;Find a global background by averaging
                                ;pixels at the top of the FOV and the
                                ;beginning of the event, and overplot it on the data plot for reference.
  ny=n_elements(rad_data.data[0,*])
  data_background=mean(rad_data.data[0:9,ny-25:ny-1])
  
  set_plot,'x'
  !p.position=[0.11,0.13,0.92,0.93]
  !p.background=255
  !p.color=0
  loadct,39,/silent
  print,''
  print,'Manual selection of the wave back edge, peak, and front edge.'
  
  order = 0
  degree = 4
  sghw = 25
  savgol_filter=SAVGOL(sghw, sghw, order, degree)
  
;------------------- START MOVIE OF WAVE PROFILE OVER TIME ----------------------  
;Do an initial loop over the steps, so the user can see the wave well.
  print,''
  uinput=''
  valid=0
  while valid eq 0 do begin
     read,uinput,prompt='Watch a movie of the wave profile? Press "y" for yes, "n" for no: '
     if uinput eq 'y' or uinput eq 'n' then valid=1
  endwhile
  if uinput eq 'y' then begin
     if plot gt 0 then wdef,15,1000,800
     rewatch:
;get the Savitzky-Golay filter of the positions.

     for ii=sp, ep do begin
        col = dat[ii,*]
        diffcol=diffdat[ii,*]
        colSmooth = reform(smooth(col, 50, /edge_truncate))
        
; Filter out the negative data
        ;average = mean(colSmooth)
        ;negInd = where(colSmooth lt 0) 
        ;if negInd[0] ne -1.0 then begin
        ;   colSmooth[negInd] = 0 ;average
        ;endif
        sgsmooth=CONVOL(reform(col), savgol_filter, /EDGE_TRUNCATE)
        plot, rad, col, xtitle='Radial distance, R!DS!N',ytitle='Pixel intensity',charthick=1,/ystyle,/xstyle,$
              title=strtrim(string(ii),2)+' Wave J-map profile, '+time[ii].cashew_time,$
              /nodata,charsize=2,yrange=[-3,max(col)+1],xrange=minmax(rad)
        oplot,rad,col,color=0
        oplot,rad,colSmooth,color=70,thick=4
        oplot,rad,sgsmooth,color=220,thick=4
        oplot,minmax(rad),[data_background,data_background],linestyle=2,color=150,thick=4
        xyouts,!p.position[2]-0.16,0.87,'Orig. data',color=0,/norm,charsize=2
        xyouts,!p.position[2]-0.16,0.84,'smooth(data)',color=70,/norm,charsize=2
        xyouts,!p.position[2]-0.16,0.81,'savgol(data)',color=220,/norm,charsize=2
        wait,0.4
     endfor
     uinput=''
     valid=0
     while valid eq 0 do begin
        read,uinput,prompt='Watch again? (y/n): '
        if uinput eq 'y' or uinput eq 'n' then valid=1
     endwhile
     if uinput eq 'y' then goto,rewatch
     wdel,15
  endif
;------------------- END MOVIE OF WAVE PROFILE OVER TIME ----------------------  
  ;print,''
  ;print,'Time for selecting manually the wave back edge, peak, and front edge. Press any key when ready!'
  ;tmp=get_kbrd()



  
;==========================
; LOOP OVER TIME STEPS
;==========================
; Determine the wave edge for each timestep within the region of
; interest
  
                                ;Plot the temporal extent and the
                                ;current time, directly on the wave
                                ;profile map.
  
  if plot gt 0 then begin
     window=15
     profile_position=[0.05,0.15,0.6,0.9]
     profile_multi=[0,2,1]
     overview_position=[0.69,0.15,0.97,0.9]
     overview_multi=[1,2,1]
     wdef,window,1800,800
  endif
  
  order = 0
  degree = 4
  sghw = 25
  savgol_filter=SAVGOL(sghw, sghw, order, degree)

  started_measurements = 0
  for ii=sp,ep do begin
     alt=0
     newtime = time.jd
     caldat, newtime[ii], m, d, y, h, m, s 
     
;The data
     col = dat[ii,*]
     diffcol=diffdat[ii,*]
;The smooth(data)
     colSmooth = reform(smooth(col, 50, /edge_truncate))
;The savgol(data)
     sgsmooth=CONVOL(reform(col), savgol_filter, /EDGE_TRUNCATE)
     
; Filter out the negative data
                                ;average = mean(colSmooth)
                                ;negInd = where(colSmooth lt 0) 
                                ;if negInd[0] ne -1.0 then begin
                                ;   colSmooth[negInd] = 0   ;average 
                                ;endif

;------------------- START PROFILE PLOT ----------------------  
;Plot the wave profile
     ;wset,profile_window
     if ii gt sp then begin
        !p=profile_p
        !x=profile_x
        !y=profile_y
     endif else !p.position=profile_position
     !p.multi=profile_multi
     plot, rad, col, xtitle='Radial distance, R!DS!N',ytitle='Pixel intensity',charthick=1,/ystyle,/xstyle,$
           title=strtrim(string(ii),2)+' Wave J-map profile, '+time[ii].cashew_time,$
           /nodata,charsize=2,yrange=[-3,max(col)+1],xrange=minmax(rad)
     oplot,rad,col,color=0
     oplot,rad,colSmooth,color=70,thick=4
     oplot,rad,sgsmooth,color=220,thick=4
     oplot,minmax(rad),[data_background,data_background],linestyle=2,color=150,thick=4
     xyouts,!p.position[2]-0.16,0.87,'Orig. data',color=0,/norm,charsize=2
     xyouts,!p.position[2]-0.16,0.84,'smooth(data)',color=70,/norm,charsize=2
     xyouts,!p.position[2]-0.16,0.81,'savgol(data)',color=220,/norm,charsize=2
     
     ;Overplot the previous positions of the back, peak, and front to guide the eye.
     if ii gt sp and started_measurements eq 1 then begin
        loadct,0,/silent
        oplot, [lastbackEdge, lastbackEdge], !y.crange,color=50,thick=2
        oplot, [lastbackEdge,lastbackEdge], !y.crange,color=180,thick=2,linestyle=2
        oplot, [lastpeakrad,lastpeakrad], !y.crange,color=50,thick=2
        oplot, [lastpeakrad,lastpeakrad], !y.crange,color=180,thick=2,linestyle=2
        oplot, [lastfrontEdge, lastfrontEdge], !y.crange,color=50,thick=2
        oplot, [lastfrontEdge, lastfrontEdge], !y.crange,color=180,thick=2,linestyle=2
        loadct,39,/silent
     endif
     if ii eq sp then begin
        profile_p=!p
        profile_x=!x
        profile_y=!y
     endif
;------------------- END PROFILE PLOT ---------------------- 


;------------------- START OVERVIEW PLOT ----------------------  
;Plot the wave J-map with the limits and the current time step.
     ;wset,overview_window
     if ii gt sp then begin
        !p=overview_p
        !x=overview_x
        !y=overview_y
     endif else !p.position=overview_position
     !p.multi=overview_multi
     aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                        data[*,yrng[0]:yrng[1]],$
                        min=-20,max=50,title='',xtitle=plotinfo.xtitle,$
                        ytitle=plotinfo.ytitle,position=overview_position
     loadct,0,/silent
     oplot,[time[sp].jd,time[sp].jd]+dt2,!y.crange,color=0,thick=3
     oplot,[time[sp].jd,time[sp].jd]+dt2,!y.crange,color=100,thick=3,linestyle=2
     oplot,[time[ep].jd,time[ep].jd]+dt2,!y.crange,color=0,thick=3
     oplot,[time[ep].jd,time[ep].jd]+dt2,!y.crange,color=100,thick=3,linestyle=2
     loadct,39,/silent
     oplot,[time[ii].jd,time[ii].jd]+dt2,!y.crange,color=0,thick=3
     oplot,[time[ii].jd,time[ii].jd]+dt2,!y.crange,color=100,thick=3,linestyle=2
     if ii eq sp then begin
        overview_p=!p
        overview_x=!x
        overview_y=!y
     endif
;------------------- END OVERVIEW PLOT ----------------------
     
     print,''
     uinput='y'
     
     ;valid=0
     ;while valid eq 0 do begin
     ;   read,uinput,prompt='Can you identify the wave easily? Press "y" for yes, "n" for no, "q" to quit: '
     ;   if uinput eq 'y' or uinput eq 'n' or uinput eq 'q' then valid=1
     ;endwhile
     ;if uinput eq 'q' then return
     if uinput eq 'n' then begin
        if started_measurements eq 0 then rad_data.timefitrange[0]=rad_data.timefitrange[0]+1
        if started_measurements eq 1 then begin
           rad_data.timefitrange[1]=ii-1
           if (timefitrange[0] ne rad_data.timefitrange[0]) or $
              (timefitrange[1] ne rad_data.timefitrange[1]) then begin
              print,''
              print,'Time range of measurements has changed.'
              valid=0
              while valid eq 0 do begin
                 read,uinput,prompt='Save the new time range as default? Press "y" for yes, "n" for no: '
                 if uinput eq 'y' or uinput eq 'n' then valid=1
              endwhile
              if uinput eq 'y' then begin
                 
                 ;Save the extent of the wave, to use by default for future measurements.
                 openw,lun,last_saved_start_end,/get_lun
                 printf,lun,strtrim(string(rad_data.timefitrange[0]),2)
                 printf,lun,strtrim(string(rad_data.timefitrange[1]),2)
                 close,lun
                 print,''
                 print,'New START/END times recorded!'
              endif
           endif
           goto,donewithmeasurements
        endif
        print,'OK, moving on to the next time step...'
        continue
     endif
     
     uinput=''
     measurementredo:
     !p=profile_p
     !x=profile_x
     !y=profile_y
     !p.multi=profile_multi
     print,''
     print,'Please click on the BACK EDGE position of the wave (the edge closest to the Sun):'
     cursor,x,y,/data,/down
     tmp=min(where(rad gt x))
     if tmp[0] eq -1 then tmp = n_elements(rad)-1
     backEdge=rad[tmp]
     wave_backedge[ii].rad = backEdge     ;the radial position
     wave_backedge[ii].yind = tmp         ;the radial position index
     wave_backedge[ii].xind = ii          ;the time index
     wave_backedge[ii].val = diffcol[tmp] ;the intensity value
     oplot, [backEdge, backEdge], !y.crange,color=0,thick=3
     oplot, [backEdge, backEdge], !y.crange,color=150,thick=3,linestyle=2
     
     print,''
     print,'Please click on the PEAK position of the wave (highest intensity of the wave profile):'
     cursor,x,y,/data,/down
     tmp=min(where(rad gt x))
     if tmp[0] eq -1 then tmp = n_elements(rad)-1
     peakrad=rad[tmp]
     wave_peak[ii].rad = peakrad      ;the radial position
     wave_peak[ii].yind = tmp         ;the radial position index
     wave_peak[ii].xind = ii          ;the time index
     wave_peak[ii].val = diffcol[tmp] ;the intensity value
     oplot, [peakrad,peakrad], !y.crange,color=0,thick=3
     oplot, [peakrad,peakrad], !y.crange,color=150,thick=3,linestyle=2
     
     print,''
     print,'Please click on the FRONT EDGE position of the wave (the edge farthest from the Sun):'
     cursor,x,y,/data,/down
     tmp=min(where(rad gt x))
     if tmp[0] eq -1 then begin
        tmp = n_elements(rad)-1
        reached_front_edge=1
     endif
     frontEdge=rad[tmp]
     wave_frontedge[ii].rad = frontEdge    ;the radial position
     wave_frontedge[ii].yind = tmp         ;the radial position index
     wave_frontedge[ii].xind = ii          ;the time index
     wave_frontedge[ii].val = diffcol[tmp] ;the intensity value
     oplot, [frontEdge, frontEdge], !y.crange,color=0,thick=3
     oplot, [frontEdge, frontEdge], !y.crange,color=150,thick=3,linestyle=2
     

;------------------- START OVERVIEW PLOT ----------------------     
     ;Switch to the other plot, and show where the front,peak, and back edges are
     ;wset,window
     !p=overview_p
     !x=overview_x
     !y=overview_y
     !p.position=overview_position
     !p.multi=overview_multi
     ;aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
     ;                   data[*,yrng[0]:yrng[1]],$
     ;                   min=-20,max=50,title='Current measurement step',xtitle=plotinfo.xtitle,$
     ;                   ytitle=plotinfo.ytitle
     ;loadct,0,/silent
     ;oplot,[time[sp].jd,time[sp].jd]+dt2,!y.crange,color=0,thick=2
     ;oplot,[time[sp].jd,time[sp].jd]+dt2,!y.crange,color=100,thick=2,linestyle=2
     ;oplot,[time[ep].jd,time[ep].jd]+dt2,!y.crange,color=0,thick=2
     ;oplot,[time[ep].jd,time[ep].jd]+dt2,!y.crange,color=100,thick=2,linestyle=2
     loadct,39,/silent
     plots,time[ii].jd+dt2,wave_frontedge[ii].rad,psym=sym(3),symsize=2,color=130
     plots,time[ii].jd+dt2,wave_frontedge[ii].rad,psym=sym(8),symsize=2,color=0,thick=3
     plots,time[ii].jd+dt2,wave_peak[ii].rad,psym=sym(1),symsize=2,color=130
     plots,time[ii].jd+dt2,wave_peak[ii].rad,psym=sym(6),symsize=2,color=0,thick=3
     plots,time[ii].jd+dt2,wave_backedge[ii].rad,psym=sym(2),symsize=2,color=130
     plots,time[ii].jd+dt2,wave_backedge[ii].rad,psym=sym(7),symsize=2,color=0,thick=3
;------------------- END OVERVIEW PLOT ----------------------


     valid=0
     while valid eq 0 do begin
        print,'Are you happy with these selections? '
        read,uinput,prompt='Press "y" for yes, "n" for no, "c" to continue, "q" to quit: '
        if uinput eq 'y' or uinput eq 'n' or uinput eq 'q'  or uinput eq 'c' then valid=1
     endwhile
     if uinput eq 'q' then begin
        wdel,window
        exit_status=QUIT_STATUS
        return
     endif
     if uinput eq 'c' then begin
        wdel,window
        exit_status=CONTINUE_STATUS
        return
     endif
     if uinput eq 'n' then begin
        print,'Okay, let us redo the measurement.'


;------------------- START PROFILE PLOT ----------------------        
        ;wset,window
        !p=profile_p
        !x=profile_x
        !y=profile_y
        !p.multi=profile_multi
        plot, rad, col, xtitle='Radial distance, R!DS!N',ytitle='Pixel intensity',charthick=1,/ystyle,/xstyle,$
              title=strtrim(string(ii),2)+' Wave J-map profile, '+time[ii].cashew_time,$
              charsize=2,yrange=[-3,max(col)+1],xrange=minmax(rad),/nodata
        
        oplot,rad,col,color=0
        oplot,rad,colSmooth,color=70,thick=4
        oplot,rad,sgsmooth,color=220,thick=4
        oplot,minmax(rad),[data_background,data_background],linestyle=2,color=150,thick=4
        xyouts,!p.position[2]-0.16,0.87,'Orig. data',color=0,/norm,charsize=2
        xyouts,!p.position[2]-0.16,0.84,'smooth(data)',color=70,/norm,charsize=2
        xyouts,!p.position[2]-0.16,0.81,'savgol(data)',color=220,/norm,charsize=2
        ;Overplot the previous positions of the back, peak, and front to guide the eye.
        if ii gt sp then begin
           loadct,0,/silent
           oplot, [lastbackEdge, lastbackEdge], !y.crange,color=50,thick=2
           oplot, [lastbackEdge,lastbackEdge], !y.crange,color=180,thick=2,linestyle=2
           oplot, [lastpeakrad,lastpeakrad], !y.crange,color=50,thick=2
           oplot, [lastpeakrad,lastpeakrad], !y.crange,color=180,thick=2,linestyle=2
           oplot, [lastfrontEdge, lastfrontEdge], !y.crange,color=50,thick=2
           oplot, [lastfrontEdge, lastfrontEdge], !y.crange,color=180,thick=2,linestyle=2
           loadct,39,/silent
        endif
;------------------- END PROFILE PLOT ----------------------


;------------------- START OVERVIEW PLOT ----------------------
        ;Switch to the overview plot, and show where the front,peak, and back edges are
        ;wset,window
        !p=overview_p
        !x=overview_x
        !y=overview_y
        !p.multi=overview_multi
        aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                           data[*,yrng[0]:yrng[1]],$
                           min=-20,max=50,title='',xtitle=plotinfo.xtitle,$
                           ytitle=plotinfo.ytitle,position=overview_position
        loadct,0,/silent
        oplot,[time[sp].jd,time[sp].jd]+dt2,!y.crange,color=0,thick=3
        oplot,[time[sp].jd,time[sp].jd]+dt2,!y.crange,color=100,thick=3,linestyle=2
        oplot,[time[ep].jd,time[ep].jd]+dt2,!y.crange,color=0,thick=3
        oplot,[time[ep].jd,time[ep].jd]+dt2,!y.crange,color=100,thick=3,linestyle=2
        loadct,39,/silent
        oplot,[time[ii].jd,time[ii].jd]+dt2,!y.crange,color=0,thick=3
        oplot,[time[ii].jd,time[ii].jd]+dt2,!y.crange,color=100,thick=3,linestyle=2
;------------------- END OVERVIEW PLOT ----------------------
        
        
        goto,measurementredo
     endif else begin
        ;DEBUG (05/26/2017)
        if reached_front_edge eq 1 then begin
           ep=ii
           rad_data.timefitrange[1]=ep
           ;Record the start and end indices
           openw,lun,last_saved_start_end,/get_lun
           printf,lun,strtrim(string(sp),2)
           printf,lun,strtrim(string(ep),2)
           close,lun
           goto, donewithmeasurements
        endif
        ;END DEBUG (05/26/2017)
        print,'Great, let us move on!'
        lastbackedge=backedge
        lastpeakrad=peakrad
        lastfrontedge=frontedge
        if started_measurements eq 0 then started_measurements = 1
     endelse
     
     wait,0.5
  endfor
  
  donewithmeasurements:
  
  print,''
  print,'Great, all measurements have been recorded!'
  
  wdel,window
  rad_data.wave_frontedge=wave_frontedge
  rad_data.wave_backedge=wave_backedge
  rad_data.wave_peak=wave_peak
  
;Find the wave thicknesses and average intensities
;Wave thickness in Rsun
  rad_data.wavethick=rad_data.wave_frontedge.rad-rad_data.wave_backedge.rad
  
;Wave average intensity over time
  backinds=rad_data.wave_backedge.yind
  frontinds=rad_data.wave_frontedge.yind
  
  xrng=rad_data.timefitrange
  for tt=xrng[0],xrng[1] do $
     if backinds[tt] ge 0 and frontinds[tt] gt 0 and frontinds[tt]-backinds[tt] gt 0 $
     then rad_data.avgintense[tt]=mean(rad_data.diffdata[tt,backinds[tt]:frontinds[tt]])
!p.multi=0
end
