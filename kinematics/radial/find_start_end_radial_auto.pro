pro find_start_end_radial_auto, event, rad_data, plotinfo, exit_status
;
;PURPOSE
;Procedure to manually find initial estimates of the start end times of the EUV front
;
;INPUTS
;     RAD_DATA - annulus data structure from aia_annulus_analyze_radial
;OUTPUTS
;     STARTIND - index of front start position
;     ENDIND - index of front end position

;To print out additional information set debug to 1
  debug = 0
  open_last_saved=0
  redoingit=0
  readit=''
  plot=0
  last_saved_start_end=event.annuluspath+'radial_wave_extent.last'
  if open_last_saved gt 0 and file_test(last_saved_start_end) then begin
     startInd=''
     endInd=''

     openr,lun,last_saved_start_end,/get_lun
     readf,lun,startInd
     readf,lun,endInd
     close,lun
     
     startInd=fix(startInd)
     endInd=fix(endInd)
     ;endif
  endif else open_last_saved=0

  if open_last_saved eq 0 then begin
     time=rad_data.time
     nt = n_elements(time)
     yrng=rad_data.radfitrange
     data=rad_data.data[*, yrng[0]:yrng[1]]
     rad=rad_data.y_rsun_array
     yarray=rad_data.y_rsun_array
     ind=where(data lt 0.0)
     dt2=(rad_data.time[1].jd-rad_data.time[0].jd)/2.
     
     if ind[0] gt -1 then data[ind] = 0.0
     
     if plot gt 0 then begin
        set_plot,'x'
        wdef,12,1200,800
        !p.background=255 
        !p.color=0
        loadct,39,/silent
        aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],rad_data.data[*,yrng[0]:yrng[1]],$
                           min=-20,max=50,title=plotinfo.imgtit,xtitle=plotinfo.xtitle,$
                           ytitle=plotinfo.ytitle
     endif
        
     
     ;Do the actual finding of the starting and ending times here.
     find_start_end_radial,rad_data
     
     startInd=rad_data.timefitrange[0]
     endInd=rad_data.timefitrange[1]
     if plot gt 0 then begin
        oplot,[time[startInd].jd,time[startInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=0,thick=3
        oplot,[time[startInd].jd,time[startInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=100,thick=3,linestyle=2
        oplot,[time[endInd].jd,time[endInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=0,thick=3
        oplot,[time[endInd].jd,time[endInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=100,thick=3,linestyle=2
     endif


     ;Save the extent of the wave, to use by default for future measurements.
     openw,lun,last_saved_start_end,/get_lun
     printf,lun,strtrim(string(startind),2)
     printf,lun,strtrim(string(endind),2)
     close,lun
     print,''
     print,'START/END times recorded. Continuing with the analysis.'
     
     if plot gt 0 then begin
        wait,3
        wdel,12
     endif
  endif
  ;The final result is written into rad_data
  rad_data.timefitrange=[startInd, endInd]
  
end
