pro find_start_end_radial_manual, event, rad_data, plotinfo, exit_status
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
  open_last_saved=1
  redoingit=0
  readit=''
  last_saved_start_end=event.annuluspath+'radial_wave_extent.last'
  if file_test(last_saved_start_end) then begin
     startInd=''
     endInd=''
     openr,lun,last_saved_start_end,/get_lun
     readf,lun,startInd
     readf,lun,endInd
     close,lun
     print,''
     print,'The start/end time information exists from a previous measurement (startInd='+startInd+', endInd='+endind+').'
     valid=0
     while valid eq 0 do begin
        read,readit,prompt='Would you like to use it? Please type "y" for yes, "n" for no:'
        if readit eq 'y' or readit eq 'n' then valid=1
     endwhile
     if readit eq 'n' then $
        open_last_saved=0
     if readit eq 'y' then begin
        print, 'Using the last saved wave time extent information.'
        startInd=fix(startInd)
        endInd=fix(endInd)
     endif
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

                                ;Go through and sum up the intensities for each time step
                                ;totalPixVals=dblarr(nt)
                                ;for tt=0,nt-1 do begin
                                ;   tmp=total(data[tt,*])
                                ;   totalPixVals[tt]=tmp
                                ;endfor
     
     set_plot,'x'
     wdef,12,1200,800
     !p.background=255
     !p.color=0
                                ;date_label=label_date(DATE_FORMAT='%H:%I')
     loadct,39,/silent
                                ;intrange=minmax(totalpixvals)
                                ;plot,time.jd,totalpixvals,yrange=intrange,/ystyle,xtickformat='LABEL_DATE',charsize=2,$
                                ;     title='J-map wave start/end detection, '+rad_data.label,$
                                ;     xtitle='Time',ytitle='Intensity'

     measurementredo:
     
     aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],rad_data.data[*,yrng[0]:yrng[1]],min=-20,max=50,title=plotinfo.imgtit,xtitle=plotinfo.xtitle,ytitle=plotinfo.ytitle

     valid=0
     uinput=''
     print,''
     if redoingit eq 0 then begin
        while valid eq 0 do begin
           read,uinput,prompt='Do you see a wave profile in the image? Please type "y" for yes, "n" for no:'
           if uinput eq 'y' or uinput eq 'n' then valid=1
        endwhile
        if uinput eq 'n' then begin
           rad_data.timefitrange=[-1,-1]
           openw,lun,last_saved_start_end,/get_lun
           printf,lun,strtrim(string(-1),2)
           printf,lun,strtrim(string(-1),2)
           close,lun
           wdel,12
           return
        endif
     endif
     

     print,'Select the wave STARTING time of the wave by clicking on the plot.'
     select1:
     cursor,x,y,/data,/down
     tmp=min(where(time.jd gt x))
     if tmp[0] eq -1 then begin
        print,'Selection out of bounds. Please select STARTING time within the time range of the plot.'
        goto,select1
     endif else begin
        startInd=tmp-1
        oplot,[time[startInd].jd,time[startInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=0,thick=3
        oplot,[time[startInd].jd,time[startInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=100,thick=3,linestyle=2
     endelse
     
     print,'Select the wave ENDING time of the wave by clicking on the plot.'
     select2:
     cursor,x,y,/data,/down
     tmp=min(where(time.jd gt x))
     if tmp[0] eq -1 then begin
        print,'Selection out of bounds. Please select ENDING time within the time range of the plot.'
        goto, select2
     endif else begin
        endInd=tmp-1
        oplot,[time[endInd].jd,time[endInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=0,thick=3
        oplot,[time[endInd].jd,time[endInd].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],color=100,thick=3,linestyle=2
     endelse
     
     uinput=''
     valid=0
     while valid eq 0 do begin
        read,uinput,prompt='Are you happy with these selections? Press "y" for yes, "n" for no, "q" to quit: '
        if uinput eq 'y' or uinput eq 'n' or uinput eq 'q' then valid=1
     endwhile
     if uinput eq 'q' then return
     if uinput eq 'n' then begin
        print,'Okay, let us redo the measurement.'
        redoingit=1
        goto,measurementredo
     endif

     ;Save the extent of the wave, to use by default for future measurements.
     openw,lun,last_saved_start_end,/get_lun
     printf,lun,strtrim(string(startind),2)
     printf,lun,strtrim(string(endind),2)
     close,lun
     print,''
     print,'START/END times recorded, thank you! Continuing with the analysis.'
     wait,1
     wdel,12
  endif
                                ;The final result is written into rad_data
  rad_data.timefitrange=[startInd, endInd]
  
end
