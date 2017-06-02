pro find_start_end_lateral_manual, event, lat_data, plotinfo, exit_status
;
;PURPOSE
;Procedure to automatically find initial estimates of the start end times of the EUV front
;Takes data, sums up pixel intensities at each time step, determines
;start and end times from a Gaussian fit and how the sum of intensities change
;
;INPUTS
;     LAT_DATA - annulus data structure from cashew_annulus_analyze_lateral
;OUTPUTS
;     STARTIND - index of front start position
;     ENDIND - index of front end position

                                ;To print out additional information set debug to 1
  debug = 0
  open_last_saved=1
  redoingit=0
  last_saved_start_end=''
  readit=''
  strh=strtrim(string(lat_data.latmeasind),2)
  if lat_data.latmeasind lt 10 then strh='0'+strh
  
  if lat_data.type eq 'lateral_left' then last_saved_start_end=event.annuluspath+'lateral_left_wave_extent_'+strh+'.last'
  if lat_data.type eq 'lateral_right' then last_saved_start_end=event.annuluspath+'lateral_right_wave_extent_'+strh+'.last'
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
     time=lat_data.time
     nt = n_elements(time)
     yrng=lat_data.latfitrange
     data=lat_data.data[*, yrng[0]:yrng[1]]
     rad=lat_data.y_rsun_array
     yarray=lat_data.x_deg_array
     dt2=(lat_data.time[1].jd-lat_data.time[0].jd)/2.
     ind=where(data lt 0.0)
     if ind[0] gt -1 then data[ind] = 0.0

     ;Go through and sum up the intensities for each time step
     ;totalPixVals=dblarr(nt)
     ;for tt=0,nt-1 do begin
     ;   tmp=total(data[tt,*])
     ;   totalPixVals[tt]=tmp
     ;endfor
     
     set_plot,'x'
     wdef,12,800
     !p.background=255
     !p.color=0
     ;date_label=label_date(DATE_FORMAT='%H:%I')
     loadct,39,/silent
     ;intrange=minmax(totalpixvals)
     ;plot,time.jd,totalpixvals,yrange=intrange,/ystyle,xtickformat='LABEL_DATE',charsize=2,$
     ;     title='J-map wave start/end detection, '+lat_data.label,$
     ;     xtitle='Time',ytitle='Intensity'
     
     measurementredo:
     
     aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],lat_data.data[*,yrng[0]:yrng[1]],min=-20,max=50,$
                        title=plotinfo.imgtit,xtitle=plotinfo.xtitle,ytitle=plotinfo.ytitle
     
     valid=0
     uinput=''
     print,''
     if redoingit eq 0 then begin
        while valid eq 0 do begin
           read,uinput,prompt='Do you see a wave profile in the image? Please type "y" for yes, "n" for no:'
           if uinput eq 'y' or uinput eq 'n' then valid=1
        endwhile
        if uinput eq 'n' then begin
           lat_data.timefitrange=[-1,-1]
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
     
     valid=0
     while valid eq 0 do begin
        read,uinput,prompt='Are you happy with these selections? Press "y" for yes, "n" for no: '
        if uinput eq 'y' or uinput eq 'n' then valid=1
     endwhile
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
  lat_data.timefitrange=[startInd, endInd]
end
