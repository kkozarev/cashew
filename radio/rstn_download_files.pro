pro test_rstn_download_files
  ;date='20130511 17:20:30'
  ;download_rstn_files_date, date
  ;stop
  events=load_events_info(label='test')
  for ev=0,n_elements(events)-1 do rstn_download_files,events[ev]
end


pro rstn_download_files, event,force=force
  baseurl='http://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-radio/rstn-spectral/'
  shortdate=strmid(event.date,2,6)
  year=strmid(event.date,0,4)
  month=strmid(event.date,4,2)
  hr=strmid(event.date,11,2)
  print,hr
  
;Find out which station is appropriate here, form the list of files
;according to it. 
  if fix(hr) ge 22 or fix(hr) lt 8 then begin
     station='learmonth'
     stationID='lm'
     checkfile=stationID+shortdate+'.srs
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  if fix(hr) ge 8 and fix(hr) lt 16 then begin
     station='san-vito'
     stationID='sv'
     checkfile=stationID+shortdate+'.srs
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  if fix(hr) ge 16 and fix(hr) lt 22 then begin
     station='sagamore-hill'
     stationID='k7'
     checkfile=stationID+shortdate+'.srs
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  
  ;First check if the file is there
  if not keyword_set(force) then begin
     fname=file_search(event.IPS_datapath+checkfile,/fold_case)
     if fname[0] ne '' then begin
        print,'File '+fname[0]+' already exists. Re-run with /force to overwrite.'
        print,''
        return
     endif
  endif

  url=baseurl+station+'/'+year+'/'+month+'/'
  for ff=0,n_elements(filenames)-1 do begin
     filename=filenames[ff]
     exec='wget '+url+filename+' --ignore-case -nc -P '+event.IPS_datapath
     spawn,exec
  endfor

  fname=file_search(event.IPS_datapath+filenames[0],/fold_case)
  spawn,'gunzip -f '+fname
  
end


pro rstn_download_files_date, date
  baseurl='http://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-radio/rstn-spectral/'
  datapath='/data/george/corwav/IPS_data'
  shortdate=strmid(date,2,6)
  year=strmid(date,0,4)
  month=strmid(date,4,2)
  hr=strmid(date,09,2)

;Find out which station is appropriate here, form the list of files
;according to it. 
  if fix(hr) ge 22 or fix(hr) lt 8 then begin
     station='learmonth'
     stationID='lm'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  if fix(hr) ge 8 and fix(hr) lt 16 then begin
     station='san-vito'
     stationID='sv'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  if fix(hr) ge 16 and fix(hr) lt 22 then begin
     station='sagamore-hill'
     stationID='k7'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  
  url=baseurl+station+'/'+year+'/'+month+'/'

  for ff=0,n_elements(filenames)-1 do begin
     filename=filenames[ff]
     exec='wget '+url+filename+' --ignore-case -nc -P '+datapath
     spawn,exec
  endfor
  
  fname=file_search(datapath+filenames[0],/fold_case)
  spawn,'gunzip -f '+fname
  
end
