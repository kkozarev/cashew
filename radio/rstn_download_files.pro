pro test_rstn_download_files

one=0
if one eq 1 then begin
  ;date='20130511 17:20:30'
  ;download_rstn_files_date, date
  ;stop
  events=load_events_info(label='110607_01')
  for ev=0,n_elements(events)-1 do rstn_download_files,events[ev]
endif

all=1
if all eq 1 then begin
   events=load_events_info()
   for ev=0,n_elements(events)-1 do rstn_download_files,events[ev]
endif

end


pro check_tII_candidates
datapath='/data/george/corwav/candidates_nitta/'
eventlist='candidate_events_nitta.txt'
dlpath=datapath+'radio/'

;Read the list of events, and process the radio data
openr,lun,datapath+eventlist,/get_lun
line=''
readf,lun,line
while not eof(lun) do begin
readf,lun,line
res=strsplit(strcompress(line),' ',/extract)
;Skip the events marked with X as 'bad'
if where(res eq 'X') gt -1 then continue
datetime=res[0]+' '+res[1]
date=strjoin(strsplit(res[0],'/',/extract))
fulldate=date+' '+res[1]

;Download the radio data
   rstn_download_files_date, fulldate, dlpath=dlpath
;Plot the radio spectrum for ten minutes before until an hour after
;the quoted EUV wave start time.
   trange=[aia_augment_timestring(datetime,-600),aia_augment_timestring(datetime,3600)]
   rstn_plot_spectrum_date,date, dlpath=dlpath, trange=trange,datarange=[25,180]
  ; stop
endwhile


end


pro rstn_download_files, event,force=force
  baseurl='http://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-radio/rstn-spectral/'
  shortdate=strmid(event.date,2,6)
  year=strmid(event.date,0,4)
  month=strmid(event.date,4,2)
  day=strmid(event.date,6,2)
  hr=strmid(event.date,11,2)

;Find out which station is appropriate here, form the list of files
;according to it. 
  if fix(hr) ge 23 or fix(hr) lt 7 then begin
     ;if fix(hr) lt 7 then begin
     ;   newday=strtrim(string(fix(day)-1),2)
     ;   if newday lt 10 then newday='0'+newday
     ;   if newday eq 0 then begin
     ;      days=[31,28,31,30,31,30,31,31,30,31,30,31]
     ;      newday=strtrim(string(days[fix(month)-1]),2)
     ;   endif
     ;   shortdate=strmid(year,2,2)+month+newday
     ;endif
     station='learmonth'
     stationID='lm'
     checkfile=stationID+shortdate+'.srs'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  if fix(hr) ge 7 and fix(hr) lt 15 then begin
     station='san-vito'
     stationID='sv'
     checkfile=stationID+shortdate+'.srs'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  if fix(hr) ge 15 and fix(hr) lt 23 then begin
     station='sagamore-hill'
     stationID='k7'
     checkfile=stationID+shortdate+'.srs'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  
  ;First check if the file is there
  if not keyword_set(force) then begin
     fname=file_search(event.RSTN_datapath+checkfile,/fold_case)
     if fname[0] ne '' then begin
        print,'File '+fname[0]+' already exists. Re-run with /force to overwrite.'
        print,''
        return
     endif
  endif

  url=baseurl+station+'/'+year+'/'+month+'/'
  for ff=0,n_elements(filenames)-1 do begin
     filename=filenames[ff]
     exec='wget '+url+filename+' --ignore-case -nc -P '+event.RSTN_datapath
     spawn,exec
  endfor
  
  fname=file_search(event.RSTN_datapath+filenames[0],/fold_case)
  if fname[0] ne '' then spawn,'gunzip -f '+fname
  
end


pro rstn_download_files_date, date, dlpath=dlpath
  baseurl='http://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-radio/rstn-spectral/'
  datapath='/data/george/corwav/RSTN_data/'
  shortdate=strmid(date,2,6)
  year=strmid(date,0,4)
  month=strmid(date,4,2)
  hr=strmid(date,9,2)
  ;min=strmid(date,12,2)
  ;time=fix(hr*60+min)
  
  if keyword_set(dlpath) then datapath=dlpath
  
;Find out which station is appropriate here, form the list of files
;according to it. 

;Learmonth
;22:48 - 09:50
;1368 - 590 mins
  if fix(hr) ge 23 or fix(hr) lt 7 then begin
     station='learmonth'
     stationID='lm'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif

;San Vito
;03:45 - 17:47
;225 - 1067 mins
  if fix(hr) ge 7 and fix(hr) lt 15 then begin
     station='san-vito'
     stationID='sv'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif

;Sagamore Hill
;09:35 - 23:45
;575 - 1425
  if fix(hr) ge 15 and fix(hr) lt 23 then begin
     station='sagamore-hill'
     stationID='k7'
     filenames=[stationID+shortdate+'.srs.gz',stationID+shortdate+'.SRS.gz',$
                strupcase(stationID)+shortdate+'.srs.gz',strupcase(stationID)+shortdate+'.SRS.gz']
  endif
  
  url=baseurl+station+'/'+year+'/'+month+'/'

;Go through the different permutations of the file names by brute force.
  for ff=0,n_elements(filenames)-1 do begin
     filename=filenames[ff]
     tmp=strsplit(filename,'.',/extract)
     datafile=tmp[0]+'.'+tmp[1]
     
     ;Don't download the file unless necessary
     if file_search(datapath+datafile,/fold_case) eq '' then begin 
        exec='wget '+url+filename+' --ignore-case -nc -P '+datapath 
        spawn,exec
     endif     
  endfor
  fname=file_search(datapath+filenames[0],/fold_case)
  if fname ne '' then begin
     spawn,'gunzip -f '+fname
  endif
  
  
end
