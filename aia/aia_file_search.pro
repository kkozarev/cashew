pro test_aia_file_search
  
  wave='193'
  event=load_events_info(label='110607_01')
  files=aia_file_search(event.st, event.et, wave,missing=missing,/check171,event=event)
  print,files
  
  ;ff=aia_file_search(st, et, '171',/check171)
  ;print,ff;[0]
  ;print,''
  ;ff=aia_file_search(st, et, '171')
  ;print,ff;[0]
  ;print,''
  ;ff=aia_file_search(st, et, '193',/check171)
  ;print,ff;[0]
  ;print,''
  ;ff=aia_file_search(st, et, '193')
  ;print,ff;[0]
  ;print,''
end


function aia_file_search, sts, ets, wav,event=event,path=path,loud=loud,missing=missing,$
                          cfa=cfa,jsoc=jsoc,remove_aec=remove_aec,check171=check171,first=first
;PURPOSE:
;Search for AIA fits files from the local CfA archive.
;Assume searching on a single day only.
;
;CATEGORY:
;AIA/General
;
;INPUTS:
;sts, ets - start and end time (Example: '2011/01/15 00:00:10')
;wav - wavelength string
;
;KEYWORDS:
; path=path,loud=loud,missing=missing,cfa=cfa,jsoc=jsoc
; check171=check171 - For DEM, you need to assume that the order of
;files is always like this: 171, 211, 94, 335, 193, 131. So if there
;is a 171 file immediately preceding the period, shift the period
;itself to accomodate that file.
;
;
;OUTPUTS:
; Returns a string array of full path filenames of AIA data.
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/2010   
;10/01/2013, KAK - added a check for Automatic Exposure Control
;                  exposure frames - keyword remove_aec
;11/30/2013, KAK - integrated with the event structure, 
;
  starttime=sts
  endtime=ets
  default_path='/Data/SDO/AIA/level1/'
  
;Format the wavelength string properly
  wave=wav
  while strlen(wave) lt 4 do wave='0'+wave
  
  if not keyword_set(path) then $
     if keyword_set(event) then path=event.aia_datapath $
     else path=default_path
  

  if n_elements(starttime) eq 1 then begin
     st=strsplit(starttime,' /:,.-T',/extract)
     if n_elements(st) eq 4 then st=[st,'00']
     if n_elements(st) eq 5 then st=[st,'00']
  endif else begin
     st=starttime
  endelse
  
  if n_elements(endtime) eq 1 then begin
     et=strsplit(endtime,' /:,.-T',/extract)
     if n_elements(et) eq 4 then et=[et,'00']
     if n_elements(et) eq 5 then et=[et,'00']
  endif else begin
     et=endtime
  endelse
  
  if keyword_set(check171) then begin
     tmpst=aia_augment_timestring(sts,-6)
     tmpet=aia_augment_timestring(ets,6)
     tim171=aia_file_search(tmpst,tmpet,'171',path=path,remove_aec=remove_aec)
     if tim171[0] ne '' then begin
        tmp=strsplit(tim171[0],'_',/extract)
        mind=n_elements(tmp)-2
        time=tmp[mind]
        st=[st[0],st[1],st[2],strmid(time,0,2),strmid(time,2,2),strmid(time,4,2)]
        sts=st[0]+'/'+st[1]+'/'+st[2]+' '+st[3]+':'+st[4]+':'+st[5]
     endif
     ntotfiles=n_elements(tim171)
  endif
  mins=st[4]
  basepath=path+st[0]+'/'+st[1]+'/'+st[2]+'/'
  
;figure out where to search
  
  
;if the hours are different
  if st[3] ne et[3] then begin
     for t=fix(st[3]),fix(et[3]) do begin
        if t lt 10 then hr='0'+strtrim(string(t),2)
        if t ge 10 then hr=strtrim(string(t),2)
        if t eq fix(st[3]) then begin
           hours=st[3]
        endif else begin
           hours=[hours,hr]
        endelse
;This part deals with the minutes
        im=0
        em=59
        if t eq fix(st[3]) then begin
           im=fix(st[4])
           em=59
        endif
        if t eq fix(et[3]) then begin
           im=0
           em=fix(et[4])
        endif
        for m=im,em do begin
           if m lt 10 then min='0'+strtrim(string(m),2)
           if m ge 10 then min=strtrim(string(m),2)
           if m eq fix(st[4]) then begin
              if t eq fix(st[3]) then hmins=[st[3],st[4]]
              mins=min
           endif else begin
              mins=[mins,min]
              hmins=[[hmins],[hr,min]]
           endelse
        endfor
     endfor
  endif else begin
;if st[3] is equal to et[3]:
     for m=fix(st[4]),fix(et[4]) do begin
        if m lt 10 then min='0'+strtrim(string(m),2)
        if m ge 10 then min=strtrim(string(m),2)
        if m eq fix(st[4]) then begin
           hmins=[st[3],st[4]]
           mins=min
        endif else begin
           hmins=[[hmins],[st[3],min]]
           mins=[mins,min]
        endelse
     endfor
  endelse
  
  
  err=-1
;do the searching for the files
  for h=0,n_elements(hmins[0,*])-1 do begin
     locpath=basepath+'H'+hmins[0,h]+'00/'
     print,'Looking for file '+locpath+'*_'+hmins[0,h]+hmins[1,h]+'*_'+wave+'.fits',h, n_elements(hmins[0,*])-1
     
     if keyword_set(jsoc) then file=file_search(locpath+'*'+strmid(wave,1,3)+'A*'+hmins[0,h]+'_'+hmins[1,h]+'*.fits') $
     else file=file_search(locpath+'*_'+hmins[0,h]+hmins[1,h]+'*_'+wave+'.fits')
     
     if strtrim(file[0],2) eq '' then begin
        if not var_exist(err) then err=h else err=[err,h]
        continue
     endif else begin
        if not var_exist(files) then files=file else files=[files,file]
     endelse
     
  endfor
  if n_elements(err) gt 1 then err=err[1:*]
  
  if keyword_set(missing) and var_exist(files) then missing=reform(hmins[*,err])
  
;Check that the image times conform to the range given, even in the
;seconds.
;1. extract the times and convert them to seconds for easier
;comparison with sts and ets - initial and final times
  if var_exist(files) then begin
     times=dblarr(n_elements(files))
     cc=0
     for ii=0,n_elements(files)-1 do begin
        if files[ii] ne '' then begin
           tmp=strsplit(files[ii],'_',/extract)
           mind=n_elements(tmp)-2
           time=tmp[mind]
           times[ii]=anytim(st[0]+'/'+st[1]+'/'+st[2]+' '+strmid(time,0,2)+':'+strmid(time,2,2)+':'+strmid(time,4,2))
           cc++
        endif
     endfor
     
;compute the final time
     finind=where(((times-anytim(sts)) ge 0.0) and ((anytim(ets)-times) ge 0.0))
     
     if finind[0] ne -1 then begin 
        files=files[finind] 
     endif else begin
        files=''
     endelse
     
  endif else begin
     files=''
     times=0
  endelse
  
  if keyword_set(loud) and files[0] ne '' then begin 
     print,files
     print,''
     print,'Number of files found: '+strtrim(string(n_elements(files)),2)
     print,''
  
  
                                ;Check for missing files within the time period.
     cad=12
     
     missing=aia_check_missing_files(files,cadence=cad)
     if missing[0] ne '' then begin
        print,'Missing data files after the following files:'
        print,missing
        print,'Consider manually reviewing the missing data. Quitting...'
        return,-1
     endif
  endif

  if keyword_set(check171) then begin
     if ntotfiles ne n_elements(files) then begin
        print,''
        print,'Total number of files for band '+wav+' is '+strtrim(string(n_elements(files)),2)
        print,'Total number of files for reference band 171 is '+strtrim(string(ntotfiles),2)
        print,'There is a discrepancy between the number of files between the current and reference bands.'
        print,'Continuing with the smaller number of files...'
        print,''
        ntotfiles=n_elements(files)
     endif
  endif
  
  
  if keyword_set(remove_aec) and files[0] ne '' then begin
     read_sdo,files,ind,dat,/nodata
     mask=where(ind.aectype eq 0)
     if mask[0] ne -1 then begin
        files=files[mask]
        nfiles=n_elements(files)
        ind=0
        dat=0
        if keyword_set(loud) then begin
           print,''
           print,'Checking for images with Automatic Exposure Control (AEC) - where index.AECTYPE != 0'
           print,''
           wait,0.1
           print,'Updated list of files to load:'
           print,files
           print,''
           print,'Number of files found: '+strtrim(string(nfiles),2)
           print,''
           print,'---------------------------------------------------'
           print,''
        endif
     endif
  endif
  
  ;If nothing in the user's personal archive, search the CfA archive.
  if files[0] eq '' then begin
     if keyword_set(path) or keyword_set(event) then begin
        if path eq default_path then return,''
        files=aia_file_search(starttime,endtime,wav,path=default_path,remove_aec=remove_aec)
     endif
  endif
  if keyword_set(first) then return,files[0] $
  else return,files
  
end

