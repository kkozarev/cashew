pro callisto_file_search,event,instation,files,station=station
;PURPOSE
; This procedure searches in the local archive for eCallisto data for
; a particular event, and forms a properly formatted string array of files.
;The function order the filenames like this - rows are time, cols are frequency (from high freq to low)
;files = [['BIR_20111105_101500_02.fit.gz','BIR_20111105_103000_02.fit.gz'],$
;         ['BIR_20111105_101500_01.fit.gz','BIR_20111105_103000_01.fit.gz']]
;  
;CATEGORY:
;AIA/Radio.NRH
;
;INPUTS:
;       event - an event structure
;
;
;OPTIONAL INPUT:
;
;OUTPUT:
;
;       files - a string array of filenames to load for creating the
;               Callisto spectrum plot.
;
;OPTIONAL OUTPUT:
;       
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 01/29/2014

 ;Find out if there is Callisto data for this event, and return the files, in the appropriate form.
  tmp=strsplit(event.st,'/ ',/extract)
  date=tmp[0]+tmp[1]+tmp[2]
  tmp=file_search(event.callisto_datapath+instation+'*'+date+'*.fit.gz')
  if tmp[0] eq '' then begin
     files=''
     return
  endif
  res=file_basename(tmp,'.fit.gz')
  
  for ff=0, n_elements(res)-1 do begin
     tmp=strsplit(res[ff],'_',/extract)
     if ff eq 0 then begin
        obstation=tmp[0]
        obdate=tmp[1]
        obst=tmp[2]
        obfr=tmp[3]
     endif else begin
        obstation=[obstation,tmp[0]]
        obdate=[obdate,tmp[1]]
        obst=[obst,tmp[2]]
        obfr=[obfr,tmp[3]]
     endelse
  endfor
;Then, need to reconstruct and reorder the filenames properly into an [m,n] array based on times and frequencies.
;This will be the return value of the function callisto_file_search
;This reverses the frequency order.
  obfr=reverse(obfr)
  
  ndat=n_elements(uniq(obdate[sort(obdate)]))
  nst=n_elements(uniq(obst[sort(obst)]))
  nfr=n_elements(uniq(obfr[sort(obfr)]))
  stind=uniq(obst)
  files=strarr(nfr,nst)
  for fr=0,nfr-1 do begin
     for st=0,nst-1 do begin
        files[fr,st]=obstation[fr+st*nfr]+'_'+obdate[fr+st*nfr]+'_'+obst[fr+st*nfr]+'_'+obfr[fr+st*nfr]+'.fit.gz'
     endfor
  endfor

  
  station=obstation[0]
  
;After that, need to check the extent of the AIA event times, and match it to the extent of the Callisto data.
;For now, AIA time range takes preference. Assume that each file lasts 15 minutes.
;Develop this later, not so necessary for now...
;use stuff like strjoin(strsplit(anytim(event.st,/STIME),'-:',/extract),'')
  
  files=event.callisto_datapath+files
end
