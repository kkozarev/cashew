pro test_aia_file_search
  
  st='2013/01/15 01:01:07'
  et='2013/01/15 01:03:10'
  wave='171'
  files=aia_file_search(st, et, wave,missing=missing,/check171)
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


function aia_file_search, stss, etss, wave,path=path,loud=loud,missing=missing,cfa=cfa,jsoc=jsoc,remove_aec=remove_aec,check171=check171
;PURPOSE:
;Search for AIA fits files from the local CfA archive.
;Assume searching on a single day only.
;
;CATEGORY:
;AIA/General
;
;INPUTS:
;sts, ets - start and end time (Example: '2011/01/15 00:00:10')
;wave - wavelength string
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
;
sts=stss
ets=etss
  if not keyword_set(path) then path='/Data/SDO/AIA/level1/'
  
  if n_elements(sts) eq 1 then begin
     st=strsplit(sts,' /:,.-T',/extract)
     if n_elements(st) eq 4 then st=[st,'00']
     if n_elements(st) eq 5 then st=[st,'00']
  endif
  
  if n_elements(ets) eq 1 then begin
     et=strsplit(ets,' /:,.-T',/extract)
     if n_elements(et) eq 4 then et=[et,'00']
     if n_elements(et) eq 5 then et=[et,'00']
  endif

  if keyword_set(check171) then begin
     tim171=aia_file_search(aia_augment_timestring(sts,-6),aia_augment_timestring(sts,6),'171')
     if tim171[0] ne '' then begin
        tmp=strsplit(tim171[0],'_',/extract)
        mind=n_elements(tmp)-2
        time=tmp[mind]
        st=[st[0],st[1],st[2],strmid(time,0,2),strmid(time,2,2),strmid(time,4,2)]
        sts=st[0]+'/'+st[1]+'/'+st[2]+' '+st[3]+':'+st[4]+':'+st[5]
     endif
  endif
  
  
  mins=st[4]
  basepath=path+st[0]+'/'+st[1]+'/'+st[2]+'/'
  
;figure out where to search
  
;Format the wavelength string properly
  while strlen(wave) lt 4 do wave='0'+wave
  
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
   file=file_search(locpath+'*_'+hmins[0,h]+hmins[1,h]+'*_'+wave+'.fits')
   if keyword_set(jsoc) then file=file_search(locpath+'*'+strmid(wave,1,3)+'A*'+hmins[0,h]+'_'+hmins[1,h]+'*.fits')
   
   if strtrim(file[0],2) eq '' then begin
      if size(err,/type) eq 0 then err=h else err=[err,h]
      continue
   endif else begin
      if size(files,/type) eq 0 then files=file else files=[files,file]
   endelse

endfor
if n_elements(err) gt 1 then err=err[1:*]
if keyword_set(missing) and files[0] ne '' then missing=reform(hmins[*,err])

;Check that the image times conform to the range given, even in the
;seconds.
;1. extract the times and convert them to seconds for easier
;comparison with sts and ets - initial and final times
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
finind=where(((times-anytim(sts)) ge 0.0) and ((anytim(ets)-times) gt 0.0))
files=files[finind]

;stop
fff=0
if fff gt 0 then begin
;Make sure the seconds are right too!
if files[0] ne '' then begin
   secind=intarr(n_elements(files))
   nn=0
   for i=0,n_elements(files)-1 do begin
      tmp=strsplit(files[i],'_',/extract)
      hr=strmid(tmp[1],0,2)
      min=strmid(tmp[1],2,2)
      sec=strmid(tmp[1],4,2)
      if (hr eq st[3] and min eq st[4] and sec ge st[5]) or $
      (hr eq et[3] and min eq et[4] and sec lt et[5]) then begin
         print,st[3]+' '+hr+' '+et[3]
         print,st[4]+' '+min+' '+et[4]
         print,st[5]+' '+sec+' '+et[5]
         print,''
         secind[nn]=i
         nn++
      endif
   endfor
   if nn gt 0 then secind=secind[0:nn-1] else secind=-1
   stop
   if secind[0] ne -1 then begin
      if n_elements(secind) eq n_elements(files) then remove,secind,files $
      else files=['']
   endif
endif
endif


if keyword_set(loud) and files[0] ne '' then begin 
    print,files
    print,''
    print,'Number of files found: '+strtrim(string(n_elements(files)),2)
    print,''
endif


if keyword_set(remove_aec) then begin
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




return,files

end
