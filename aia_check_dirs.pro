pro aia_check_dirs,basepath,stt,ett
;PURPOSE:
;Check for existing archive AIA folders, create them if they don't exist
;
;CATEGORY:
;Backend
;
;INPUTS:
;
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 09/23/2013   
;
starttime=stt
endtime=ett

;Record the times in string arrays
if n_elements(stattime) eq 1 then begin
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

;check for year
if not dir_exist(basepath+st[0]) then begin
   exec='mkdir '+basepath+st[0]
   spawn,exec
endif

;check for month
if not dir_exist(basepath+st[0]+'/'+st[1]) then begin
   exec='mkdir '+basepath+st[0]+'/'+st[1]
   spawn,exec
endif

;check for day
if not dir_exist(basepath+st[0]+'/'+st[1]+'/'+st[2]) then begin
   exec='mkdir '+basepath+st[0]+'/'+st[1]+'/'+st[2]
   spawn,exec
endif

;check for hours
if not dir_exist(basepath+st[0]+'/'+st[1]+'/'+st[2]+'/H'+st[3]+'00') then begin
   exec='mkdir '+basepath+st[0]+'/'+st[1]+'/'+st[2]+'/H'+st[3]+'00'
   spawn,exec 
endif

if st[3] ne et[3] then begin
   for t=fix(st[3]),fix(et[3]) do begin
      if t lt 10 then hr='0'+strtrim(string(t),2) else hr=strtrim(string(t),2)
      if not dir_exist(basepath+st[0]+'/'+st[1]+'/'+st[2]+'/H'+hr+'00') then begin
         exec='mkdir '+basepath+st[0]+'/'+st[1]+'/'+st[2]+'/H'+hr+'00'
         spawn,exec
      endif
   endfor
endif
end 
