pro aia_check_dirs,basepath,st,et
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
