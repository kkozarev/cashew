function aia_file_search, st, et, wave,path=path,loud=loud,missing=missing,cfa=cfa,jsoc=jsoc
;Search for AIA fits files from the local CfA archive.
;Assume searching on a single day only.

  if not keyword_set(path) then path='/Data/SDO/AIA/level1/'
  
  if n_elements(st) eq 1 then begin
     st=strsplit(st,' /:,.-T',/extract)
     if n_elements(st) eq 4 then st=[st,'00']
     if n_elements(st) eq 5 then st=[st,'00']
  endif
  
  if n_elements(et) eq 1 then begin
     et=strsplit(et,' /:,.-T',/extract)
     if n_elements(et) eq 4 then et=[et,'00']
     if n_elements(et) eq 5 then et=[et,'00']
  endif
  
  mins=st[4]
  basepath=path+st[0]+'/'+st[1]+'/'+st[2]+'/'
  
  files=''
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
   file=find_file(locpath+'*_'+hmins[0,h]+hmins[1,h]+'*_'+wave+'.fits')
   if keyword_set(jsoc) then file=find_file(locpath+'*'+strmid(wave,1,3)+'A*'+hmins[0,h]+'_'+hmins[1,h]+'*.fits')
   
   if h eq 0 then $ 
      if file[0] ne '' then files=file else err=[err,h] $
      else $
         if file[0] ne '' then files=reform([files,reform(file)]) else err=[err,h]
endfor
if n_elements(err) gt 1 then err=err[1:*]

if keyword_set(missing) and files[0] ne '' then missing=reform(hmins[*,err])


;Make sure the seconds are right too!
if files[0] ne '' then begin
   secind=intarr(n_elements(files))
   nn=0
   for i=0,n_elements(files)-1 do begin
      tmp=strsplit(files[i],'_',/extract)
      hr=strmid(tmp[1],0,2)
      min=strmid(tmp[1],2,2)
      sec=strmid(tmp[1],4,2)
      if (hr eq st[3] and min eq st[4] and sec lt st[5]) or $
      (hr eq et[3] and min eq et[4] and sec gt et[5]) then begin
         secind[nn]=i
         nn++
      endif
   endfor
   if nn gt 0 then secind=secind[0:nn-1] else secind=-1
   if secind[0] ne -1 then remove,secind,files
endif


if keyword_set(loud) and files[0] ne '' then begin 
    print,files
    print,''
    print,'Number of files found: '+strtrim(string(n_elements(files)),2)
    print,''
endif


return,files

end
