function aia_check_missing_files,files,cadence=cadence
  if n_elements(files) lt 2 then return,''
;A function to check whether there are files missing
  if files[0] eq '' then return, ''
  if not keyword_set(cadence) then cadence=12.
  nfiles=n_elements(files)
  tims=fltarr(nfiles)
  for ff=0,nfiles-1 do begin
     tmp=strsplit(file_basename(files[ff]),'_',/extract)
     if n_elements(tmp) lt 2 then continue
     tims[ff]=anytim(strmid(tmp[1],0,2)+':'+strmid(tmp[1],2,2)+':'+strmid(tmp[1],4,2))
  endfor
  ;Check for a missing file. There should be a time difference of more than cadence-3 seconds to declare a missing file.
  missind=where((tims[1:*]-tims[0:n_elements(tims)-2]) gt float(cadence+2))
  ;If there's a missing file, set the appropriate flag.
  if missind[0] ne -1 then missing=files[missind] else missing=''
  return,missing
end
