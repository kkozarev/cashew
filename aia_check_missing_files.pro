function aia_check_missing_files,files,cadence=cadence
;A function to check whether there are files missing
  if not keyword_set(cadence) then cadence=12.
  
  nfiles=n_elements(files)
  tims=fltarr(nfiles)
  for ff=0,nfiles-1 do begin
     tmp=strsplit(file_basename(files[ff]),'_',/extract)
     tims[ff]=anytim(strmid(tmp[1],0,2)+':'+strmid(tmp[1],2,2)+':'+strmid(tmp[1],4,2))
  endfor
  ;Check for a missing file. There should be a time difference of more than cadence-3 seconds to declare a missing file.
  missind=where(((tims-tims[0])-findgen(nfiles)*float(cadence)) gt float(cadence-3))
  ;If there's a missing file, set the appropriate flag.
  if missind[0] ne -1 then missing=files[missind] else missing=''
  return,missing
end
