pro pfss_get_field_line_info,event,pfssLines,lores=lores,hires=hires,pfssfile=pfssfile,sph_data=sph_data
;PURPOSE:
;This procedure returns a structure with info/cordinates for all pfss
;field lines
;
;CATEGORY:
;PFSS
;
;INPUTS:
;       event - an event structure
;
;KEYWORDS:
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;transform_volume, sym
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/22/2014
;
  
  ;Load the PFSS results, and use the information to create the structure
  pfsspath=event.pfsspath
  date=event.date
  label=event.label
  pfssfname=file_search(pfsspath+'pfss_results_'+date+'_'+label+'_hires.sav')
  if keyword_set(lores) then pfssfname=file_search(pfsspath+'pfss_results_'+date+'_'+label+'_lores.sav')
  if keyword_set(hires) then pfssfname=file_search(pfsspath+'pfss_results_'+date+'_'+label+'_hires.sav')
  if keyword_set(pfssfile) then pfssfname=pfssfile

  ;Load the PFSS model results
  print,'Loading PFSS File '+pfssfname
  if pfssfname[0] ne '' then begin
     restore,pfssfname[0]
   print,'Loaded File '+pfssfname  
  endif else begin
     print,'No PFSS data present. Quitting...'
     return
  endelse
  
  ;SAVE THE FIELD LINE INFORMATION TO A STRUCTURE ARRAY
  nlines=n_elements(nstep)*1.0D
  pfssLine={npts:0L,ptr:dblarr(max(nstep)),ptth:dblarr(max(nstep)),ptph:dblarr(max(nstep)),$
            open:0,linid:0L,color:0.}
  pfssLines=replicate(pfssLine,nlines)
  linecols=fix(abs(randomu(10L,nlines,/uniform))*255.)

;MAIN TIME LOOP
  for ff=0.0D,nlines-1 do begin
     npt=nstep[ff]              ;the number of points on this particular line.
     pfssLines[ff].npts=npt
     pfssLines[ff].ptr=ptr[0:npt-1,ff]
     pfssLines[ff].ptth=ptth[0:npt-1,ff]
     pfssLines[ff].ptph=ptph[0:npt-1,ff]
     if kind[ff] eq 2 then pfssLines[ff].open=1 else pfssLines[ff].open=0
     pfssLines[ff].linid=ff
     pfssLines[ff].color=linecols[ff]
     if ff mod 100 eq 0 then print,'pfss_get_info: Line #'+strtrim(string(fix(ff)),2)+'/'+strtrim(string(fix(nlines)),2)
  endfor

end