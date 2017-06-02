;+
; NAME: pfss_surffield_restore
;
; PURPOSE: This function retrieves a surface-field IDL save file and puts the
;          results in a structure.
;
; INPUTS: A name or a URL of a snapshot from an evolving surface flux model
;         (usually acquired by first calling pfss_time2file with the
;         /surffield switch set)
;
; KEYWORD PARAMETERS: "quiet", if set prevents messages from appearing
;
; OUTPUTS: An anonymous structure the tags FLUXS, PHIS, THETAS, NFLUX, I,
;          RUNNUMBER, and NOW, which contain arrays of the locations and
;          strengths of surface-flux concentrations and ancillary data.  The
;          PHIS (longitude) and THETAS (colatitude) coordinates are in
;          radians, and the FLUXS are in units of 10^18 Mx.  The NFLUX tag is
;          the number of elements in the FLUXS field, RUNNUMBER is the number
;          of the evolving surface flux model, I is the serial number of
;          the snapshot, and NOW is the date/time of the snapshot.
;
; NOTES: - If this function fails, it returns -1.
;        - The original (Version 1) of the model corresponds to RUNNUMBER=48,
;          while Version 2 corresponds to RUNNUMBER=76.  If restoring from an
;          IDL save file and RUNNUMBER is unknown, Version 1 is assumed and
;          RUNNUMBER is set to 48.  If restoring from an HDF5 file and
;          RUNNUMBER is unknown, Version 2 is assumed and RUNNUMBER is set to
;          76.
;
; MODIFICATION HISTORY:
;   M.DeRosa - 25 Sep 2007 - created, somewhat based on pfss_restore.pro
;              19 Oct 2007 - fixed bug with restore process
;              12 Sep 2012 - added HDF5 capability and RUNNUMBER specification
;
;-

function pfss_surffield_restore,restore_file,quiet=quiet

;  idiot check
if n_elements(restore_file) eq 0 then begin
  print,'  ERROR in pfss_surffield_restore: needs name of a restore file'
  return,-1
endif

;  if non local, then sock_copy it and restore is
if not file_exist(restore_file) then begin
  if strpos(restore_file(0),'http') eq 0 then begin
    break_url,restore_file(0),IP,subdir,file
    outdir=get_temp_dir()
    locfile=concat_dir(outdir,file)
    if not file_exist(locfile) then begin
      if not keyword_set(quiet) then $
        print,'  MESSAGE from pfss_surffield_restore: '+$
        'retrieving PFSS surffield file via http...'
      sock_copy,restore_file(0),out_dir=outdir,use_network=1, $ 
        prog=is_member(!d.name,'win,x,mac',/wc,/ignore_case)
    endif
  endif else begin
    print,'  ERROR in pfss_surffield_restore: cannot find restore file'
    return,-1
  endelse
endif else locfile=restore_file

;  restore it and put important bits into a structure
if not keyword_set(quiet) then $
  print,'  MESSAGE from pfss_surffield_restore: restoring file'
if strupcase(strmid(locfile,2,/reverse)) eq 'SAV' then begin
  struct_temp=ssw_save2struct(locfile,only='fluxs,phis,thetas,nflux,now,i')
endif else begin  ;  assuming HDF5
  h5=h5_parse(locfile)
  tags=tag_names(h5)
  if total(strupcase(tags) eq 'EVOLVING_MODEL_SNAPSHOT') gt 0 then begin
    struct_temp=h5.evolving_model_snapshot._data
  endif else begin
    print,'  ERROR in pfss_surffield_restore: problem with input HDF file'
    return,-1
  endelse
endelse

;  return
return,struct_temp

end
