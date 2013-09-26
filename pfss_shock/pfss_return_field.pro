pro pfss_return_field,date,rstart=rstart,invdens=invdens,pfss_struct=pfss_struct,save=save
;PURPOSE:
; Return the PFSS field model
;CATEGORY:
; PFSS
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;pfss_restore, pfss_field_start_coord, pfss_trace_field,
;pfss_to_spherical, 
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;

;date='2011-01-25'
;/Users/kkozarev/AIA/algoTests/yaftawave/normalized_AIA_20110125_05_193_subdata.sav
;Set up the common block variables
;@pfss_data_block
;spherical_to_pfss,pfssData
if n_elements(date) eq 0 then begin
   print,'You need to supply a date string, like "2011-01-25"'
   return
endif
;Restore B-field model
  pfss_restore,pfss_time2file(date,/ssw_cat,/url)

;  starting points to be on a regular grid covering the full disk,
;  with a starting radius of r=1.00 Rsun
  if not keyword_set (rstart) then rstart=1.05
;  factor inverse to line density, i.e. lower values = more lines
  if not keyword_set(invdens) then invdens = 1 
  pfss_field_start_coord,5,invdens,radstart=rstart

;  trace the field lines passing through the starting point arrays
  pfss_trace_field, kind
@pfss_data_block
     ind=where(ptph lt 0.0)
     if ind[0] gt -1 then ptph[ind]+=2*!PI
     ind=where(ptph ge 2*!PI)
     if ind[0] gt -1 then ptph[ind]-=2*!PI
     ind=where(ptth lt 0.0)
     if ind[0] gt -1 then ptth[ind]+=2*!PI
     ind=where(ptth ge 2*!PI)
     if ind[0] gt -1 then ptth[ind]-=2*!PI
;Create a structure to hold the results. The data are in 
;(r,theta,phi) spherical/heliographic coordinate system:
;r is the distance away from sun-center in units of solar
;      radii, such that valid values are between 1 (the nominal
;      photosphere and 2.5 (the radius of the source surface).
;      theta and phi are respectively the colatitude and
;      longitude in radians.
  if keyword_set(pfss_struct) then begin 
     pfss_to_spherical,pfssData
     pfss_struct=pfssData
  endif
  
;pfss_data is a structure array of type
;{spherical_field_data, $
;  br:ptr_new(),bth:ptr_new(),bph:ptr_new(),bderivs:ptr_new(),$
;  nr:-1l,nlat:-1l,nlon:-1l,$
;  rix:ptr_new(),theta:ptr_new(),phi:ptr_new(),lat:ptr_new(),lon:ptr_new(),$
;  lonbounds:[-1d,-1d],str:ptr_new(),stth:ptr_new(),stph:ptr_new(),$
;  ptr:ptr_new(),ptth:ptr_new(),ptph:ptr_new(),nstep:ptr_new(),$
;  extra_objects:ptr_new()}
;      The three arrays ptr,ptth,ptph contain the coordinates of
;      all N field lines that have been traced.  Since the field
;      lines can be of arbitrary length, the array nstep contains
;      the number of points needed to define the Nth field line.
;      Thus, field line i (where i is between 0 and N-1) is
;      represented by the points ptr(0:nstep(i)-1,i), and likewise
;      for ptth and ptph.

;Get the Carrington coordinates. Returns an array [R,lon,lat] at
;specified time.
;Note: AIA's index already has the carrington coordinates in
;the tags CRLN_OBS and CRLT_OBS
;carrCoords=get_stereo_lonlat(date,'Earth',/degrees,system='Carrington')
;print,carrCoords



;Save the structure and Carrington coordinates of SDO to a sav file:
res=strsplit(date,'-',/extract)
dat=strtrim(res[0]+res[1]+res[2],2)
fname='pfss_results_'+dat+'_'+strtrim(string(rstart,format='(f3.1)'),2)+'Rs_dens_'+strtrim(string(invdens),2)+'.sav'
if keyword_set(save) then save,filename=fname,kind,ptr,ptth,ptph,nstep,br,bph,bth

end
