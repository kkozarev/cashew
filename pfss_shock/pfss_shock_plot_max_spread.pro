pro pfss_shock_plot_max_spread,event
;PURPOSE:
;Visualize the time-dependent CSGS model with interacting field lines.
;
;CATEGORY:
;PFSS_Shock
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
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/27/2014
;
return
 ;Find a file to load with the latest results of the CSGS model
  csgsfile=find_latest_file(event.pfsspath+'csgs_results_*') 
  if csgsfile eq '' then begin
     print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif
  
  ;Load the CSGS model results
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
  
  lon=ind_arr[0].crln_obs*!PI/180.
  maxFootpointExtent=fltarr(sstep)
  for sstep=0,nsteps-1 do begin
     pmax=max(abs(endpointCoords[sstep,*,*].ptph-lon))
     pmin=
     plotmax[sstep]=max(endpointCoords[sstep,*,*].ptph-lon)
  
  
end
