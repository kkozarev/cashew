pro pfss_shock_generate_csgs_radii,ind_arr,rad_data,radius,newtime=newtime,tindrange=tindrange
;PURPOSE:
;Return the radius of the CSGS surface based on CSGS
;results. Optionally, do interpolation for different times.
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;       ind_arr - the index array from the annulusplot analysis results
;       rad_data - the radial data fits from the annulusplot analysis
;                  results
;
;KEYWORDS:
;       newtime - a different time series of times (in seconds,
;                 relative to the first time)
;OUTPUTS:
;       radius - the calculated CSGS radius for every time step, in Rsun
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/30/2014
;
  sp=rad_data.timefitrange[0]
  ep=rad_data.timefitrange[1]
  begtime=rad_data.time[sp].relsec
  endtime=rad_data.time[ep].relsec
  time=(rad_data.time[sp:ep].relsec-rad_data.time[sp].relsec)
  if keyword_set(newtime) then begin
     tmp=min(where(newtime ge fix(begtime)))
     if tmp[0] ne -1 then begin
        sp=tmp
        begtime=newtime[tmp]
     endif
     
     tmp=max(where(newtime le fix(endtime)))
     if tmp[0] ne -1 then begin
        ep=tmp
        endtime=newtime[tmp]
     endif
     time=newtime[sp:ep]-newtime[sp]
     newtime=time
  endif
  tindrange=[sp,ep]
  
  nsteps=n_elements(time)
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.  
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  fit=reform(rad_data.fitparams.front)
  radius=(fit[0]+fit[1]*time+0.5*fit[2]*time^2)/RSUN
  radius-=1.
end
