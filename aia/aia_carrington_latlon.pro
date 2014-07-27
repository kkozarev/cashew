pro aia_carrington_latlon,event,lat,lon
;Return the Carrington latitude and lotitude, in degrees
  aia_load_data,event.st,event.et,'193',index,/nodata,/quiet
  if n_elements(index) gt 0 then begin
     lon=index[0].crln_obs
     lat=index[0].crlt_obs
  endif else begin
     lon=-1
     lat=-1
     print,'No data was found. Quitting...'
  endelse
end
