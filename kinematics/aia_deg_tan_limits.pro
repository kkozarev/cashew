function aia_deg_tan_limits,x_deg_array,y_rsun_array,y_rsun_limits,lat_heights
;Find the indices of the lateral measurement heights
  nlatmeas=n_elements(lat_heights)
  ncols=n_elements(x_deg_array)
  htind=intarr(nlatmeas)
  x_good_lats=fltarr(nlatmeas,ncols)
  for ii=0,nlatmeas-1 do begin
     htind[ii]=min(where(y_rsun_array ge lat_heights[ii]))
                                ;Find where the lat_heights are larger than the radial data limits 'y_rsun_limits'.
     tt=where(y_rsun_limits-lat_heights[ii] ge 0.)
     if tt[0] ne -1 then x_good_lats[ii,tt]=y_rsun_array[htind[ii]]
  endfor
  return, x_good_lats
end
