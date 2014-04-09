pro aia_carrington_latlon,event,lat,lon
;Return the Carrington latitude and lotitude, in degrees
aia_load_data,event.st,aia_augment_timestring(event.st,20),'193',index,data,/nodata
lon=index[0].crln_obs
lat=index[0].crlt_obs
end
