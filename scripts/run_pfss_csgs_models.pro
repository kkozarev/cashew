pro run_pfss_csgs_models

event=load_events_info(label='paper')
wav='193'

;aia_load_data,event.st,event.et,wav,event=event,/subroi,/force

;aia_annulus_create,event,/force
;aia_annulus_analyze_radial,event,wave=wav,/constrain;,/auto

;aia_carrington_latlon,event,lat,lon
;aclon=lon+event.arlon
;aclat=lat+event.arlat
;box=[aclon-90.,aclat-90.,aclon+90.,aclat+90.]
;pfss_return_field,event,box=box,/save

pfss_shock_run_csgs,event,/lores
stop
pfss_shock_run_csgs,event,/hires

end
