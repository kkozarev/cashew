pro run_pfss_csgs_models

event=load_events_info(label='130423_01')
wav='193'

;aia_load_data,event.st,event.et,wav,event=event,/subroi,/force

;aia_annulus_create,event,/force
;aia_annulus_analyze_radial,event,wave=wav,/constrain;,/auto

aia_carrington_latlon,event,lat,lon
aclon=lon+event.arlon
aclat=lat+event.arlat
box=[aclon-90.,aclat-90.,aclon+90.,aclat+90.]
pfss_return_field,event,box=box,/hires,/save

;pfss_shock_run_csgs,event,/lores
;pfss_shock_plot_csgs,event,/lores,/png
;pfss_shock_plot_thetabn_stats,event,/lores
;pfss_shock_plot_crossing_angles,event,/lores,/oplot
;pfss_shock_plot_angular_influence,event,/lores
;pfss_shock_plot_angular_influence,event,/lores,/topview

;pfss_shock_run_csgs,event,/hires
;pfss_shock_plot_csgs,event,/hires,/png
;pfss_shock_plot_thetabn_stats,event,/hires
;pfss_shock_plot_crossing_angles,event,/hires,/oplot
pfss_shock_plot_angular_influence,event,/hires
pfss_shock_plot_angular_influence,event,/hires,/topview

end
