pro test_pfss_shock_plot_all
event=load_events_info(label='paper')
pfss_shock_plot_all,event,/hires
pfss_shock_plot_all,event,/lores
end

pro pfss_shock_plot_all,event,_extra=_extra;lores=lores,hires=hires,
;PURPOSE:
; This is a wrapper procedure for all PFSS/SHOCK plotting procedures
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;       event - an event structure
;
;KEYWORDS:
;       lores - low resolution of the PFSS model
;       hires - high resolution of the PFSS model
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/30/2014
  
  pfss_get_field_line_info,event,pfssLines=pfssLines,_extra=_extra
  pfss_shock_plot_csgs,event,/png,pfssLines=pfssLines,/newtimes,_extra=_extra
  pfss_shock_plot_angular_influence,event,pfssLines=pfssLines,/newtimes,_extra=_extra
  pfss_shock_plot_angular_influence,event,/topview,pfssLines=pfssLines,/newtimes,_extra=_extra
  pfss_shock_plot_thetabn_stats,event,_extra=_extra
  pfss_shock_plot_crossing_angles,event,/oplot,pfssLines=pfssLines,_extra=_extra

end
