pro test_pfss_shock_plot_all
event=load_events_info(label='110607_01')
;pfss_shock_plot_all,event,/hires,/force
pfss_shock_plot_all,event,/lores,/force
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
  event=load_events_info(label=event.label)
  pfss_get_field_line_info,event,pfssLines=pfssLines,_extra=_extra
  event=load_events_info(label=event.label)
  pfss_shock_plot_csgs,event,/png,pfssLines=pfssLines,_extra=_extra;,/newtimes
  event=load_events_info(label=event.label)
  pfss_shock_plot_angular_influence,event,pfssLines=pfssLines,_extra=_extra ;,/newtimes
  event=load_events_info(label=event.label)
  pfss_shock_plot_angular_influence,event,/topview,pfssLines=pfssLines,_extra=_extra;,/newtimes
  event=load_events_info(label=event.label)
  pfss_shock_plot_thetabn_stats,event,_extra=_extra
  event=load_events_info(label=event.label)
  pfss_shock_plot_crossing_angles,event,/oplot,pfssLines=pfssLines,_extra=_extra

end
