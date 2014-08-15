pro test_pfss_shock_plot_all
event=load_events_info(label='130423_01')
pfss_shock_plot_all,event,/hires
pfss_shock_plot_all,event,/lores
end

pro pfss_shock_plot_all,event,lores=lores,hires=hires
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
;

if keyword_set(lores) or not keyword_set(hires) then begin
   pfss_get_field_line_info,event,pfssLines=pfssLines,/lores
   pfss_shock_plot_csgs,event,/lores,/png,pfssLines=pfssLines,/newtimes
   pfss_shock_plot_angular_influence,event,/lores,pfssLines=pfssLines,/newtimes
   pfss_shock_plot_angular_influence,event,/lores,/topview,pfssLines=pfssLines,/newtimes
   pfss_shock_plot_thetabn_stats,event,/lores
   pfss_shock_plot_crossing_angles,event,/lores,/oplot,pfssLines=pfssLines
endif

if keyword_set(hires) then begin
   pfss_get_field_line_info,event,pfssLines=pfssLines,/hires
   pfss_shock_plot_csgs,event,/hires,/png,pfssLines=pfssLines,/newtimes
   pfss_shock_plot_crossing_angles,event,/hires,/oplot,pfssLines=pfssLines
   pfss_shock_plot_angular_influence,event,/hires,pfssLines=pfssLines,/newtimes
   pfss_shock_plot_angular_influence,event,/hires,/topview,pfssLines=pfssLines,/newtimes
   pfss_shock_plot_thetabn_stats,event,/hires
endif

end
