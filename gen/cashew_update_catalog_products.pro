pro test_cashew_update_catalog_products
; Program to test run cashew_update_catalog_products
  subset=1
  if subset > 0 then begin
     subset_labels=['120307_01','120405_01','151104_01','151104_02','151104_03']
     cashew_update_catalog_products,subset=subset_labels
  endif
  
  all=0
  if all > 0 then begin
     cashew_update_catalog_products
  endif
  
end


pro cashew_update_catalog_products,subset=subset,force=force
;PURPOSE:
; This script cycles over all or some events and creates all the necessary
; products for the online CASHeW catalog. It should skip the ones that
; exist already, but could force their re-creation if supplied the
; /force keyword. However, this will take a long time, so be careful
; when using it.
;
;CATEGORY:
; CASHeW/System
;
;INPUTS:
;      
;
;KEYWORDS:
;     subset - run with a subset of events. Should be set to an array
;              of event label strings.
;                  
;OUTPUTS:
;      
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/27/2016

  events=load_events_info()
  if keyword_set(subset) then events=load_events_info(label=subset)
  
  wav=['94','131','171','193','211','335']
  swav=['193','211']
  movie_types=['araw','abase','arun','raw','base','run','teem_ratios','teem_differences']
  
  for e=0, n_elements(events)-1 do begin
     event = events[e]
     
     ;Load the AIA data for thannels 94,131,171,193,211,335
     for w=0,n_elements(wav)-1 do aia_load_data,event.st,event.et,wav[w] ;,/force
     ;Create the png files - raw, running, and base difference
     for w=0,n_elements(swav)-1 do aia_make_images,event,swav[w],/raw,/base,/run ;,/force
     ;Create the deprojected data for channels 193 and 211
     for w=0,n_elements(swav)-1 do aia_annulus_create,event,wav=swav[w],/raw,/base,/run ;,/force
     ;Create the deprojected png files - raw, running, and base difference
     for w=0,n_elements(swav)-1 do aia_annulus_plot,event,wav=swav[w],/raw,/base,/run
     ;Create the movies for the regular and deprojected data - raw, running, and base difference
     for w=0,n_elements(swav)-1 do $
        for mt=0,n_elements(movie_types)-1 do $
           aia_make_movies, event, movie_type=movie_types[mt], wav=swav[w] ;,/force
     ;Run the radial annulus kinematics analysis
     for w=0,n_elements(swav)-1 do aia_annulus_analyze_radial,event,wave=swav[w]
     ;Run the tangential annulus kinematics analysis
     ;for w=0,n_elements(swav)-1 do aia_annulus_analyze_tangential,event,wave=swav[w]
     ;Run the Aschwanden DEM model for the event
     aia_cfa_teem_run,event,/remove_aec ;,/force
     ;Created the DEM png files
     
     ;Created the EM base ratio and normalized base difference files
     
     ;Run the Aschwanden DEM model analysis
     aia_define_rois,event 
     aia_dem_analyze,event 
     ;Create the DEM n and T ratio plots
     aia_cfa_teem_plot_em_ratios,event
     ;Run the PFSS model for the event
     pfss_return_field,event,/hires,/save ;/force
     pfss_return_field,event,/lores,/save ;/force
     ;Run the CSGS model 
     pfss_shock_run_csgs,event
     ;Plot the shock crossing angles
     pfss_shock_plot_crossing_angles,event
     pfss_shock_plot_crossing_angles,event,/oplot
     ;Plot the Theta_BN statistics
     pfss_shock_plot_thetabn_stats,event
     ;Create heliospheric connectivity plots
     pfss_shock_plot_angular_influence,event
     pfss_shock_plot_angular_influence,event,/topview
     
     ;Create the animated GIFs for the catalog webpage
     
  endfor
     
end
