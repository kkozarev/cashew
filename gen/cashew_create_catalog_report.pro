pro test_cashew_create_catalog_report
;A program to test cashew_create_catalog_report.
  labels=['110511_01','151104_01','151104_02','151104_03']
  cashew_create_catalog_report;,labels=labels
end


pro cashew_create_catalog_report,savename=savename,labels=labels
;The procedure, which runs the main program for individual events.
;By default it runs on all events
  if not keyword_set(savename) then savename='./cashew_catalog_report.json'
  
  if keyword_set(labels) then $
     events=load_events_info(label=labels) $
  else events=load_events_info()
  
  nevents=n_elements(events)
  result='['
  for ev=0,nevents-1 do begin
     event=events[ev]
     result+=cashew_create_catalog_report_main(event)
     if ev lt nevents-1 then result+=','
  endfor
  result+=']'
  openw,lun,savename,/get_lun
  printf,lun,result
  close,lun
end

function cashew_create_catalog_report_main,event
;PURPOSE:
; This procedure will crawl the CASHeW catalog directories and will
; a report on which products are created, and which not. It will save
; the information in a JSON file, and then generate a report webpage.
; Optionally, it will direct a script to create the missing products.
;
;CATEGORY:
; CASHeW/System
;
;INPUTS:
;      
;
;KEYWORDS:
;      
;      
;OUTPUTS:
;        
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 03/2016
  
  YES='true'
  NO='false'
  WAV='193' ;Reference wavelength to include in filenames
  WAVES=['131','171','193','211','335','94']
  NWAVES=n_elements(WAVES)
  NNN='???' ;Reference file number to include in filenames
  HHMMSS='??????'
  SSSSS='*'

;Go through the folders and file types one by one  
  result='{'
  
  ;The event label
  result+='"event":'+'"'+event.label+'",'
  

;AIA DATA PRODUCTS
;+======================================================================================================

  ;The normalized full data
  result+='"aia_fulldata":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.savepath+replace_string(event.aia_fulldata_savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
  
  ;The normalized subdata
  result+='"aia_subdata":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.savepath+replace_string(event.aia_subdata_savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
;-====================================================================================================== 



;ANNULUS REPROJECTION PRODUCTS
;+======================================================================================================

  ;The deprojected annulus
  result+='"aia_deprojected_annulus":{'
    for w=0,nwaves-1 do begin
     if file_exist(event.annuluspath+replace_string(event.annplot.savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
  
  ;The annulus overview plot
  result+='"aia_annulus_overviewplot":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.annuluspath+replace_string(event.annplot.overviewplot_savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
  
  ;The analyzed annulus kinematics radial data
  result+='"aia_annulus_analyzed_radial":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.annuluspath+replace_string(event.annplot.analyzed.radial.savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
  
  ;The analyzed annulus kinematics tangential data
  result+='"aia_annulus_analyzed_tangential":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.annuluspath+replace_string(event.annplot.analyzed.tangential.savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
  
  ;The analyzed annulus kinematics data
  result+='"aia_annplot_radial_plot":{'
  for w=0,nwaves-1 do begin
      tmp=replace_string(event.annplot.analyzed.radial.plot_savename,'SSSSS',SSSSS)
     if file_exist(event.annuluspath+replace_string(tmp,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'

  ;The analyzed annulus kinematics tangential right plots
  result+='"aia_annplot_tangential_right_plot":{'
  for w=0,nwaves-1 do begin
     tmp=replace_string(event.annplot.analyzed.tangential.rightplot_savename,'SSSSS',SSSSS)
     if file_exist(event.annuluspath+replace_string(tmp,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},' 
  
  ;The analyzed annulus kinematics tangential left plots
  result+='"aia_annplot_tangential_left_plot":{'
  for w=0,nwaves-1 do begin
     tmp=replace_string(event.annplot.analyzed.tangential.leftplot_savename,'SSSSS',SSSSS)
     if file_exist(event.annuluspath+replace_string(tmp,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
  
  ;The annulus running difference plots
  result+='"aia_annplot_arun":{'
  for w=0,nwaves-1 do begin
     tmp=replace_string(event.annplot.plot.arun_savename,'NNN',NNN)
     filename=replace_string(tmp,'WAV',WAVES[w])
     if file_exist(event.annuluspath+filename) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'

 ;The annulus base difference plots
  result+='"aia_annplot_abase":{'
  for w=0,nwaves-1 do begin
     tmp=replace_string(event.annplot.plot.abase_savename,'NNN',NNN)
     filename=replace_string(tmp,'WAV',WAVES[w])
     if file_exist(event.annuluspath+filename) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'

  ;The annulus raw plots
  result+='"aia_annplot_araw":{'
  for w=0,nwaves-1 do begin
     tmp=replace_string(event.annplot.plot.araw_savename,'NNN',NNN)
     filename=replace_string(tmp,'WAV',WAVES[w])
     if file_exist(event.annuluspath+filename) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
;-======================================================================================================
  
  

;ASCHWANDEN DEM PRODUCTS
;+======================================================================================================
  ;The Aschwanden DEM model result - Maps
  result+='"aschdem_map":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.map_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','

  ;The Aschwanden DEM model result - Totals
  result+='"aschdem_total":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.total_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result - Table
  result+='"aschdem_table":'
  if file_exist(event.aschdempath+event.aschdem.teem_table_savename) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result - Map Subrois
  result+='"aschdem_map_subrois":'
  if file_exist(event.aschdempath+event.aschdem.subrois_savename) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result - Region time series
  result+='"aschdem_region_series":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.region_series_savename,'NN','??')) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result plots - Maps
  result+='"aschdem_plot_map":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.map_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result plots - EM Ratios
  result+='"aschdem_plot_em_ratios":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.em_ratios_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','

  ;The Aschwanden DEM model result plots - EM Differences
  result+='"aschdem_plot_em_differences":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.em_differences_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','

  ;The Aschwanden DEM model result plots - Density
  result+='"aschdem_plot_density":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.density_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','

  ;The Aschwanden DEM model result plots - Density Ratios
  result+='"aschdem_plot_density_ratios":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.density_ratios_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','

  ;The Aschwanden DEM model result plots - Density Differences
  result+='"aschdem_plot_density_differences":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.density_differences_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result plots - Temperature
  result+='"aschdem_plot_temperature":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.temperature_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','

  ;The Aschwanden DEM model result plots - Temperature Ratios
  result+='"aschdem_plot_temperature_ratios":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.temperature_ratios_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result plots - Temperature Differences
  result+='"aschdem_plot_temperature_differences":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.temperature_differences_savename,'HHMMSS',HHMMSS)) then $
     result+=YES else result+=NO
  result+=','
  
  ;The Aschwanden DEM model result plots - Region time series
  result+='"aschdem_plot_region_series":'
  if file_exist(event.aschdempath+replace_string(event.aschdem.plot.region_series_savename,'NN','??')) then $
     result+=YES else result+=NO
  result+=','


;PFSS/CSGS PRODUCTS
;+======================================================================================================

  ;PFSS hires map
  result+='"pfss_hires_map":'
  if file_exist(event.mfhcpath+event.pfss.hiresmap_savename) then $
     result+=YES else result+=NO
  result+=','
  
  ;PFSS lores map
    result+='"pfss_lores_map":'
  if file_exist(event.mfhcpath+event.pfss.loresmap_savename) then $
     result+=YES else result+=NO
  result+=','

;=======================
  ;CSGS Hires products
  result+='"csgs_hires":{'
  ;map
  result+='"map":'
  if file_exist(event.mfhcpath+event.csgs.hires.map_savename) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn plotfile
  result+='"thetabn_plot_savename":'
  if file_exist(event.mfhcpath+replace_string(event.csgs.hires.thetabn.plot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn overplotfile
  result+='"thetabn_overplot_savename":'
  if file_exist(event.mfhcpath+replace_string(event.csgs.hires.thetabn.overplot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn overplotfile
  result+='"anginfluence_plot_savename":'
  if file_exist(event.mfhcpath+replace_string(event.csgs.hires.anginfluence.plot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn overplotfile
  result+='"anginfluence_topview_plot_savename":'
  if file_exist(event.mfhcpath+replace_string(event.csgs.hires.anginfluence.topview_plot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn stats timeplot
  result+='"thetabn_stats_timeplot":'
  if file_exist(event.mfhcpath+event.csgs.hires.thetabn.stats.timeplot_savename) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn stats crossings
  result+='"thetabn_stats_crossings":'
  if file_exist(event.mfhcpath+event.csgs.hires.thetabn.stats.crossings_savename) then $
     result+=YES else result+=NO
  result+=','
  
    ;Thetabn stats crossings
  result+='"thetabn_stats_timeradpos":'
  if file_exist(event.mfhcpath+event.csgs.hires.thetabn.stats.timeradpos_savename) then $
     result+=YES else result+=NO
  result+=','

    ;Thetabn stats crossings
  result+='"thetabn_stats_timeradposbinned":'
  if file_exist(event.mfhcpath+event.csgs.hires.thetabn.stats.timeradposbinned_savename) then $
     result+=YES else result+=NO
  result+=','

    ;Thetabn stats crossings
  result+='"thetabn_stats_distance":'
  if file_exist(event.mfhcpath+event.csgs.hires.thetabn.stats.distance_savename) then $
     result+=YES else result+=NO
  result+='},'
  
;=======================
  ;CSGS Lores products
  result+='"csgs_lores":{'
  ;map
  result+='"map":'
  if file_exist(event.mfhcpath+event.csgs.lores.map_savename) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn plotfile
  result+='"thetabn_plot_savename":'
  ;print,event.mfhcpath+replace_string(event.csgs.lores.thetabn.plot_savename,'NNN','???')
  if file_exist(event.mfhcpath+replace_string(event.csgs.lores.thetabn.plot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn overplotfile
  result+='"thetabn_overplot_savename":'
  if file_exist(event.mfhcpath+replace_string(event.csgs.lores.thetabn.overplot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn overplotfile
  result+='"anginfluence_plot_savename":'
  if file_exist(event.mfhcpath+replace_string(event.csgs.lores.anginfluence.plot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn overplotfile
  result+='"anginfluence_topview_plot_savename":'
  if file_exist(event.mfhcpath+replace_string(event.csgs.lores.anginfluence.topview_plot_savename,'NNN','???')) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn stats timeplot
  result+='"thetabn_stats_timeplot":'
  if file_exist(event.mfhcpath+event.csgs.lores.thetabn.stats.timeplot_savename) then $
     result+=YES else result+=NO
  result+=','
  
  ;Thetabn stats crossings
  result+='"thetabn_stats_crossings":'
  if file_exist(event.mfhcpath+event.csgs.lores.thetabn.stats.crossings_savename) then $
     result+=YES else result+=NO
  result+=','
  
    ;Thetabn stats crossings
  result+='"thetabn_stats_timeradpos":'
  if file_exist(event.mfhcpath+event.csgs.lores.thetabn.stats.timeradpos_savename) then $
     result+=YES else result+=NO
  result+=','

    ;Thetabn stats crossings
  result+='"thetabn_stats_timeradposbinned":'
  if file_exist(event.mfhcpath+event.csgs.lores.thetabn.stats.timeradposbinned_savename) then $
     result+=YES else result+=NO
  result+=','

    ;Thetabn stats crossings
  result+='"thetabn_stats_distance":'
  if file_exist(event.mfhcpath+event.csgs.lores.thetabn.stats.distance_savename) then $
     result+=YES else result+=NO
  result+='},'

  
; The Movies
;===================
  
  ;Annulusplot
  result+='"movies_annplot_araw":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.moviepath+replace_string(event.movies.annplot.araw_savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
  
  result+='"movies_annplot_abase":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.moviepath+replace_string(event.movies.annplot.abase_savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'
    
  result+='"movies_annplot_araw":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.moviepath+replace_string(event.movies.annplot.araw_savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='},'


  ;Aschwanden DEM
  result+='"movies_aschdem_teem_map":'
  if file_exist(event.moviepath+event.movies.aschdem.teem_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_aschdem_density":'
  if file_exist(event.moviepath+event.movies.aschdem.density_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_aschdem_temperature":'
  if file_exist(event.moviepath+event.movies.aschdem.temperature_savename) then $
     result+=YES else result+=NO
  result+=','
  
  result+='"movies_aschdem_em_ratios":'
  if file_exist(event.moviepath+event.movies.aschdem.em_ratios_savename) then $
     result+=YES else result+=NO
  result+=','
  
  result+='"movies_aschdem_em_differences":'
  if file_exist(event.moviepath+event.movies.aschdem.em_ratios_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_aschdem_density_ratios":'
  if file_exist(event.moviepath+event.movies.aschdem.density_ratios_savename) then $
     result+=YES else result+=NO
  result+=','
  
  result+='"movies_aschdem_density_differences":'
  if file_exist(event.moviepath+event.movies.aschdem.density_differences_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_aschdem_temperature_ratios":'
  if file_exist(event.moviepath+event.movies.aschdem.temperature_ratios_savename) then $
     result+=YES else result+=NO
  result+=','
  
  result+='"movies_aschdem_temperature_differences":'
  if file_exist(event.moviepath+event.movies.aschdem.temperature_differences_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_hires_plot":'
  if file_exist(event.moviepath+event.movies.csgs.hires.plot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_hires_thetabn_plot":'
  if file_exist(event.moviepath+event.movies.csgs.hires.thetabn.plot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_hires_thetabn_overplot":'
  if file_exist(event.moviepath+event.movies.csgs.hires.thetabn.overplot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_hires_anginfluence_plot":'
  if file_exist(event.moviepath+event.movies.csgs.hires.anginfluence.plot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_hires_anginfluence_topview_plot":'
  if file_exist(event.moviepath+event.movies.csgs.hires.anginfluence.topview_plot_savename) then $
     result+=YES else result+=NO
  result+=','
  
  result+='"movies_csgs_lores_plot":'
  if file_exist(event.moviepath+event.movies.csgs.lores.plot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_lores_thetabn_plot":'
  if file_exist(event.moviepath+event.movies.csgs.lores.thetabn.plot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_lores_thetabn_overplot":'
  if file_exist(event.moviepath+event.movies.csgs.lores.thetabn.overplot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_lores_anginfluence_plot":'
  if file_exist(event.moviepath+event.movies.csgs.lores.anginfluence.plot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_csgs_lores_anginfluence_topview_plot":'
  if file_exist(event.moviepath+event.movies.csgs.lores.anginfluence.topview_plot_savename) then $
     result+=YES else result+=NO
  result+=','

  result+='"movies_yaftawave":{'
  for w=0,nwaves-1 do begin
     if file_exist(event.moviepath+replace_string(event.movies.yaftawave_savename,'WAV',WAVES[w])) then $
        result+='"'+WAVES[w]+'":'+YES $
     else result+='"'+WAVES[w]+'":'+NO
     if w lt nwaves-1 then result+=','
  endfor
  result+='}'
  result+='}'
  
  ;print,result
  return,result
end
