
pro save_duplicate_event_info,event
;This procedure will create an invisible .json file with the core info for each event given to it.
;Note: the event structure must have been populated prior to passing it to this procedure.
  path=event.savepath
  close,/all
  if not file_exist(event.json_savename) then begin
     print,'ERROR: Non-existing file: '+event.json_savename
     return
  endif
  openw,lun,event.json_savename,/get_lun
  printf,lun,'{'
  printf,lun,'     "label": "'+event.label+'",'
  printf,lun,'     "coordX": '+strtrim(string(event.coordx),2)+','
  printf,lun,'     "coordY": '+strtrim(string(event.coordy),2)+','
  printf,lun,'     "st": "'+event.st+'",'
  printf,lun,'     "et": "'+event.et+'",'
  printf,lun,'     "aiafov": ['+strtrim(string(event.aiafov[0]),2)+','+strtrim(string(event.aiafov[1]),2)+'],'
  printf,lun,'     "flareclass": "'+event.flareclass+'",'
  if event.typeII eq 1 then printf,lun,'     "typeII": true,'
  if event.typeII eq 0 then printf,lun,'     "typeII": false,'
  if event.loop eq 1 then printf,lun,'     "loop": true,'
  if event.loop eq 0 then printf,lun,'     "loop": false,'
  if event.filament eq 1 then printf,lun,'     "filament": true,'
  if event.filament eq 0 then printf,lun,'     "filament": false,'
  printf,lun,'     "comment": "'+event.comment+'",'
  if event.web eq 1 then printf,lun,'     "web": true,'
  if event.web eq 0 then printf,lun,'     "web": false,'
  printf,lun,'     "rstn_lookup": "'+event.rstn_lookup+'",'
  printf,lun,'     "callisto_lookup": "'+event.callisto_lookup+'",'
  printf,lun,'     "nrh_lookup": "'+event.nrh_lookup+'",'
  printf,lun,'}'
  close,lun
end

  
function load_events_info,printlabels=printlabels,label=label,quiet=quiet,globaljson=globaljson,reset=reset
;PURPOSE:
;
;This procedure will load the information for one or a list of events into an
;array of structures and return it.
;
;CATEGORY:
; AIA/Kinematic
;
;INPUTS:
;
;KEYWORDS:
; printlabels - if set, print just the labels of the events
; label - if set, return the information about the event with this
;         label or an error message if it doesn't exist
; quiet - don't produce any messages
; globaljson - use the global json file of the event instead of the global
; reset - resets the local json file by copying from the global json with all events
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 09/30/2013
;11/18/2013, Kamen Kozarev - added various folder elements
;01/28/2014, Kamen Kozarev - added the ability to specify multiple events via the label keyword.
;05/10/2017, Kamen Kozarev - Added functionality to save the individual event's core info in a
                                ;separate hidden JSON file in its data folder.


;----------------------------------------------------------------------------------------
  basepath=GETENV('CORWAV')
  datapath=GETENV('CORWAV_DATA')
  webbasepath=GETENV('CORWAV_WEB')
  trunk=GETENV('CORWAV_TRUNK')

;DEBUG
  globaljsonfile=trunk+'dat/events.json'
  jsonfile=globaljsonfile
  
  if keyword_set(label) then begin
     localjsonfile=basepath+'events/'+label+'/.'+label+'.json'
     jsonfile=localjsonfile
  endif
  if not file_exist(jsonfile) then jsonfile=globaljsonfile
  
  parse_events_info,jsonfile, labels=labels, coordX=coordX, coordY=coordY, sts=sts, ets=ets, typeII=typeII, loop=loop, filament=filament, comment=comment, flareclass=flareclass, aiafov=aiafov, nrh_lookup=nrh_lookup, callisto_lookup=callisto_lookup, rstn_lookup=rstn_lookup,web=web
;DEBUG
  
  if keyword_set(label) then begin
     nerr=0
     for ll=0,n_elements(label)-1 do begin
        tmp=where(labels eq label[ll])
        if tmp[0] eq -1 then begin
           if not keyword_set(quiet) then begin
              print,''
              print,'Event "'+label[ll]+'" does not exist!'
              print,''
              nerr++
           endif
           if nerr eq n_elements(label) then begin
              print,'Quitting...'
              return,-1
           endif
        endif else begin
           if var_exist(lind) then lind=[lind,tmp] else lind=tmp
        endelse
     endfor
     if n_elements(lind) lt n_elements(label) then print,'Running with existing event label(s).'
     
     labels=labels[lind]
     coordX=coordX[lind]
     coordY=coordY[lind]
     sts=sts[lind]
     ets=ets[lind]
     typeII=typeII[lind]
     loop=loop[lind]
     web=web[lind]
     filament=filament[lind]
     comment=comment[lind]
     flareclass=flareclass[lind]
     aiafov=aiafov[*,lind]
     nrh_lookup=nrh_lookup[lind]
     callisto_lookup=callisto_lookup[lind]
     rstn_lookup=rstn_lookup[lind]
  endif
  
  nevents=n_elements(labels)
  
  hemisphere=strarr(nevents)
  for ll=0,nevents-1 do if coordX[ll] lt 0. then hemisphere[ll]='E' else hemisphere[ll]='W'
  
;DATAPATHS
  aia_datapath=datapath+'AIA_data/'
  nrh_datapath=datapath+'NRH_data/'
  rstn_datapath=datapath+'RSTN_data/'
  callisto_datapath=datapath+'Callisto_data/'
  rhessi_datapath=datapath+'RHESSI_data/'
  euvi_datapath=datapath+'EUVI_data/'
  swap_datapath=datapath+'SWAP_data/'
  pfss_datapath=datapath+'PFSS_data/'
  
;SAVEPATHS
  savepath=basepath+'events/'   ;The base savepath
  webpath=webbasepath+'events/'
  
  ;FOLDER STRUCTURE
  folder_structure=['radio','annulusplot','kinematics','pfss','swap','ionization','particles','png','gif','movies','dem','yaftawave','euvi','csgs','dtc']
  ;folder_structure=['radio','annulusplot','kinematics','pfss','mfhc','csgs','swap','ionization','particles','png','gif','movies','dtc','yaftawave','euvi']
  subfolder_structure={radio:['NRH','RSTN','Callisto'],annulusplot:['araw','abase','arun'],png:['raw','base','run'],dem:['aschwanden','weber'],dtc:['aschwanden','weber']}
  
  
;----------------------------------------------------------------------------------------
  
  event={label:'',st:'',et:'',coordX:0,coordY:0,aiafov:intarr(2),hemisphere:'',date:'',$
         arlon:0.,arlat:0.,geomcorfactor:0.,flareclass:'',typeII:0,loop:0,filament:0,comment:'',web:0,$
         aia_datapath:'',nrh_datapath:'',rhessi_datapath:'',rstn_datapath:'',callisto_datapath:'',$
         rstn_lookup:'',callisto_lookup:'',nrh_lookup:'',$
         euvi_datapath:'',swap_datapath:'',pfss_datapath:'',savepath:'',webpath:'',$
         moviepath:'',radiopath:'',nrhpath:'',rstnpath:'',callistopath:'',annuluspath:'',pfsspath:'',mfhcpath:'',csgspath:'',$
         swappath:'',ionizationpath:'',aschdempath:'',weberpath:'',particlespath:'',$
         euvipath:'',dtcpath:'',pngpath:'',gifpath:'',yaftawavepath:'',kinematicspath:'',aia_fulldata_savename:'',aia_subdata_savename:'',json_savename:'',$
         $
         annplot:{$
        plot:{arun_savename:'',abase_savename:'',araw_savename:''},$
        savename:'',overviewplot_savename:'',bdiff_savename:'',$
          analyzed:{radial:{avg_savename:'',savename:'',plot_savename:''},lateral:{avg_savename:'',savename:'',rightplot_savename:'',leftplot_savename:''}}},$
         $
         aschdem:{$
        map_savename:'',total_savename:'',teem_table_savename:'',subrois_savename:'',region_series_savename:'',$
        plot:{$
        map_savename:'',em_ratios_savename:'',em_differences_savename:'',region_series_savename:'',$
        density_savename:'',temperature_savename:'',density_ratios_savename:'',temperature_ratios_savename:'',$
        density_differences_savename:'',temperature_differences_savename:''}},$
         pfss:{hiresmap_savename:'',loresmap_savename:''},$
         csgs:{$
        hires:{map_savename:'',plot_savename:'',thetabn:{plot_savename:'',overplot_savename:'',stats:{timeplot_savename:'',crossings_savename:'',timeradpos_savename:'',timeradposbinned_savename:'',distance_savename:''}},anginfluence:{plot_savename:'',topview_plot_savename:''}},$
        lores:{map_savename:'',plot_savename:'',thetabn:{plot_savename:'',overplot_savename:'',stats:{timeplot_savename:'',crossings_savename:'',timeradpos_savename:'',timeradposbinned_savename:'',distance_savename:''}},anginfluence:{plot_savename:'',topview_plot_savename:''}}$
              },$
         movies:{$
        raw_savename:'',base_savename:'',run_savename:'',$
        annplot:{araw_savename:'',abase_savename:'',arun_savename:''},$
        aschdem:{teem_savename:'',density_savename:'',temperature_savename:'',em_ratios_savename:'',em_differences_savename:'',density_ratios_savename:'',density_differences_savename:'',temperature_ratios_savename:'',temperature_differences_savename:''},$
        csgs:{hires:{plot_savename:'',thetabn:{plot_savename:'',overplot_savename:''},anginfluence:{plot_savename:'',topview_plot_savename:''}},lores:{plot_savename:'',thetabn:{plot_savename:'',overplot_savename:''},anginfluence:{plot_savename:'',topview_plot_savename:''}}},$
        yaftawave_savename:''},$
         folder_structure:folder_structure,subfolder_structure:subfolder_structure}
  
  events=replicate(event,nevents)
  
  for ev=0,nevents-1 do begin
     events[ev].label=labels[ev]
     events[ev].coordX=coordX[ev]
     events[ev].coordY=coordY[ev]
     events[ev].hemisphere=hemisphere[ev]
     
;An attempt to get the AR coordinates in degrees
     tmp=strsplit(sts[ev],' ',/extract)
     dat=tmp[0]
     tmp=strsplit(dat,'/',/extract)
     events[ev].date=tmp[0]+tmp[1]+tmp[2]
     res=arcmin2hel(coordX[ev]/60.,coordY[ev]/60.,date=events[ev].date)
     events[ev].arlat=res[0]
     events[ev].arlon=res[1]
     events[ev].geomcorfactor=abs(1.0/sin(events[ev].arlon*!PI/180.))
;if ev eq 3 then print,res[0]
     events[ev].st=sts[ev]
     events[ev].et=ets[ev]
     events[ev].typeII=typeII[ev]
     events[ev].loop=loop[ev]
     events[ev].filament=filament[ev]
     events[ev].web=web[ev]
;Field of view, in pixels
     events[ev].aiafov=aiafov[*,ev]
     events[ev].comment=comment[ev]
     events[ev].flareclass=flareclass[ev]
     events[ev].nrh_lookup=nrh_lookup[ev]
     events[ev].callisto_lookup=callisto_lookup[ev]
     events[ev].rstn_lookup=rstn_lookup[ev]
     
;DATAPATHS
;+======================================================================================================
     events[ev].aia_datapath=aia_datapath
     events[ev].nrh_datapath=nrh_datapath
     events[ev].callisto_datapath=callisto_datapath
     events[ev].rhessi_datapath=rhessi_datapath
     events[ev].rstn_datapath=rstn_datapath
     events[ev].swap_datapath=swap_datapath
     events[ev].euvi_datapath=euvi_datapath
     events[ev].pfss_datapath=pfss_datapath
;-======================================================================================================     


;SAVEPATHS
;+======================================================================================================
     events[ev].savepath=savepath+events[ev].label+'/'
     events[ev].webpath=webpath+events[ev].label+'/'
     events[ev].moviepath=events[ev].savepath+'movies/'
     events[ev].radiopath=events[ev].savepath+'radio/'
     events[ev].nrhpath=events[ev].radiopath+'NRH/'
     events[ev].rstnpath=events[ev].radiopath+'RSTN/'
     events[ev].callistopath=events[ev].radiopath+'Callisto/'
     events[ev].annuluspath=events[ev].savepath+'annulusplot/'
     events[ev].mfhcpath=events[ev].savepath+'pfss/'
     events[ev].pfsspath=events[ev].savepath+'pfss/'
     events[ev].csgspath=events[ev].savepath+'csgs/'
     events[ev].swappath=events[ev].savepath+'swap/'
     events[ev].ionizationpath=events[ev].savepath+'ionization/'
     events[ev].euvipath=events[ev].savepath+'euvi/'
     events[ev].dtcpath=events[ev].savepath+'dem/'
     events[ev].aschdempath=events[ev].dtcpath+'aschwanden/'
     events[ev].weberpath=events[ev].dtcpath+'weber/'
     events[ev].pngpath=events[ev].savepath+'png/'
     events[ev].gifpath=events[ev].savepath+'gif/'
     events[ev].yaftawavepath=events[ev].savepath+'yaftawave/'
     events[ev].kinematicspath=events[ev].savepath+'kinematics/'
     events[ev].particlespath=events[ev].savepath+'particles/'
;-======================================================================================================
     
     
;FILENAMES
;+======================================================================================================
     events[ev].aia_fulldata_savename="normalized_AIA_"+events[ev].date+'_'+events[ev].label+'_WAV.sav'
     events[ev].aia_subdata_savename="normalized_AIA_"+events[ev].date+'_'+events[ev].label+'_WAV_subdata.sav'
     events[ev].json_savename=events[ev].savepath+'.'+events[ev].label+'.json'
     
;Annulus reprojection
     events[ev].annplot.savename="aia_deprojected_annulus_"+events[ev].date+'_'+events[ev].label+'_WAV.sav'
     events[ev].annplot.bdiff_savename="aia_deprojected_annulus_bdiff_"+events[ev].date+'_'+events[ev].label+'_WAV.sav'
     events[ev].annplot.overviewplot_savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_overview_plot.png'
     events[ev].annplot.plot.arun_savename="arun/WAV/annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_run_NNN.png'
     events[ev].annplot.plot.abase_savename="abase/WAV/annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_base_NNN.png'
     events[ev].annplot.plot.araw_savename="araw/WAV/annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_raw_NNN.png'
     
     events[ev].annplot.analyzed.radial.savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_analyzed_radialSSSSS.sav'
     events[ev].annplot.analyzed.radial.avg_savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_analyzed_radial_avg.sav'
     events[ev].annplot.analyzed.radial.plot_savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_radialSSSSS.png'
     
     events[ev].annplot.analyzed.lateral.savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_analyzed_lateralSSSSS.sav'
     events[ev].annplot.analyzed.lateral.avg_savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_analyzed_lateral_avg.sav'
     events[ev].annplot.analyzed.lateral.rightplot_savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_lateral_rightSSSSS.png'
     events[ev].annplot.analyzed.lateral.leftplot_savename="annplot_"+events[ev].date+'_'+events[ev].label+'_WAV_lateral_leftSSSSS.png'
     
     
;Aschwanden DEM
     events[ev].aschdem.map_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_map.sav'
     events[ev].aschdem.total_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_tot.sav'
     events[ev].aschdem.teem_table_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_table.sav'
     events[ev].aschdem.subrois_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_map_subrois.sav'
     events[ev].aschdem.region_series_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_normalized_series_rNN.sav'
     
     events[ev].aschdem.plot.map_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_map.png'
     events[ev].aschdem.plot.em_ratios_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_em_ratios.png'
     events[ev].aschdem.plot.em_differences_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_em_differences.png'
     events[ev].aschdem.plot.density_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_density.png'
     events[ev].aschdem.plot.temperature_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_temperature.png'
     events[ev].aschdem.plot.density_ratios_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_density_ratios.png'
     events[ev].aschdem.plot.temperature_ratios_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_temperature_ratios.png'
     events[ev].aschdem.plot.density_differences_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_density_differences.png'
     events[ev].aschdem.plot.temperature_differences_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_HHMMSS_teem_temperature_differences.png'
     events[ev].aschdem.plot.region_series_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_normalized_series_rNN.png'
     
     
;PFSS Results
     events[ev].pfss.hiresmap_savename='pfss_results_'+events[ev].date+'_'+events[ev].label+'_hires.sav'
     events[ev].pfss.loresmap_savename='pfss_results_'+events[ev].date+'_'+events[ev].label+'_lores.sav'
     
     
;CSGS Results
     events[ev].csgs.hires.map_savename='csgs_results_'+events[ev].date+'_'+events[ev].label+'_hires.sav'
     events[ev].csgs.lores.map_savename='csgs_results_'+events[ev].date+'_'+events[ev].label+'_lores.sav'
     
     events[ev].csgs.hires.plot_savename='aia_pfss_shock_'+events[ev].date+'_'+events[ev].label+'_hires_HHMMSS.png'
     events[ev].csgs.hires.thetabn.plot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_hires_HHMMSS.png'
     events[ev].csgs.hires.thetabn.overplot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_hires_oplot_HHMMSS.png'
     events[ev].csgs.hires.anginfluence.plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_hires_HHMMSS'
     events[ev].csgs.hires.anginfluence.topview_plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_topview_hires_HHMMSS'
     events[ev].csgs.hires.thetabn.stats.timeplot_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_hires_time.png'
     events[ev].csgs.hires.thetabn.stats.crossings_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_hires_time_crossings.png'
     events[ev].csgs.hires.thetabn.stats.timeradpos_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_hires_time_radpos.png'
     events[ev].csgs.hires.thetabn.stats.timeradposbinned_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_hires_time_radpos_binned.png'
     events[ev].csgs.hires.thetabn.stats.distance_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_hires_distance.png'
     
     events[ev].csgs.lores.plot_savename='aia_pfss_shock_'+events[ev].date+'_'+events[ev].label+'_lores_HHMMSS.png'
     events[ev].csgs.lores.thetabn.plot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_lores_HHMMSS.png'
     events[ev].csgs.lores.thetabn.overplot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_lores_oplot_HHMMSS.png'
     events[ev].csgs.lores.anginfluence.plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_lores_HHMMSS.png'
     events[ev].csgs.lores.anginfluence.topview_plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_topview_lores_HHMMSS.png'
     events[ev].csgs.lores.thetabn.stats.timeplot_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_lores_time.png'
     events[ev].csgs.lores.thetabn.stats.crossings_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_lores_time_crossings.png'
     events[ev].csgs.lores.thetabn.stats.timeradpos_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_lores_time_radpos.png'
     events[ev].csgs.lores.thetabn.stats.timeradposbinned_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_lores_time_radpos_binned.png'
     events[ev].csgs.lores.thetabn.stats.distance_savename='thetabn_stats_'+events[ev].date+'_'+events[ev].label+'_lores_distance.png'
     
     
;MOVIE FILENAMES WILL GO HERE
     events[ev].movies.raw_savename='raw_WAV_'+events[ev].label+'.mp4'
     events[ev].movies.base_savename='base_WAV_'+events[ev].label+'.mp4'
     events[ev].movies.run_savename='run_WAV_'+events[ev].label+'.mp4'
     
     events[ev].movies.annplot.araw_savename='araw_WAV_'+events[ev].label+'.mp4'
     events[ev].movies.annplot.abase_savename='abase_WAV_'+events[ev].label+'.mp4'
     events[ev].movies.annplot.arun_savename='arun_WAV_'+events[ev].label+'.mp4'
     
     events[ev].movies.aschdem.teem_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_map.mp4'
     events[ev].movies.aschdem.density_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_density.mp4'
     events[ev].movies.aschdem.temperature_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_temperature.mp4'
     events[ev].movies.aschdem.em_ratios_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_em_ratios.mp4'
     events[ev].movies.aschdem.em_differences_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_em_differences.mp4'
     events[ev].movies.aschdem.density_ratios_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_density_ratios.mp4'
     events[ev].movies.aschdem.density_differences_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_density_differences.mp4'
     events[ev].movies.aschdem.temperature_ratios_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_temperature_ratios.mp4'
     events[ev].movies.aschdem.temperature_differences_savename='aschdem_'+events[ev].date+'_'+events[ev].label+'_teem_temperature_differences.mp4'
     
     events[ev].movies.csgs.hires.plot_savename='aia_pfss_shock_'+events[ev].date+'_'+events[ev].label+'_hires.mp4'
     events[ev].movies.csgs.hires.thetabn.plot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_hires.mp4'
     events[ev].movies.csgs.hires.thetabn.overplot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_hires_oplot.mp4'
     events[ev].movies.csgs.hires.anginfluence.plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_hires.mp4'
     events[ev].movies.csgs.hires.anginfluence.topview_plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_topview_hires.mp4'
     
     events[ev].movies.csgs.lores.plot_savename='aia_pfss_shock_'+events[ev].date+'_'+events[ev].label+'_lores.mp4'
     events[ev].movies.csgs.lores.thetabn.plot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_lores.mp4'
     events[ev].movies.csgs.lores.thetabn.overplot_savename='thetabn_'+events[ev].date+'_'+events[ev].label+'_lores_oplot.mp4'
     events[ev].movies.csgs.lores.anginfluence.plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_lores.mp4'
     events[ev].movies.csgs.lores.anginfluence.topview_plot_savename='aia_pfss_shock_angular_influence_'+events[ev].date+'_'+events[ev].label+'_topview_lores.mp4'
     
     events[ev].movies.yaftawave_savename='yaftawave_'+events[ev].date+'_'+events[ev].label+'_WAV.mp4'
     if not dir_exist(events[ev].savepath) then create_event_folders,events[ev]
     if keyword_set(reset) or not file_exist(events[ev].savepath+'.'+events[ev].label+'.json') then save_duplicate_event_info,events[ev]
  endfor

  if keyword_set(printlabels) then print,events.label
  
;Order the events by time
  tmpind=sort(anytim(events.st,/sec))
  events=events[tmpind]
  
  return,events
  
end
