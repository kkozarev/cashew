pro test_aia_nrh_map_overlay
  
  ;Run for a single or a few events, like this:
  one=1
  if one eq 1 then begin
     label=['130517_01']
     events=load_events_info(label=label)
     for ev=0,n_elements(events)-1 do $
        aia_nrh_map_overlay,events[ev],/base ;/run,/subroi
  endif
  
  ;Alternatively, run for all the events:
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        aia_nrh_map_overlay,event,/base ;/run,/subroi
     endfor
  endif
end


pro aia_nrh_map_overlay,event,datapath=datapath,savepath=savepath,base=base,run=run,subroi=subroi
;PURPOSE
; This procedure loads NRH and AIA data, plotting full-disk AIA maps
; with NRH contours overlaid.
;
;CATEGORY:
;AIA/Radio.NRH
;
;INPUTS:
;       file - the name of the NRH data file
;       (Example: '2i130517.01')
;	starttime - a string time
;	(Example: '08:47')	
;	endtime - a string time
;	(Example: '09:05')
;
;
;OPTIONAL INPUT:
;
;OUTPUT:
;
;       data - a datacube with the (prepped) data
;       index - a structure of indices for the data
;
;OPTIONAL OUTPUT:
;       savepath - path to save the output files.
;
;DEPENDENCIES:
;read_nrh, aia_augment_timestring,aia_load_data, index2map, plot_map
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 10/2013
;Update, 01/27/2014, KAK - improved appearances slightly


  tmp=strsplit(event.st,' ',/extract)
  stime=strmid(tmp[1],0,5)
  tmp=strsplit(event.et,' ',/extract)
  etime=strmid(tmp[1],0,5)
  tmp=strsplit(event.label,'_',/extract)
  file='2i'+tmp[0]+'.0*'
  
  
  if not keyword_set(savepath) then savepath=event.nrhpath
  if not keyword_set(datapath) then datapath=event.nrh_datapath
  if not dir_exist(savepath) then mk_dir,savepath
  
  ;read the NRH data
  res=file_exist(datapath+file)
  print,datapath+file
  if not res then begin
     print,''
     print,'File '+file+' does not exist. Quitting...'
     print,''
     return
  endif
  file=file_basename(find_file(datapath+file))
  

  ;Load the NRH data
  read_nrh, file, nrh_ind, nrh_dat, HBEG=stime,HEND=etime,dir=datapath
  index2map,nrh_ind,nrh_dat,nrh_map
  nrh_max=max(nrh_dat)
  nnrh=n_elements(nrh_ind)
  
  
  ;Make base AIA map
  if keyword_set(base) or keyword_set(run) then begin
     st=aia_augment_timestring(nrh_ind[0].date_obs,0) ;just reformat the string time
     et=aia_augment_timestring(nrh_ind[nnrh-1].date_obs,30)
     aia_load_data,st,et,'193',aia_ind,aia_dat,/local,/first,/remove_aec,/subroi,map=aia_basemap,event=event,/force
     ;index2map,aia_ind,aia_dat,aia_basemap
     if keyword_set(run) then aia_runmap=aia_basemap
     ;stop
  endif
  
  
  
  ;MAIN LOOP
  set_plot,'z'
  device, set_resolution=[1200,1200], SET_PIXEL_DEPTH=24, DECOMPOSED=0
  !P.BACKGROUND=255
  !P.COLOR=0
  !P.THICK=2.6
  !P.CHARSIZE=1.2
  !P.CHARTHICK=2.0
  !P.POSITION=[0.1,0.1,0.95,0.95]
  !P.font=1
  

  for tsp=1,nnrh-1 do begin

     strin=strsplit(nrh_ind[tsp].date_obs,'-:/T .',/extract)
     strout=strin[3]+strin[4]+strin[5]
     
     ;select the AIA data map
     event.st=aia_augment_timestring(nrh_ind[tsp-1].date_obs,0) ;just reformat the string time
     event.et=aia_augment_timestring(nrh_ind[tsp].date_obs,5)
     aia_load_data,event.st,event.et,'193',aia_ind,aia_dat,/local,/first,/remove_aec,/subroi,map=aia_map,event=event,/force
     ;index2map,aia_ind,aia_dat,aia_map
     ;stop
     
     ;Plot AIA base difference map
     if keyword_set(base) then begin
        plotmap=diff_map(aia_map[0],aia_basemap)
        ;plotmap=aia_map[0].data-aia_basemap.data
        loadct,9,/silent
        plot_map,plotmap,fov=nrh_map[tsp],/limb,dmin=-250,dmax=100,charsize=2,charthick=2
        file='aia_nrh_overlay_base_'+event.label+'_'+event.date+'_'+strout
     endif else begin
      ;Plot AIA running difference map
        if keyword_set(run) then begin
           saverunmap=aia_map[0]
           plotmap=aia_map[0].data-aia_runmap.data
           loadct,9,/silent
           plot_map,plotmap,fov=nrh_map[tsp],/limb,dmin=-250,dmax=100
           file='aia_nrh_overlay_run_'+event.label+'_'+event.date+'_'+strout
        endif else begin
           aia_lct,r,g,b,wavelnth='193',/load
           plot_map,aia_map[0],fov=nrh_map[tsp],/limb,dmin=1,dmax=1000
           file='aia_nrh_overlay_raw_'+event.label+'_'+event.date+'_'+strout
        endelse
     endelse
     
     ;Overplot contours of NRH data
     plot_map,nrh_map[tsp],/over,/rotate,thick=2,/smooth,color=0,charsize=2,charthick=2,$
              levels=[0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95]*max(nrh_map[tsp].data)
     xyouts,0.30,0.88,'NRH '+nrh_ind[tsp].date_obs+' UT',/normal,color=0,charsize=3,charthick=2
     tvlct,rr,gg,bb,/get
     if file_exist(savepath+file+'.png') then spawn, 'rm '+savepath+file+'.png'
     print,'Saving file '+savepath+file+'.png'
     write_png,savepath+file+'.png',tvrd(),rr,gg,bb
     if tsp gt 0 and keyword_set(run) then aia_runmap=saverunmap
     
  endfor
  set_plot,'x'
  close,/all
end
