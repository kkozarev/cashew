pro test_aia_nrh_map_overlay
;Load multiple NRH data files, plot them against AIA data.
  basepath='/Volumes/Backscratch/Users/kkozarev/NRH/'
  files=['2i110127.01','2i110214.01','2i110215.01','2i110216.01','2i110307.01','2i110309.01','2i110529.01','2i110923.01','2i130118.01','2i130319.01','2i130517.01']
  stimes=['12:05','13:00','08:52','14:23','14:17','10:32','10:20','12:11','09:38','13:55','08:47']
  etimes=['12:12','13:10','09:00','14:40','14:30','10:43','10:40','12:20','09:44','14:15','09:00']
  
  
  ;TEST
  files=['2i110127.01','2i110214.01','2i110215.01','2i110307.01','2i110309.01','2i110529.01','2i130118.01','2i130319.01','2i130517.01']
  stimes=['12:05','13:00','08:52','14:17','10:32','10:20','09:38','13:55','08:47']
  etimes=['12:12','13:10','09:00','14:30','10:43','10:40','09:44','14:15','09:00']
  ;TEST
  
  files=['2i131106.01']
  stimes=['13:40']
  etimes=['14:00']
  fov=[-1100,-800,-300,0]

  nf=n_elements(files)
  dates=strarr(nf)
  for i=0,nf-1 do begin
     tmp=strsplit(files[i],'i.',/extract)
     dates[i]='20'+tmp[1]
     label=tmp[1]+'_'+tmp[2]
     event=load_events_info(label=label)
     ;print,'20'+dates[i]+' '+stimes[i]+'-'+etimes[i]
     ;Call the plotting routine
     aia_nrh_map_overlay,files[i],stimes[i],etimes[i],event=event,datapath=event.nrh_datapath,savepath=basepath+dates[i]+'/',fov=fov
  endfor
  
end



pro aia_nrh_map_overlay,file,stime,etime,datapath=datapath,savepath=savepath,base=base,run=run,event=event,fov=fov
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
;       fov  - field of view specification, in arcsec, format is [x1,y1,x2,y2]
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

  if not keyword_set(savepath) then savepath='./'
  if not keyword_set(datapath) then datapath='./'
  if not dir_exist(savepath) then mk_dir,savepath
  if keyword_set(event) then savepath=event.nrhpath
  
  ;read the NRH data
  if not file_exist(datapath+file) then begin
     print,''
     print,'File '+file+' does not exist. Quitting...'
     print,''
     return
  endif
  ;Load the NRH data
  read_nrh, file, nrh_ind, nrh_dat, HBEG=stime,HEND=etime,dir=datapath
  index2map,nrh_ind,nrh_dat,nrh_map
  if keyword_set(fov) then begin
     sub_map,nrh_map,smap,xrange=[fov[0],fov[2]],yrange=[fov[1],fov[3]]
     nrh_map=smap
  endif
  nrh_max=max(nrh_map.data)
  nnrh=n_elements(nrh_ind)
 
  ;Make base AIA map
  if keyword_set(base) or keyword_set(run) then begin
     st=aia_augment_timestring(nrh_ind[0].date_obs,0) ;just reformat the string time
     et=aia_augment_timestring(nrh_ind[0].date_obs,10)
     aia_load_data,st,et,'193',aia_ind,aia_dat,/local,/first,/remove_aec
     index2map,aia_ind,aia_dat,aia_basemap
     if keyword_set(run) then aia_runmap=aia_basemap
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
  for i=1,nnrh-1 do begin
     stri=strtrim(string(i),2)
     if i lt 10 then stri='0'+stri
     if i lt 100 then stri='0'+stri

     strin=strsplit(nrh_ind[i].date_obs,'-:/T .',/extract)
     strout=strin[0]+strin[1]+strin[2]+'_'+strin[3]+strin[4]+strin[5]
     strout=strin[0]+strin[1]+strin[2]+'_'+event.label+'_'+stri
     ;select the AIA data map
     st=aia_augment_timestring(nrh_ind[i].date_obs,0) ;just reformat the string time
     et=aia_augment_timestring(nrh_ind[i].date_obs,60)
     aia_load_data,st,et,'193',aia_ind,aia_dat,/local,/first,/norm;,/remove_aec
     index2map,aia_ind,aia_dat,aia_map
     
     ;Plot AIA base difference map
     if keyword_set(base) then begin
        aia_map=diff_map(aia_map,aia_basemap,/rotate)
        loadct,9,/silent
        plot_map,aia_map,/limb,dmin=-250,dmax=100,fov=nrh_map
        file='AIA_NRH_base_map_overlay_'+strout
     endif else begin
      ;Plot AIA running difference map
        if keyword_set(run) then begin
           saverunmap=aia_map
           aia_map=diff_map(aia_map,aia_runmap,/rotate)
           loadct,9,/silent
           plot_map,aia_map,fov=nrh_map,/limb,dmin=-250,dmax=100
           file='AIA_NRH_run_map_overlay_'+strout
        endif else begin
           aia_lct,r,g,b,wavelnth='193',/load
           plot_map,aia_map,fov=nrh_map,/limb,/log,dmin=50,dmax=3000
           file='AIA_NRH_raw_map_overlay_'+strout
        endelse
     endelse
     
     ;Overplot contours of NRH data
     plot_map,nrh_map[i],/over,/rotate,thick=3,/smooth,color=0,$
              levels=[0.35,0.45,0.55,0.65,0.75,0.85,0.95]*max(nrh_map[i].data)
     xyouts,0.36,0.875,'NRH '+nrh_ind[i].date_obs+' UT',/normal,color=0,charsize=1.4,charthick=1
     tvlct,rr,gg,bb,/get
     write_png,savepath+file+'.png',tvrd(/true),rr,gg,bb
     if i gt 0 and keyword_set(run) then aia_runmap=saverunmap
     ;stop
  endfor
  set_plot,'x'
end
