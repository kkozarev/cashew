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
  

  nf=n_elements(files)
  dates=strarr(nf)
  for i=0,nf-1 do begin
     tmp=strsplit(files[i],'i.',/extract)
     dates[i]='20'+tmp[1]
     ;print,'20'+dates[i]+' '+stimes[i]+'-'+etimes[i]
     ;Call the plotting routine
     aia_nrh_map_overlay,files[i],stimes[i],etimes[i],datapath=basepath+'NRH_data/',savepath=basepath+dates[i]+'/',/rundiff;/basediff
  endfor
  
  

end



pro aia_nrh_map_overlay,file,stime,etime,datapath=datapath,savepath=savepath,basediff=basediff,rundiff=rundiff
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

  if not keyword_set(savepath) then savepath='./'
  if not keyword_set(datapath) then datapath='./'
  if not dir_exist(savepath) then mk_dir,savepath
  
  ;read the NRH data
  if not file_exist(datapath+file) then begin
     print,''
     print,'File '+file+' does not exist. Quitting...'
     print,''
     return
  endif
  read_nrh, file, nrh_ind, nrh_dat, HBEG=stime,HEND=etime,dir=datapath
  nrh_max=max(nrh_dat)
  nnrh=n_elements(nrh_ind)
  
  ;Make base AIA map
  if keyword_set(basediff) or keyword_set(rundiff) then begin
     st=aia_augment_timestring(nrh_ind[0].date_obs,0) ;just reformat the string time
     et=aia_augment_timestring(nrh_ind[nnrh-1].date_obs,0)
     aia_load_data,st,et,'193',aia_ind,aia_dat,/local,/first,/remove_aec
     index2map,aia_ind,aia_dat,aia_basemap
     if keyword_set(rundiff) then aia_runmap=aia_basemap
  endif
  
  
  ;MAIN LOOP
  set_plot,'z'
  !P.BACKGROUND=255
  !P.COLOR=0
  !P.THICK=2.6
  !P.CHARSIZE=1.2
  !P.CHARTHICK=2.0
  !P.POSITION=[0.1,0.1,0.95,0.95]
  for i=0,nnrh-1 do begin
     wdef,0,1200
;     stri=strtrim(string(i),2)
;     if i lt 10 then stri='0'+stri
;     if i lt 100 then stri='0'+stri
     strin=strsplit(nrh_ind[i].date_obs,'-:/T .',/extract)
     strout=strin[0]+strin[1]+strin[2]+'_'+strin[3]+strin[4]+strin[5]
     
     ;select the NRH data map
     index2map,nrh_ind[i],nrh_dat[*,*,i],nrh_map
     st=aia_augment_timestring(nrh_ind[i].date_obs,0) ;just reformat the string time
     et=aia_augment_timestring(nrh_ind[i].date_obs,60)
     aia_load_data,st,et,'193',aia_ind,aia_dat,/local,/first,/remove_aec
     index2map,aia_ind,aia_dat,aia_map
     
     ;Plot AIA base difference map
     if keyword_set(basediff) then begin
        aia_map.data=aia_map.data-aia_basemap.data
        loadct,9,/silent
        plot_map,aia_map,fov=nrh_map,/limb,dmin=-250,dmax=100
        file='AIA_NRH_basediff_map_overlay_'+strout
     endif else begin
      ;Plot AIA running difference map
        if keyword_set(rundiff) then begin
           saverunmap=aia_map
           aia_map.data=aia_map.data-aia_runmap.data
           loadct,9,/silent
           plot_map,aia_map,fov=nrh_map,/limb,dmin=-250,dmax=100
           file='AIA_NRH_rundiff_map_overlay_'+strout
        endif else begin
           aia_lct,r,g,b,wavelnth='193',/load
           plot_map,aia_map,fov=nrh_map,/limb,dmin=1,dmax=1000
           file='AIA_NRH_map_overlay_'+strout
        endelse
     endelse
     ;Overplot contours of NRH data
     plot_map,nrh_map,/over,/rotate,thick=3,/smooth,color=0,$
              levels=[0.15,0.25,0.35,0.45,0.55,0.65]*max(nrh_map.data)
     xyouts,0.36,0.875,'NRH '+nrh_ind[i].date_obs+' UT',/normal,color=0,charsize=1.4,charthick=1
     tvlct,rr,gg,bb,/get
     write_png,savepath+file+'.png',tvrd(),rr,gg,bb
     if i gt 0 and keyword_set(rundiff) then aia_runmap=saverunmap
     stop
  endfor
  set_plot,'x'
end
