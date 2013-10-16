pro test_nrh_plot_maps
;Load multiple NRH data files, plot them against AIA data.
  basepath='/Volumes/Backscratch/Users/kkozarev/NRH/'
  files=['2i110127.01','2i110214.01','2i110215.01','2i110216.01','2i110307.01','2i110309.01','2i110529.01','2i110923.01','2i130118.01','2i130319.01','2i130517.01']
  stimes=['12:05','13:00','08:52','14:22','14:17','10:32','10:20','12:11','09:38','13:55','08:47']
  etimes=['12:12','13:10','09:00','14:40','14:30','10:43','10:40','12:20','09:44','14:15','09:00']
  

  ;TEST
  files=['2i130319.01','2i130517.01']
  stimes=['13:55','08:47']
  etimes=['13:57','09:00']
  ;TEST
  

  nf=n_elements(files)
  dates=strarr(nf)
  for i=0,nf-1 do begin
     tmp=strsplit(files[i],'i.',/extract)
     dates[i]=tmp[1]
     
     ;Call the plotting routine
     nrh_plot_maps,files[i],stimes[i],etimes[i],datapath=basepath+'NRH_data/',savepath=basepath+dates[i],/basediff
  endfor
  
  

end



pro nrh_plot_maps,file,stime,etime,datapath=datapath,savepath=savepath,basediff=basediff
  
  if not keyword_set(savepath) then savepath='./'
  if not keyword_set(datapath) then datapath='./'
  ;read the NRH data
  read_nrh, file, nrh_ind, nrh_dat, HBEG=stime,HEND=etime,dir=datapath
  nnrh=n_elements(nrh_ind)
  
  ;Make base AIA map
  st=aia_augment_timestring(nrh_ind[0].date_obs,0) ;just reformat the string time
  et=aia_augment_timestring(nrh_ind[0].date_obs,30)
  aia_load_data,st,et,'193',aia_ind,aia_dat,/first,/remove_aec
  index2map,aia_ind,aia_dat,aia_basemap

     ;MAIN LOOP
  for i=1,nnrh-1 do begin
     stri=strtrim(string(i),2)
     if i lt 10 then stri='0'+stri
     if i lt 100 then stri='0'+stri
     strin=strsplit(nrh_ind[i].date_obs,'-:/T .',/extract)
     strout=strin[0]+strin[1]+strin[2]+'_'+strin[3]+strin[4]+strin[5]
     
     ;select the
     index2map,nrh_ind[i],nrh_dat[*,*,i],nrh_map
     
     st=aia_augment_timestring(nrh_ind[i].date_obs,0) ;just reformat the string time
     et=aia_augment_timestring(nrh_ind[i].date_obs,60)

     aia_load_data,st,et,'193',aia_ind,aia_dat,/first,/remove_aec
     index2map,aia_ind,aia_dat,aia_map
     wdef,0,1024
     ;Plot AIA map
     ;aia_lct,r,g,b,wavelnth='193',/load
     loadct,9,/silent
     tvlct,rr,gg,bb,/get
     if keyword_set(basediff) then begin
        aia_map.data=aia_map.data-aia_basemap.data
        plot_map,aia_map,/limb,dmin=-250,dmax=100
        file='AIA_NRH_basediff_map_overlay_'+strout
     endif else begin
        plot_map,aia_map,/log,/limb,dmin=1,dmax=1000    
        file='AIA_NRH_map_overlay_'+strout
     endelse
     ;Overplot contours of NRH data
     plot_map,nrh_map,/over,/rotate,thick=2,color=0,levels=[0.15,0.25,0.35,0.45,0.55,0.65]*max(nrh_map.data)
     xyouts,0.36,0.875,'NRH '+nrh_ind[i].date_obs+' UT',/normal,color=255,charsize=1.4,charthick=1
     write_png,savepath+file+'.png',tvrd(),rr,gg,bb
  endfor
  
end
