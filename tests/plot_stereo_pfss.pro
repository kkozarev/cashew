pro test_plot_stereo_pfss
  ;Test run the STEREO/PFSS overplotting routine
  event=load_events_info(label='110511_01')
  plot_stereo_pfss,event,/saved
end




pro plot_stereo_pfss,event,saved=saved,png=png,wav=wav
  ;Overplot PFSS models on AIA and STEREO
  if not keyword_set(wav) then wav='195'
  event=load_events_info(label=event.label)
  date=event.date
  label=event.label
  path='./'+wav+'/'

  
  ;The procedure to get the Carrington coordinates
  ;earthcoords=get_stereo_lonlat(date,'Earth',/degrees,system='Carrington')
  ;stacoords=get_stereo_lonlat(date,'sta',/degrees,system='Carrington')
  ;stbcoords=get_stereo_lonlat(date,'stb',/degrees,system='Carrington')
  
  
;------------------------------------------------------------  
;Load the A-maps
  amaps_fnames=file_basename(file_search(path+date+'*n4euA.fts'))
  namaps=n_elements(amaps_fnames)
  if not keyword_set(saved) then begin
     abckg_map=mk_secchi_map(path+amaps_fnames[0],/rotate_on)
     amaps=replicate(abckg_map,namaps)
     amaps_bdiff=amaps
     cc=0
     for i=0,n_elements(amaps_fnames)-1 do begin
        tmpmap=mk_secchi_map(path+amaps_fnames[i],/rotate_on)
        if where(strsplit(tmpmap.id,' ',/extract) eq wav) eq -1 then continue
        amaps[cc]=tmpmap
        amaps_fnames[cc]=amaps_fnames[i]
       ; if cc gt 0 then amaps_bdiff[cc].data=amaps[cc].data-abckg_map.data
        if cc gt 0 then amaps_bdiff[cc]=diff_map(tmpmap,abckg_map)
        cc++
     endfor
     namaps=cc
     amaps=amaps[0:cc-1]
     amaps_bdiff=amaps_bdiff[0:cc-1]
     amaps_fnames=amaps_fnames[0:cc-1]
     save,filename=path+event.label+'_'+event.date+'_sta_maps.sav',$
          amaps,amaps_bdiff,abckg_map,amaps_fnames,namaps,wav,event
  endif else begin
     restore,path+event.label+'_'+event.date+'_sta_maps.sav'
  endelse
  
  if keyword_set(png) then begin
     wdef,0,1000
     for i=1,namaps-1 do begin
        plot_map,amaps_bdiff[i],dmin=-20,dmax=20,/limb,/smooth
        tmp=strsplit(amaps_fnames[i],'_',/extract)
        fname=event.label+'_'+event.date+'_'+tmp[1]+'_STA_bdiff.png'
        write_png,path+fname,tvrd(/true)
     endfor
  endif
;------------------------------------------------------------


;------------------------------------------------------------
;Load the B-maps
  bmaps_fnames=file_basename(file_search(path+date+'*n4euB.fts'))
  nbmaps=n_elements(bmaps_fnames)
  if not keyword_set(saved) then begin
     bbckg_map=mk_secchi_map(path+bmaps_fnames[0],/rotate_on)
     bmaps=replicate(bbckg_map,nbmaps)
     bmaps_bdiff=bmaps
     cc=0
     for i=0,n_elements(bmaps_fnames)-1 do begin
        tmpmap=mk_secchi_map(path+bmaps_fnames[i],/rotate_on)
        if where(strsplit(tmpmap.id,' ',/extract) eq wav) eq -1 then continue
        bmaps[cc]=tmpmap
        bmaps_fnames[cc]=bmaps_fnames[i]
      ; if cc gt 0 then bmaps_bdiff[i].data=bmaps[i].data-bbckg_map.data
        if cc gt 0 then bmaps_bdiff[cc]=diff_map(tmpmap,bbckg_map)
        cc++
     endfor
     nbmaps=cc
     bmaps=bmaps[0:cc-1]
     bmaps_bdiff=bmaps_bdiff[0:cc-1]
     bmaps_fnames=bmaps_fnames[0:cc-1]
     save,filename=path+event.label+'_'+event.date+'_stb_maps.sav',$
          bmaps,bmaps_bdiff,bbckg_map,bmaps_fnames,nbmaps,wav,event
  endif else begin
     restore,path+event.label+'_'+event.date+'_stb_maps.sav'
  endelse

  if keyword_set(png) then begin  
     wdef,0,1000
     for i=1,nbmaps-1 do begin
        plot_map,bmaps_bdiff[i],dmin=-20,dmax=20,/limb,/smooth
        tmp=strsplit(bmaps_fnames[i],'_',/extract)
        fname=event.label+'_'+event.date+'_'+tmp[1]+'_STB_bdiff.png'
        write_png,path+fname,tvrd(/true)
     endfor
  endif
;------------------------------------------------------------
  
  
  
  
;------------------------------------------------------------
  ;Create the PFSS models and maps
  ;STEREO
  
  @pfss_data_block
  pfssfile=event.pfsspath+'pfss_results_'+date+'_'+label+'_1.05Rs_dens_8.0.sav'
  restore,pfssfile

  ;DEBUG - THESE MUST BE SPECIFIED AUTOMATICALLY!!!
  lcent=128.50276
  bcent=7.2161878
  ;DEBUG
  
  width=1.6
  mag=16
  pfss_draw_field,outim=pfssim_sta,bcent=bcent,lcent=lcent,width=width,mag=mag
  nax=size(pfssim_sta,/dim)
  pfssmap_sta={data:pfssim_sta, $
               xc:0.0, $
               yc:0.0, $
               dx:2./nax[0], $
               dy:2./nax[1], $
               time:now, $
               units:'normalized', $
               pfss_cent_l0:lcent, $
               pfss_cent_b0:bcent, $
               id:'PFSS field line map', $
               roll_angle:0.0, $
               roll_center:[0.0,0.0]}
  save,filename=event.date+'_'+label+'_pfss_map_sta.sav',pfssim_sta,pfssmap_sta
  
  rsunarc_stereo=amaps[0].rsun
  pfssmap_sta.dx*=amaps[0].rsun*width
  pfssmap_sta.dy*=amaps[0].rsun*width
  
  loadct,9,/silent
  tvlct,rr,gg,bb,/get
  plot_map,amaps_bdiff[2],dmin=-20,dmax=20,/limb,/smooth
  loadct,6,/silent
  plot_map,pfssmap_sta,/over,/rotate,cthick=2
  loadct,0,/silent
  stop
;------------------------------------------------------------


;------------------------------------------------------------
;Create the PFSS maps


;------------------------------------------------------------
  


  
  
end
