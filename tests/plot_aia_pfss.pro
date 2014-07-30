pro test_plot_aia_pfss
  ;Test run the AIA/PFSS overplotting routine
  event=load_events_info(label='110511_01')
  plot_aia_pfss,event;,/saved
end
 

pro plot_aia_pfss,event,saved=saved,png=png,wav=wav,diff=diff,sta=sta,stb=stb
  ;Overplot PFSS models on AIA and STEREO
  if not keyword_set(wav) then wav='171'
   
;------------------------------------------------------------

;Load the AIA-maps
  if not keyword_set(saved) then begin
     aia_load_data,event.st,aia_augment_timestring(event.st,15),wav,index,map=aiamaps
     naiamaps=n_elements(aiamaps)
  aiamaps_latlon=replicate({crln:0.D,crlt:0.D},naiamaps)
     for i=0,naiamaps-1 do begin
        ;Get the Carrington Lat/Lon of the observation
        tmpwcs=fitshead2wcs(index[0])
        aiamaps_latlon[i].crln=tmpwcs.position.crln_obs
        aiamaps_latlon[i].crlt=tmpwcs.position.crlt_obs
     endfor
     save,filename=event.pfsspath+event.date+'_'+event.label+'_'+wav+'_aia_maps.sav',$
          aiamaps,aiamaps_latlon,naiamaps,wav,event
  endif else begin
     restore,event.pfsspath+event.date+'_'+event.label+'_'+wav+'_aia_maps.sav'
  endelse

  
;------------------------------------------------------------
  ;Create the PFSS models and maps
  
  @pfss_data_block
  width=1.7
  mag=16
  
  dsun_pfss=192.0               ; in px, from detailed notes in PFSS_VIEWER
  rsunarc_aia=aiamaps[0].rsun           ;solar radius in arcseconds
  width=1.6
  mag=rsunarc_aia*2.0/dsun_pfss
 
;===-------------------------------------------===
  realevent=event
     if not keyword_set(saved) then begin
        pfssfile=event.pfsspath+'pfss_results_'+event.date+'_'+event.label+'_lores.sav'
        restore,pfssfile
        event=realevent
;CREATE/LOAD MODEL FOR AIA
        lcent=aiamaps_latlon[0].crln ;128.50276
        bcent=aiamaps_latlon[0].crlt ;7.2161878
        pfss_draw_field,outim=pfssim_aia,bcent=bcent,lcent=lcent,width=width,mag=mag
        nax=size(pfssim_aia,/dim)
        pfssmap_aia={data:pfssim_aia, $
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
        save,filename=event.pfsspath+event.date+'_'+event.label+'_pfss_map_aia.sav',pfssim_aia,pfssmap_aia
        
  ;===-------------------------------------------===
     endif else begin
        restore,event.pfsspath+event.date+'_'+event.label+'_pfss_map_aia.sav'
     endelse
     
     
     ;MAKE A PLOT OF THE PFSS-overlaid data
     pfssmap_aia.dx*=aiamaps[0].rsun*width
     pfssmap_aia.dy*=aiamaps[0].rsun*width
     
     wdef,0,1200

     ;Create the time string for the filename
     tmp=index[0].date_obs
     tmp=strsplit(tmp,'T:.',/extract)
     timstring=tmp[1]+tmp[2]+tmp[3]
     loadct,9,/silent
     tvlct,rr,gg,bb,/get
     plot_map,aiamaps[0],/limb,/smooth,/log,dmin=0.5
     loadct,6,/silent
     plot_map,pfssmap_aia,/over,/rotate,cthick=0.5
     loadct,0,/silent
     fname=event.label+'_'+event.date+'_'+timstring+'_'+wav+'_AIA_PFSS_overlay_full.png'
     write_png,event.pfsspath+fname,tvrd(/true),rr,gg,bb
     
     
;Make the submaps
   xr=[-400,1600]
   yr=[-500,1500]
     wdef,1,1200
     ;sub_map,aiamaps_bdiff[2],smap_sta,xrange=xr,yrange=yr
     sub_map,aiamaps[0],smap_aia,xrange=xr,yrange=yr
     sub_map,pfssmap_aia,spfssmap_aia,xrange=xr,yrange=yr
     loadct,9,/silent
     tvlct,rr,gg,bb,/get
     plot_map,smap_aia,/limb,/smooth,/log,dmin=0.5
     loadct,6,/silent
     plot_map,spfssmap_aia,/over,/rotate,cthick=0.5
     loadct,0,/silent
     fname=event.label+'_'+event.date+'_'+timstring+'_'+wav+'_AIA_PFSS_overlay_partial.png'
     write_png,event.pfsspath+fname,tvrd(/true),rr,gg,bb
  
end
