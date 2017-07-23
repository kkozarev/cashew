pro test_plot_stereo_pfss
  ;Test run the STEREO/PFSS overplotting routine
  event=load_events_info(label='test')
  plot_stereo_pfss,event;,/saved
end


pro plot_stereo_pfss,event,saved=saved,png=png,wav=wav,diff=diff,sta=sta,stb=stb
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
  
  if not keyword_set(sta) and not keyword_set(stb) then begin
     sta=1
     stb=1
  endif
   
;------------------------------------------------------------
if keyword_set(sta) then begin
;Load the A-maps
  amaps_fnames=file_basename(file_search(event.euvi_datapath+wav+'/'+event.date+'*n4euA.fts'))
  namaps=n_elements(amaps_fnames)
  if namaps eq 1 and amaps_fnames[0] eq '' then begin
     print,'No STEREO-A/EUVI data for this event. Quitting...'
     return
  endif
  if not keyword_set(saved) then begin
     abckg_map=mk_secchi_map(event.euvi_datapath+wav+'/'+amaps_fnames[0],/rotate_on)
     amaps=replicate(abckg_map,namaps)
     amaps_bdiff=replicate(abckg_map,namaps)
     amaps_latlon=replicate({crln:0.D,crlt:0.D},namaps)
     cc=0
     for i=0,n_elements(amaps_fnames)-1 do begin
        tmpmap=mk_secchi_map(event.euvi_datapath+wav+'/'+amaps_fnames[i],/rotate_on)
        if where(strsplit(tmpmap.id,' ',/extract) eq wav) eq -1 then continue
        amaps[cc]=tmpmap
        amaps_fnames[cc]=amaps_fnames[i]
        
        ;Make the difference map
        if namaps gt 1 and keyword_set(diff) then begin
           tmp2=diff_map(tmpmap,abckg_map)
           if cc eq 0 then amaps_bdiff=replicate(tmp2,namaps)
           amaps_bdiff[cc]=tmp2
        endif
        
        ;Get the Carrington Lat/Lon of the observation
        tmpdata=sccreadfits(event.euvi_datapath+wav+'/'+amaps_fnames[i],tmpindex,/nodata)
        tmpwcs=fitshead2wcs(tmpindex)
        amaps_latlon[cc].crln=tmpwcs.position.crln_obs
        amaps_latlon[cc].crlt=tmpwcs.position.crlt_obs

        cc++
     endfor
     namaps=cc
     amaps=amaps[0:cc-1]
     
     if namaps gt 1 and keyword_set(diff) then amaps_bdiff=amaps_bdiff[0:cc-1]
     amaps_fnames=amaps_fnames[0:cc-1]
     amaps_latlon=amaps_latlon[0:cc-1]
     save,filename=event.euvipath+event.label+'_'+event.date+'_'+wav+'_sta_maps.sav',$
          amaps,amaps_bdiff,abckg_map,amaps_fnames,amaps_latlon,namaps,wav,event
  endif else begin
     restore,event.euvipath+event.label+'_'+event.date+'_'+wav+'_sta_maps.sav'
  endelse
  
  if keyword_set(png) then begin
     wdef,0,1000
     for i=0,namaps-1 do begin
        if keyword_set(diff) then begin
           if namaps gt 1 then plot_map,amaps_bdiff[i],dmin=-20,dmax=20,/limb,/smooth
           endif else begin
              plot_map,amaps[i],/limb,/smooth,/log
              
           endelse
        tmp=strsplit(amaps_fnames[i],'_',/extract)
        fname=event.label+'_'+event.date+'_'+tmp[1]+'_'+wav+'_STA_bdiff.png'
        write_png,event.euvipath+fname,tvrd(/true)
     endfor
     
  endif
endif
;------------------------------------------------------------


;------------------------------------------------------------
if keyword_set(stb) then begin
;Load the B-maps
  bmaps_fnames=file_basename(file_search(event.euvi_datapath+wav+'/'+event.date+'*n4euB.fts'))
  nbmaps=n_elements(bmaps_fnames)
  if nbmaps eq 1 and bmaps_fnames[0] eq '' then begin
     print,'No STEREO-B/EUVI data for this event. Quitting...'
     return
  endif

  if not keyword_set(saved) then begin
     bbckg_map=mk_secchi_map(event.euvi_datapath+wav+'/'+bmaps_fnames[0],/rotate_on)
     bmaps=replicate(bbckg_map,nbmaps)
     bmaps_bdiff=replicate(bbckg_map,nbmaps)
     bmaps_latlon=replicate({crln:0.D,crlt:0.D},nbmaps)
     cc=0
     for i=0,n_elements(bmaps_fnames)-1 do begin
        tmpmap=mk_secchi_map(event.euvi_datapath+wav+'/'+bmaps_fnames[i],/rotate_on)
        if where(strsplit(tmpmap.id,' ',/extract) eq wav) eq -1 then continue
        bmaps[cc]=tmpmap
        bmaps_fnames[cc]=bmaps_fnames[i]
        
        ;Make the difference map
        if nbmaps gt 1 and keyword_set(diff) then begin
           tmp2=diff_map(tmpmap,bbckg_map)
           if cc eq 0 then bmaps_bdiff=replicate(tmp2,nbmaps)
           bmaps_bdiff[cc]=tmp2
        endif
        
        ;Get the Carrington Lat/Lon of the observation
        tmpdata=sccreadfits(event.euvi_datapath+wav+'/'+bmaps_fnames[i],tmpindex,/nodata)
        tmpwcs=fitshead2wcs(tmpindex)
        bmaps_latlon[cc].crln=tmpwcs.position.crln_obs
        bmaps_latlon[cc].crlt=tmpwcs.position.crlt_obs
        
        cc++
     endfor
     nbmaps=cc
     bmaps=bmaps[0:cc-1]
     if nbmaps gt 1 and keyword_set(diff) then bmaps_bdiff=bmaps_bdiff[0:cc-1]
     bmaps_fnames=bmaps_fnames[0:cc-1]
     bmaps_latlon=bmaps_latlon[0:cc-1]
     save,filename=event.euvipath+event.label+'_'+event.date+'_'+wav+'_stb_maps.sav',$
          bmaps,bmaps_bdiff,bbckg_map,bmaps_fnames,bmaps_latlon,nbmaps,wav,event
  endif else begin
     restore,path+event.label+'_'+event.date+'_'+wav+'_stb_maps.sav'
  endelse
  
  if keyword_set(png) then begin  
     wdef,0,1000
     for i=0,nbmaps-1 do begin
        if nbmaps gt 1 and keyword_set(diff) then plot_map,bmaps_bdiff[i],dmin=-20,dmax=20,/limb,/smooth $
           else plot_map,bmaps[i],/limb,/smooth,/log
        tmp=strsplit(bmaps_fnames[i],'_',/extract)
        fname=event.label+'_'+event.date+'_'+tmp[1]+'_'+wav+'_STB_bdiff.png'
        write_png,event.euvipath+fname,tvrd(/true)
     endfor
  endif
endif
;------------------------------------------------------------
  
  
  
  
;------------------------------------------------------------
  ;Create the PFSS models and maps
  ;STEREO
  
  @pfss_data_block
  width=1.7
  mag=16
;===-------------------------------------------===
  if keyword_set(sta) then begin
     if not keyword_set(saved) then begin
        pfssfile=event.mfhcpath+'pfss_results_'+event.date+'_'+event.label+'_lores.sav'
        restore,pfssfile
;CREATE/LOAD MODEL FOR STEREO-A
        lcent=amaps_latlon[0].crln ;128.50276
        bcent=amaps_latlon[0].crlt ;7.2161878
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
        save,filename=event.euvipath+event.date+'_'+label+'_pfss_map_sta.sav',pfssim_sta,pfssmap_sta
        
                                ;===-------------------------------------------===
     endif else begin
        restore,event.euvipath+event.date+'_'+label+'_pfss_map_sta.sav'
     endelse
     
     
     ;MAKE A PLOT OF THE PFSS-overlaid data
     rsunarc_stereo=amaps[0].rsun
     pfssmap_sta.dx*=amaps[0].rsun*width
     pfssmap_sta.dy*=amaps[0].rsun*width
     
     wdef,0,1200
     loadct,9,/silent
     tvlct,rr,gg,bb,/get
     ;  plot_map,amaps_bdiff[2],dmin=-20,dmax=20,/limb,/smooth
     plot_map,amaps[0],/limb,/smooth,/log,dmin=1
     loadct,6,/silent
     plot_map,pfssmap_sta,/over,/rotate,cthick=2
     loadct,0,/silent
     tmp=strsplit(amaps_fnames[0],'_',/extract)
     fname=event.label+'_'+event.date+'_'+tmp[1]+'_'+wav+'_STA_PFSS_overlay_full.png'
     write_png,event.euvipath+fname,tvrd(/true),rr,gg,bb
     
;Make the submaps
     xr=[-1600,400]
     yr=[-500,1500]
     wdef,1,1200
     ;sub_map,amaps_bdiff[2],smap_sta,xrange=xr,yrange=yr
     sub_map,amaps[0],smap_sta,xrange=xr,yrange=yr
     sub_map,pfssmap_sta,spfssmap_sta,xrange=xr,yrange=yr
     loadct,9,/silent
     tvlct,rr,gg,bb,/get
     ;plot_map,smap_sta,dmin=-20,dmax=20,/limb,/smooth
     plot_map,smap_sta,/limb,/smooth,/log,dmin=1
     loadct,6,/silent
     plot_map,spfssmap_sta,/over,/rotate,cthick=2
     loadct,0,/silent
     tmp=strsplit(amaps_fnames[0],'_',/extract)
     fname=event.label+'_'+event.date+'_'+tmp[1]+'_'+wav+'_STA_PFSS_overlay_partial.png'
     write_png,event.euvipath+fname,tvrd(/true),rr,gg,bb
  endif
  
  
if keyword_set(stb) then begin
   if not keyword_set(saved) then begin
;===-------------------------------------------===
      pfssfile=event.mfhcpath+'pfss_results_'+event.date+'_'+event.label+'_lores.sav'
      restore,pfssfile
;CREATE/LOAD MODEL FOR STEREO-B
      lcent=bmaps_latlon[0].crln ;128.50276
      bcent=bmaps_latlon[0].crlt ;7.2161878
      pfss_draw_field,outim=pfssim_stb,bcent=bcent,lcent=lcent,width=width,mag=mag
      nax=size(pfssim_stb,/dim)
      pfssmap_stb={data:pfssim_stb, $
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
      save,filename=event.euvipath+event.date+'_'+label+'_pfss_map_stb.sav',pfssim_stb,pfssmap_stb
                                ;===-------------------------------------------===
   endif else begin
      restore,event.euvipath+event.date+'_'+label+'_pfss_map_stb.sav'
   endelse
   
   
  ;MAKE A PLOT OF THE STEREO AND PFSS TOGETHER
   rsunarc_stereo=bmaps[0].rsun
   pfssmap_stb.dx*=bmaps[0].rsun*width
   pfssmap_stb.dy*=bmaps[0].rsun*width
   
   wdef,0,1200
   loadct,9,/silent
   tvlct,rr,gg,bb,/get
   ;plot_map,amaps_bdiff[1],dmin=-20,dmax=20,/limb,/smooth
   plot_map,bmaps[0],/limb,/smooth,/log,dmin=1
   loadct,6,/silent
   plot_map,pfssmap_sta,/over,/rotate,cthick=2
   loadct,0,/silent
   tmp=strsplit(bmaps_fnames[0],'_',/extract)
   fname=event.label+'_'+event.date+'_'+tmp[1]+'_'+wav+'_STB_PFSS_overlay_full.png'
   write_png,event.euvipath+fname,tvrd(/true),rr,gg,bb

   
;Make the submaps
   xr=[-400,1600]
   yr=[-500,1500]
   wdef,1,1200
   ;sub_map,amaps_bdiff[1],smap_sta,xrange=xr,yrange=yr
   sub_map,bmaps[0],smap_stb,xrange=xr,yrange=yr
   sub_map,pfssmap_stb,spfssmap_stb,xrange=xr,yrange=yr
   loadct,9,/silent
   tvlct,rr,gg,bb,/get
   ;plot_map,bmaps_bdiff[1],dmin=-20,dmax=20,/limb,/smooth
   plot_map,smap_stb,/limb,/smooth,/log,dmin=1
   loadct,6,/silent
   plot_map,pfssmap_stb,/over,/rotate,cthick=2
   loadct,0,/silent
   tmp=strsplit(bmaps_fnames[0],'_',/extract)
   fname=event.label+'_'+event.date+'_'+tmp[1]+'_'+wav+'_STB_PFSS_overlay_partial.png'
   write_png,event.euvipath+fname,tvrd(/true),rr,gg,bb
   
endif
;------------------------------------------------------------


;------------------------------------------------------------
;Create the PFSS maps


;------------------------------------------------------------
  


  
  
end
