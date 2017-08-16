pro test_aia_aschdem_plot_global_change
;Test procedure for aia_cfa_teem_plot_em_ratios
  one=0
  if one eq 1 then begin
     labels=['151104_01','151104_02','151104_03']
     labels=['151104_01','100613_01']
     labels=['110607_01'] ;131212_01
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        ;aia_aschdem_plot_global_change,event
        ;stop
;aia_aschdem_plot_global_change,event,/temperature
        aia_aschdem_plot_global_change,event,/density
        aia_aschdem_plot_global_change,event,/temperature
        aia_aschdem_plot_global_change,event,/density,/bratio   ;,/smooth
        aia_aschdem_plot_global_change,event,/temperature,/diff;,/smooth
        ;aia_aschdem_plot_global_change,event,/density,/smooth
        ;aia_aschdem_plot_global_change,event,/temperature,/bdiff
        
        ;movie_types=['teem_temperature','teem_density','teem','density_ratios','temperature_differences']
        ;for mt=0,n_elements(movie_types)-1 do begin 
        ;   movie_type=movie_types[mt]
        ;   aia_make_movies, event, movie_type=movie_type,/force
        ;endfor
     endfor
  endif
  
  all=1
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        if event.web eq 0 then continue
	aia_aschdem_plot_global_change,event,/density,/bratio
	aia_aschdem_plot_global_change,event,/temperature,/diff
        movie_types=['density_ratios','temperature_differences']
	aia_make_movies, event, movie_type=movie_types
     endfor
  endif 
  
end


pro aia_aschdem_plot_global_change,event,density=density,temperature=temperature,bratio=bratio,bdiff=bdiff,diff=diff,force=force,smooth=smooth
;+
; Project     : AIA/SDO
;
; Name        : AIA_ASCHDEM_PLOT_GLOBAL_CHANGE
;
; Category    : Display global DEM-based change (average density,
;               average temperature, emission measure),
;		previously calculated with AIA_CFA_TEEM_RUN.PRO
;
; Explanation : Based on aia_cfa_teem_disp.pro
;
; Syntax      : IDL>aia_aschdem_plot_global_change,event
;
; Inputs      :
;
; Outputs     : png-file
;
; History     :  01-Mar-2016 - Kamen Kozarev - based on aia_cfa_teem_disp.pro
;
; Contact     : kkozarev@cfa.harvard.edu
;-

;Find the files first
  path=event.aschdempath
  fileset=file_basename(file_search(path+'aschdem_'+event.date+'_'+event.label+'*teem_tot.sav'))
  if n_elements(fileset) lt 5 then begin
	print, 'Not enough files to make plots for event '+event.label
	return
  endif
  basefname=path+fileset[0:4]
  nfiles=n_elements(fileset)
  for ff=0,nfiles-1 do begin
     res=strsplit(fileset[ff],'_',/extract)
     if n_elements(res) eq 6 then timeobs=res[3] else timeobs=res[4]
     infname=path+fileset[ff] ;'aschdem_'+event.date+'_'+event.label+'_'+timeobs+'_teem_map.sav'
     aia_aschdem_plot_global_change_main,event,infname,outfname,basefname,timeobs,density=density,temperature=temperature,bratio=bratio,bdiff=bdiff,diff=diff,force=force,smooth=smooth
     
  endfor
  
end




pro aia_aschdem_plot_global_change_main,event,infname,outfname,basefname,timeobs,density=density,temperature=temperature,bratio=bratio,bdiff=bdiff,diff=diff,force=force,smooth=smooth
;________________________DISPLAY EM-BASED GLOBAL CHANGE________________________
  set_plot,'z'
  path=event.aschdempath
  
  time=strmid(timeobs,0,2)+':'+strmid(timeobs,2,2)+':'+strmid(timeobs,4,2)
  date=strmid(event.date,0,4)+'/'+strmid(event.date,4,2)+'/'+strmid(event.date,6,2)
  res=strsplit(file_basename(basefname[0]),'_',/extract)
  if n_elements(res) eq 6 then basetime=res[3] else basetime=res[4]
  basetime=strmid(basetime,0,2)+':'+strmid(basetime,2,2)+':'+strmid(basetime,4,2)
  cbarposition=[0.84, 0.03, 0.865, 0.97]

  ;Load some information about the data:
  if file_exist(event.mfhcpath+event.csgs.lores.map_savename) then begin
     restore,event.mfhcpath+event.csgs.lores.map_savename
     ind = subindex[0]
     wcs = fitshead2wcs(ind)
     coords = wcs_get_coord(wcs)/ind.rsun_obs ;Cartesian coordinates of the pixels in solar radii.
     radcoords=reform(sqrt(coords[0,*,*]^2 + coords[1,*,*]^2))
  endif

  basemap=1
  inmap=1
  if keyword_set(density) then begin
     if var_exist(radcoords) then $
        aia_aschdem_calculate_em,event,basefname[0],em_map,radcoords=radcoords,density=basemap $
     else aia_aschdem_calculate_em,event,basefname[0],em_map,density=basemap
  endif else begin
     if keyword_set(temperature) then begin
        if var_exist(radcoords) then $
           aia_aschdem_calculate_em,event,basefname[0],em_map,radcoords=radcoords,temperature=basemap $
        else aia_aschdem_calculate_em,event,basefname[0],em_map,temperature=basemap
     endif else begin
        if var_exist(radcoords) then $
           aia_aschdem_calculate_em,event,basefname[0],basemap $
        else aia_aschdem_calculate_em,event,basefname[0],basemap,radcoords=radcoords
     endelse
  endelse
  basemap=fix_bad_pixels(basemap)
  
  if keyword_set(density) then begin
     ;Plot the change in density
     if var_exist(radcoords) then $
        aia_aschdem_calculate_em,event,infname,em_map,radcoords=radcoords,density=inmap $
     else aia_aschdem_calculate_em,event,infname,em_map,density=inmap
     if keyword_set(bratio) then begin
        plotim=inmap/(1.*basemap+1.e-30)
        dmin= 1.0                     
        dmax= 1.35
        plot_title='AIA <N> BASE RATIO'
        legend_title='AVG. DENSITY BASE RATIO'+' REL. TO '+basetime
        cbarformat='(f5.2)'
        outfname=path+replace_string(event.aschdem.plot.density_ratios_savename,'HHMMSS',timeobs)
     endif else begin
        if keyword_set(bdiff) then begin
           bdiff = inmap - basemap
           badd = inmap + basemap
           plotim = bdiff / badd
           dmin= 0.0                     
           dmax= 1.0                     
           plot_title='AIA <N> NORM. BDIFF'
           legend_title='AVG. DENSITY NORM. BDIFF.'+' REL. TO '+basetime
           cbarformat='(f4.2)'
           outfname=path+replace_string(event.aschdem.plot.density_differences_savename,'HHMMSS',timeobs)
        endif else begin
           plotim = fix_bad_pixels(inmap+1.e-30)
           dmin= 3.e7         ;3.e7           
           dmax= 9.e8         ;9.e8    
           plot_title='AIA <N>'
           legend_title='AVERAGE DENSITY [cm!U-3!N]'
           cbarformat='(e8.1)'
           cbarposition=[0.805, 0.03, 0.82, 0.97]
           outfname=path+replace_string(event.aschdem.plot.density_savename,'HHMMSS',timeobs)
        endelse
     endelse
  endif else begin
     if keyword_set(temperature) then begin
       ;Plot the change in temperature
        if var_exist(radcoords) then $
           aia_aschdem_calculate_em,event,infname,em_map,radcoords=radcoords,temperature=inmap $
        else aia_aschdem_calculate_em,event,infname,em_map,temperature=inmap
        if keyword_set(bratio) then begin
           plotim=inmap/(1.*basemap+1.e-30)
           dmin= 1.0
           dmax= 1.6
           plot_title='AIA <T> BASE RATIO'
           legend_title='AVG. TEMPERATURE BASE RATIO'+' REL. TO '+basetime
           cbarformat='(f4.2)'
           outfname=path+replace_string(event.aschdem.plot.temperature_ratios_savename,'HHMMSS',timeobs)
        endif else begin
           if keyword_set(bdiff) then begin
              bdiff = inmap - basemap
              badd = basemap; inmap + basemap
              plotim = bdiff / badd
              dmin= 0.1                     
              dmax= 0.3                
              plot_title='AIA <T> NORM. BDIFF'
              legend_title='AVG. TEMPERATURE NORM. BDIFF.'+' REL. TO '+basetime
              cbarformat='(f4.2)'
              outfname=path+replace_string(event.aschdem.plot.temperature_differences_savename,'HHMMSS',timeobs)
           endif else begin
              if keyword_set(diff) then begin
                 plotim = alog10(inmap - basemap)
                 dmin= 4.9            
                 dmax= 5.8         
                 plot_title='AIA <T> BASE DIFF.'
                 legend_title='LOG(AVG. TEMPERATURE BASE DIFFERENCE)'+' REL. TO '+basetime
                 cbarformat='(f5.2)'
                 outfname=path+replace_string(event.aschdem.plot.temperature_differences_savename,'HHMMSS',timeobs)
              endif else begin
                 plotim = alog10(fix_bad_pixels(inmap+1.e-30))
                 dmin= alog10(8.e5)
                 dmax= alog10(8.e6)
                 plot_title='AIA <T>'
                 legend_title='LOG(AVERAGE TEMPERATURE [K])'
                 cbarformat='(f8.2)'
                 outfname=path+replace_string(event.aschdem.plot.temperature_savename,'HHMMSS',timeobs)
              endelse
           endelse
        endelse
     endif else begin
        ;Plot the change in EM
        if var_exist(radcoords) then $
           aia_aschdem_calculate_em,event,infname,inmap,radcoords=radcoords $
        else aia_aschdem_calculate_em,event,infname,inmap
        if keyword_set(bratio) then begin
           plotim=inmap/(1.*basemap)
           dmin= 0.0                     
           dmax= 2.0
           plot_title='AIA EM BASE RATIO'
           legend_title='EM BASE RATIO'+' REL. TO '+basetime
           cbarformat='(f4.2)'
           outfname=path+replace_string(event.aschdem.plot.em_ratios_savename,'HHMMSS',timeobs)
        endif else begin
           ;bdiff = inmap - basemap
           ;badd = inmap + basemap
           ;plotim = bdiff / badd
           plotim=fix_bad_pixels(alog10(inmap))
           
           dmin= 25.5                     
           dmax= 30.           
           plot_title='AIA EMISSION MEASURE'
           legend_title='LOG(EMISSION MEASURE [cm!U-5!N])'
           cbarformat='(f6.2)'
           outfname=path+replace_string(event.aschdem.plot.em_differences_savename,'HHMMSS',timeobs)
        endelse
     endelse
  endelse
  plotim=fix_bad_pixels(plotim)
  if keyword_set(smooth) then plotim=smooth(plotim,2,/edge_truncate)

  
  dim	=size(inmap)
  nx	=dim(1)
  ny	=dim(2)
  ;Make sure the dimensions are divisible by 2, so ffmpeg doesn't complain.
  imres=floor([nx*1.4,ny])
  while imres[0] mod 2 ne 0 do begin
     imres[0]++
  endwhile
  while imres[1] mod 2 ne 0 do begin
     imres[1]++
  endwhile
  
  ;while imres[0] mod no
  device, set_resolution=imres, SET_PIXEL_DEPTH=24, DECOMPOSED=0
  ct=13
  ;if keyword_set(temperature) then ct=15
  
  !p.font=0
  tvlct,rr,gg,bb,/get 
  loadct,0,/silent
  !p.position=[0,0,1,1]
  !x.range=[0,2.*nx]
  !y.range=[0,ny]
  !x.style=1
  !y.style=1
  !p.background=255
  plot,[0,0],[0,0],xticks=1,yticks=1,xminor=1,yminor=1
  loadct,ct,/silent
 
  text_scaling=event.aiafov[1]/1024.0
  xscaling=event.aiafov[0]/1024.0
  chsize=1.2*text_scaling
  chthick=1.2*text_scaling
 
  
  scimage=(bytscl(plotim,min=dmin,max=dmax)+0)
  cbartitle=legend_title
  if keyword_set(bdiff) then begin
     scimage*=100.
     dmin*=100.
     dmax*=100.
     cbarformat='(i4)'
     cbartitle+=', %'
  endif
  tvscl, scimage
  
  
  fcolorbar, MIN=dmin,MAX=dmax,Divisions=8, $
             Color=0,VERTICAL=1,RIGHT=1, TITLE=cbartitle,$
             CHARSIZE=2*text_scaling,charthick=1.6*text_scaling,format=cbarformat,Position=cbarposition
  
  loadct,0,/silent
  ;polyfill,[0.0,0.64/xscaling,0.64/xscaling,0.0],[0.98,0.98,1.0,1.0],color=0,/norm
  ;xyouts,0.005,0.985,date+' '+time+'  '+plot_title,charsize=chsize,charthick=chthick,color=255,/norm
  

;PNG file
  image_tv=tvrd(true=1)
  write_png,outfname,image_tv,rr,gg,bb
  print,'file written : ',outfname
  
  set_plot,'x'

  ;stop
end



