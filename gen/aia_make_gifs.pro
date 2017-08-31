pro test_aia_make_gifs
;Test make_gifs procedure

;You can run this for a few events, like so
  one=1
  if one eq 1 then begin
     labels=['140708_01','131212_01','130517_01','130423_01','120915_01','120526_01',$
             '120424_01','110607_01','110211_02','110125_01','130423_01','140708_01']
     wavelengths=['193','211']
     product_types=['araw','abase','arun','raw','base','run']
     for ev=0,n_elements(labels)-1 do begin 
        label=labels[ev]
        event=load_events_info(label=label)
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           for pt=0,n_elements(product_types)-1 do begin 
              product_type=product_types[mt]
              aia_make_gifs, event, product_type=product_type,/force, wav=wavelength
           endfor
        endfor 
     endfor
  endif
  
  ;Alternatively, run it for all/multiple events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     product_types=['araw','abase','arun','raw','base','run']
     nevents=n_elements(events)
     for ev=0,nevents-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           for pt=0,n_elements(product_types)-1 do begin
              product_type=product_types[mt]
              aia_make_gifs, event, product_type=product_type, wav=wavelength,/force
           endfor
        endfor
     endfor
  endif
end



pro aia_make_gifs,event,path=path,name=name,force=force,product_type=product_type,wav=wav
;PURPOSE:
; This procedure will return an animated gif file from a folder full of
; png files.
;
;INPUTS:
;	EVENT - the event structure returned by load_events_info()
;
;OUTPUTS:
; 
;DEPENDENCIES:
;
;KEYWORDS:
;       PRODUCT_TYPE:
;            'raw', 'base', 'run' - raw or base/running difference from
;                             original data
;            'araw', 'abase', 'arun' - raw or base/running difference from
;                                the deprojected data
;            'teem' - The Aschwanden DEM maps.
;            'teem_density' - Plot the average density,
;                               based on the Aschwanden model EM.
;            'teem_temperature' - Plot the average temperature,
;                               based on the Aschwanden model EM.
;            'em_ratios' - Plot a base ratio of the Aschwanden
;                             model EM.
;            'em_differences' - Plot a normalized base difference of the Aschwanden
;                             model EM.
;            'density_ratios' - Plot a base ratio of average density,
;                               based on the Aschwanden model EM.
;            'density_differences' - Plot a normalized base difference of average temperature,
;                               based on the Aschwanden model EM.
;            'temperature_ratios' - Plot a base ratio of average density,
;                               based on the Aschwanden model EM.
;            'temperature_differences' - Plot a normalized base difference of average temperature,
;                               based on the Aschwanden model EM.
;            'pfss_shock' - the PFSS/Shock model images
;            'pfss_shock_hires' - same as 'pfss_shock', but with high PFSS resolution
;            'thetabn' - the plots of ThetaBN angle as a
;                         function of the shock surface position
;            'thetabn_hires' - same as 'thetabn' but with high PFSS resolution
;            'thetabn_oplot' - the plots of cumulative ThetaBN angle as a
;                         function of the shock surface position
;            'thetabn_oplot_hires' - same as thetabn_oplot but with
;                                    high PFSS resolution
;            'pfss_spread' - the plots of PFSS interacting field lines
;                            showing the angular spread of shock
;                            influence
;            'pfss_spread_hires' - same as pfss_spread but with high
;                                  PFSS resolution
;            'pfss_spread_topview' - the plots of PFSS interacting field lines
;                            showing the angular spread of shock
;                            influence
;            'pfss_spread_topview_hires' - same as pfss_spread_topview but with high
;                                  PFSS resolution
;            'ywave' - the plots of YAFTA/Wave tracking results
;	WAV: wavelength of the AIA channel, 
;              string - 94,131,171,193,211,304,335
;            Default is '193'
;       FORCE: force the program to overwrite movies even if they exist
;       PATH: If the path is not an expected path, include it as a keyword 
;
  ; Determine Path
  if not keyword_set(path) then path=event.savepath
  gif_path=path
  if not keyword_set(wav) then wavelength = '193' else wavelength=wav
  if not keyword_set(product_type) then product_type = 'raw'
  

; Determine step for images to include.
  step = 4
  


;Prepare the date string
  date=event.date
  label=event.label
  filetype = product_type + '_'

  savepath=path+'png/'
  gif_path=event.gifpath
  pngfname='normalized_AIA_'+date + '_' + event.label + '_' + wavelength + "_subdata_" + product_type
;The png image files
  imgfnames = savepath + product_type + '/' + wavelength + '/' + pngfname + '_%03d.png'
  imgsearch = savepath + product_type + '/' + wavelength + '/' + pngfname + '_*.png'
  gifname = gif_path + product_type + '_' + wavelength + '_' + label + '.gif'
  
  if product_type eq 'araw' or product_type eq 'arun' or product_type eq 'abase' then begin
     savepath=path+'annulusplot/'
     gif_path=path+'movies/'
     tp=strmid(product_type,1)
     pngfname='annplot_'+date+'_'+event.label+'_'+wavelength+'_'+tp
;The png image files
     imgfnames = savepath + product_type + '/' + wavelength + '/' + pngfname + '_%03d.png'
     imgsearch = savepath + product_type + '/' + wavelength + '/' + pngfname + '_*.png'
     gifname = gif_path + product_type + '_' + wavelength + '_' + label + '.gif'
  endif

  if product_type eq 'teem' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_map.png'
     imgsearch = savepath + pngfname + '_*_teem_map.png'
     gifname = gif_path + pngfname + '_teem_map.gif'
  endif

  if product_type eq 'teem_density' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_density.png'
     imgsearch = savepath + pngfname + '_*_teem_density.png'
     gifname = gif_path + pngfname + '_teem_density.gif'
  endif

  if product_type eq 'teem_temperature' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_temperature.png'
     imgsearch = savepath + pngfname + '_*_teem_temperature.png'
     gifname = gif_path + pngfname + '_teem_temperature.gif'
  endif

  if product_type eq 'em_ratios' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_em_ratios.png'
     imgsearch = savepath + pngfname + '_*_teem_em_ratios.png'
     gifname = gif_path + pngfname + '_teem_em_ratios.gif'
  endif

  if product_type eq 'em_differences' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_em_differences.png'
     imgsearch = savepath + pngfname + '_*_teem_em_differences.png'
     gifname = gif_path + pngfname + '_teem_em_differences.gif'
  endif

  if product_type eq 'density_ratios' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_density_ratios.png'
     imgsearch = savepath + pngfname + '_*_teem_density_ratios.png'
     gifname = gif_path + pngfname + '_teem_density_ratios.gif'
  endif

  if product_type eq 'density_differences' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_density_differences.png'
     imgsearch = savepath + pngfname + '_*_teem_density_differences.png'
     gifname = gif_path + pngfname + '_teem_density_differences.gif'
  endif
  
  if product_type eq 'temperature_ratios' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_temperature_ratios.png'
     imgsearch = savepath + pngfname + '_*_teem_temperature_ratios.png'
     gifname = gif_path + pngfname + '_teem_temperature_ratios.gif'
  endif

  if product_type eq 'temperature_differences' then begin
     savepath=event.aschdempath
     gif_path=event.gifpath
     pngfname='aschdem_'+date+'_'+label
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d_teem_temperature_differences.png'
     imgsearch = savepath + pngfname + '_*_teem_temperature_differences.png'
     gifname = gif_path + pngfname + '_teem_temperature_differences.gif'
  endif

  if product_type eq 'pfss_shock_hires' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='aia_pfss_shock_'+date+'_'+label+'_hires'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'pfss_shock' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='aia_pfss_shock_'+date+'_'+label+'_lores'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'thetabn' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='thetabn_'+date+'_'+label+'_lores'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'thetabn_hires' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='thetabn_'+date+'_'+label+'_hires'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'thetabn_oplot' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='thetabn_'+date+'_'+label+'_lores'
                                ;The png image files
     imgfnames = savepath + pngfname + '_oplot_%03d.png'
     imgsearch = savepath + pngfname + '_oplot_???.png'
     gifname = gif_path + pngfname + '_oplot.gif'
  endif

  if product_type eq 'thetabn_oplot_hires' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='thetabn_'+date+'_'+label+'_hires'
                                ;The png image files
     imgfnames = savepath + pngfname + '_oplot_%03d.png'
     imgsearch = savepath + pngfname + '_oplot_???.png'
     gifname = gif_path + pngfname + '_oplot.gif'
  endif

  if product_type eq 'pfss_spread' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='aia_pfss_shock_angular_influence_'+date+'_'+label+'_lores'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'pfss_spread_hires' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='aia_pfss_shock_angular_influence_'+date+'_'+label+'_hires'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'pfss_spread_topview' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='aia_pfss_shock_angular_influence_'+date+'_'+label+'_topview_lores'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'pfss_spread_topview_hires' then begin
     savepath=event.mfhcpath
     gif_path=event.gifpath
     pngfname='aia_pfss_shock_angular_influence_'+date+'_'+label+'_topview_hires'
                                ;The png image files
     imgfnames = savepath + pngfname + '_%03d.png'
     imgsearch = savepath + pngfname + '_???.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  if product_type eq 'ywave' then begin
     savepath=event.yaftawavepath
     gif_path=event.gifpath
     pngfname='yaftawave_'+date+'_'+label+'_'+wavelength
                                ;The png image files
     imgfnames = savepath + pngfname + '_%06d.png'
     imgsearch = savepath + pngfname + '_??????.png'
     gifname = gif_path + pngfname + '.gif'
  endif

  print,''
  print,'----'
  print,'Creating animated GIF ' + gifname
  print,''

  if not file_test(imgsearch) then begin
     print,'Required PNG files do not exist: '+imgsearch
     print,'Quitting...'
     print,'----'
     return
  endif

  if file_test(gifname) and not keyword_set(force) then begin
   print,'This GIF file exists. To overwrite, rerun with /force. Quitting...'
   print,'----'
   return
endif

  ;According to the movie type, find the files to animate.
  imgs=file_search(imgsearch)
  nimgs=n_elements(imgs)
  if n_elements(imgs) lt step+1 then begin
     print,'Not enough frames to make movie '+moviefname
     return
  endif

  ;Create a temporary folder to order the images properly.
  tmpdir=gif_path+"tmpdir_"+strtrim(string(fix(floor(randomn(2)*100000))),2)+'/'
  res=file_test(tmpdir,/directory)
  if res then spawn,'rm '+tmpdir+'*' $
  else spawn,'mkdir -m 775 '+tmpdir

  
  for ii=0,n_elements(imgs)-1,step do begin
     img_strind=strtrim(string(ii+1),2)
     if img_strind lt 100 then img_strind='0'+img_strind
     if img_strind lt 10 then img_strind='0'+img_strind
     command='convert -resize 200x '+imgs[ii]+' '+tmpdir+'tmpim_'+img_strind+'.png'
     spawn,command
  endfor
  imgfnames=tmpdir+'tmpim_???.png'

  gif_command='convert -delay 40 -loop 0 '+imgfnames+' '+gifname
  spawn,gif_command
  ;Remove the temporary folder
  spawn,'rm -rf '+tmpdir
end
