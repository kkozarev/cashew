;+==============================================================================
pro test_aia_cfa_teem_run
  ;test/batch run the AschDEM code
 ;You can run for one or several events, like this.
  one=1
  if one eq 1 then begin
    labels=['140708_01','131212_01','130517_01','130423_01','120915_01']
    labels=['120526_01','120424_01','110607_01','110211_02','110125_01']
    labels=['110211_02','110125_01']
    for ev=0,n_elements(labels)-1 do begin 
       label=labels[ev]
       event=load_events_info(label=label)
;    To do (1): '131107_01','131029_01','131119_01','131207_01'
;    To do (2): '110804_01','110809_01','110211_02','110128_01','110125_01','120728_01','121007_01'
;    Currently being calculated (remove when done): '131105_01', '130517_01', '130501_01'
       aia_cfa_teem_run,event,/remove_aec
     endfor
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        aia_cfa_teem_run,event,/remove_aec
     endfor
  endif
  
end
;-==============================================================================



;+==============================================================================
pro aia_cfa_teem_run,event,savepath=savepath,fileset=fileset,remove_aec=remove_aec,force=force
;PURPOSE:
;Program which runs Marcus Aschwanden's DEM code on an AIA datacube
;
;CATEGORY:
; DEM/AschDEM
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
; aia_load_data, aia_augment_timestring, aia_cfa_coalign_test
; aia_cfa_teem_table, aia_file_search, aia_cfa_teem_map, aia_cfa_teem_disp
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/2013
;10/01/2013, KAK - added remove_aec keyword that does not load AEC exposures.
;
  set_plot,'x'
  resolve_routine,'aia_load_data',/either,/compile_full_file,/no_recompile
  
  if not keyword_set(fileset) then fileset ='aschdem'
  
;INPUTS
;
  if not keyword_set(savepath) then savepath=event.aschdempath
  st=event.st
  et=event.et
  date=event.date
  label=event.label
  arcoords=[event.coordX,event.coordY]
  
;Wavelengths ordered by temperature?
  wave =['131','171','193','211','335','94'] 
  nwave =n_elements(wave)
  
;==================================
;PHYSICAL PARAMETERS - DO NOT TOUCH
  io=3                          ;   (0=screen, 3=color postscript file)
  ct=3                          ;   (IDL color table) 
  nsig=3                        ;   (contrast in number of standard deviations)
  nsm=7                         ;   (smoothing boxcar of limb profiles)
  te_range=[0.5,10]*1.e6        ;   ([K], valid temperature range for DEM solutions)
  tsig=0.1*(1+1*findgen(10))    ;   (values of Gaussian logarithmic temperature widths)
  q94=6.7                       ;   (correction factor for low-temperature 94 A response)
  fov=[0.5,0.5,1,1]*1.25        ;   (field-of-view [x1,y1,x2,y2] in solar radii)
  npix=1                        ;   (macropixel size=8x8 pixels, yields 512x512 map)
;=================================
  
  
;Obtain all the necessary filenames
  for w=0,nwave-1 do begin
     if keyword_set(remove_aec) then ftmp=aia_file_search(st,et,wave[w],/check171,event=event,/remove_aec) $
     else ftmp=aia_file_search(st,et,wave[w],/check171,event=event)
     
                                ;Create the filename structure
     if w eq 0 then begin
        fstr={w131:'',w171:'',w193:'',w211:'',w335:'',w94:''}
        nfiles=n_elements(ftmp)
        files=replicate(fstr,nfiles)
     endif
     if n_elements(ftmp) lt nfiles then begin
        nfiles=n_elements(ftmp)
        files=files[0:nfiles-1]
     endif
     ftmp=ftmp[0:nfiles-1]
     
     case w of
        0: files.w131=ftmp
        1: files.w171=ftmp
        2: files.w193=ftmp
        3: files.w211=ftmp
        4: files.w335=ftmp
        5: files.w94=ftmp
     endcase   
  endfor
  
;Loop over the data
  for ff=0,nfiles-1 do begin
     stepfiles=[files[ff].w131,files[ff].w171,files[ff].w193,files[ff].w211,files[ff].w335,files[ff].w94]
     
;Get the file names of the resulting maps and tables
     tmp=strsplit(files[ff].w171,'_',/extract)
     mind=n_elements(tmp)-2
     
     tmp=tmp[mind]
     tmpstr=+strmid(tmp,0,2)+strmid(tmp,2,2)+strmid(tmp,4,2)
     
     prefix=fileset+'_'+date+'_'+label
     teem_fname=savepath+prefix+'_'+tmpstr+'_'+'teem'
     table_fname=savepath+prefix+'_'+'teem'
     teem_table=table_fname+'_table.sav' ; (savefile that contains DEM loopup table)
     teem_map=teem_fname+'_map'
     teem_tot=teem_fname+'_tot.sav'
     
     if not file_exist(teem_map+'.sav') or keyword_set(force) then begin
;Make some initial tests on the data for fitness determination   
        if ff eq 0 then begin
           aia_cfa_teem_table,stepfiles,wave,tsig,te_range,q94,teem_table
           aia_cfa_coalign_test,stepfiles,table_fname,wave,io,ct,nsig,nsm,h_km,dx,dy
        endif
        aia_cfa_teem_map,stepfiles,arcoords,wave,npix,teem_table,teem_map,event=event
        aia_cfa_teem_total,stepfiles,arcoords,wave,npix,q94,teem_table,teem_map+'.sav',teem_tot,event=event
     endif
     
     aia_cfa_teem_disp,teem_map,te_range,st
     
  endfor
  
end
;-==============================================================================
