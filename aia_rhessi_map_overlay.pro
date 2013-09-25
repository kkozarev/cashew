pro test_aia_rhessi_map_overlay
  datapath="/Users/kkozarev/Desktop/Antonia/"
  rhessipath=datapath+'RHESSI/'
  aiapath=datapath+'AIA/'
  hsi_dates=['2010/08/07',$
             '2012/05/08']
  hsi_start_times=[['18:01:00','18:40:00'],$
                   ['09:40:00','09:27:00']]
  hsi_end_times=[['18:13:00','18:45:00'],$
                 ['09:50:00','09:40:00']]
  min_augment=1                 ;by how many minutes to augment the array
  aia_rhessi_map_overlay,ps=ps,datapath=datapath, rhessipath=rhessipath, aiapath=aiapath, hsi_dates=hsi_dates, hsi_start_times=hsi_start_times, hsi_end_times=hsi_end_times, minaugment=minaugment
end


pro aia_rhessi_map_overlay,ps=ps,datapath=datapath, rhessipath=rhessipath, aiapath=aiapath, hsi_dates=hsi_dates, hsi_start_times=hsi_start_times, hsi_end_times=hsi_end_times, minaugment=minaugment
;PURPOSE:
;Procedure to load RHESSI and AIA data, and overlay them on top of
;each other. RHESSI data-loading code from
;http://hesperia.gsfc.nasa.gov/ssw/hessi/doc/image/hsi_image_howto.html
;This is written for a particular date/times
;
;CATEGORY:
; AIA/RHESSI
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
; hsi_image(), fits2map, aia_load_data
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 04/2013
;
  
  search_network, /enable
  
;------------------------------
;INPUTS
 if not keyword_set(datapath) then datapath="/Users/kkozarev/Desktop/Antonia/"
 if not keyword_set(rhessipath) then rhessipath=datapath+'RHESSI/'
 if not keyword_set(aiapath) then aiapath=datapath+'AIA/'
 if not keyword_set(hsi_dates) then hsi_dates=['2010/08/07','2012/05/08']
 if not keyword_set(hsi_start_times) then hsi_start_times=[['18:01:00','18:40:00'],$
                                                           ['09:40:00','09:27:00']]
 if not keyword_set(hsi_end_times) then hsi_end_times=[['18:13:00','18:45:00'],$
                                                       ['09:50:00','09:40:00']]
 if not keyword_set(min_augment) then min_augment=1 ;by how many minutes to augment the array

  ;RHESSI images parameters
  clean_niter=175
  fov=256
  pix_size=[1.0,1.0]
  dim=fov/pix_size
  det=[0, 0, 1, 1, 1, 1, 1, 1, 1]
  energy_range=[6., 25.]
  offset=0
  wave='171'                    ;AIA wavelength
  if keyword_set(ps) then begin
     rhessi_thick=4
     rhessi_charthick=2
     rhessi_charsize=1.2
     rhessi_color=0
     endif else begin
        rhessi_thick=2
        rhessi_charthick=1.4
        rhessi_charsize=1.4
        rhessi_color=255
     endelse 
  hsi_start_times=transpose(hsi_start_times)
  hsi_end_times=transpose(hsi_end_times)
  
  nevents=n_elements(hsi_start_times[*,0]) ;Number of events
  nobs=n_elements(hsi_start_times[0,*])  ;Number of observation epochs or observation epoch sequences
  errs=intarr(nevents,nobs)
  
;Split the times into HH MM SS string array elements for each epoch sequence.
;Split the dates into YYYY MM DD string array elements for each event
  new_start_times=strarr(nevents,nobs,3)
  new_end_times=strarr(nevents,nobs,3)
  new_dates=strarr(nevents,3)
  for e=0,nevents-1 do begin
     new_dates[e,*]=strsplit(hsi_dates[e],"/",/extract)
     for obs=0,nobs-1 do begin
        tmp=strsplit(hsi_start_times[e,obs],":",/extract)
        new_start_times[e,obs,*]=tmp
        tmp=strsplit(hsi_end_times[e,obs],":",/extract)
        new_end_times[e,obs,*]=tmp
     endfor
  endfor
  
;Load and process the RHESSI data into images, then save them.
  resolve_routine,'aia_load_data',/either,/compile_full_file
  if keyword_set(ps) then set_plot,'ps' else set_plot,'x'
  for e=1,nevents-1 do begin
     date=new_dates[e,0]+new_dates[e,1]+new_dates[e,2]
     for obs=0,nobs-1 do begin
        
 ;       if obs eq 0 then begin
           cc=0
           curr_start_time=hsi_start_times[e,obs]
           tmp=strtrim(string(new_start_times[e,obs,1]+min_augment),2)
           if tmp lt 10 then tmp='0'+tmp
           curr_end_time=new_start_times[e,obs,0]+':'+tmp+':'+new_start_times[e,obs,2]
           filetime=new_start_times[e,obs,0]+new_start_times[e,obs,1]+new_start_times[e,obs,2]
;        endif

       ; stop
        while curr_start_time ne hsi_end_times[e,obs] do begin
           file=rhessipath+'hsi_'+date+'_'+filetime+'_image'
           if not file_exist(file+'.fits') then begin
              
;Create the RHESSI object and specify its parameters
              o=hsi_image()
              o -> set, im_time_interval= [hsi_dates[e]+' '+curr_start_time, hsi_dates[e]+' '+curr_end_time]
              o->set, im_time_bin = 8
              o -> set, im_energy_binning= energy_range
              o -> set, image_algorithm= 'clean'
              o -> set, det_index_mask=det
              o->set, use_rate=1
              o->set, use_local_average =1
              o->set, use_single_return_mode =1
              o->set, use_flare_xyoffset=1
              if obs eq 1 and offset[0] ne 0 then o->set, xyoffset=offset
              o -> set, pixel_size=pix_size
              o -> set, image_dim=dim
              o->set, uniform_weighting=0		;;you can either see uniform weighting OR natural - see documentation
              o->set, natural_weighting= 1
              o->set, use_phz_stacker= 1
              o->set, clean_niter=clean_niter			;;max number of clean iterations, this can be set higher or lower
              o->set, clean_mark=0
              o->set, clean_progress_bar=0
              o->set, clean_show_maps=0     
              im=o->getdata()
              if max(im) eq 0.0 then begin
                 print,''
                 print,'No data for this time interval. Continuing...'
                 print,''
                 errs[e,obs]++
                 continue
              endif
              print,o->get(/xyoffset)
              print,o->get(/im_tim_bin)
                                ;Record the xy offset for the second plot
              if obs eq 0 then offset=o->get(/xyoffset)
           ;Write to a file
              o->fitswrite, this_out_filename=file+'.fits'
           endif
     
           fits2map,file+'.fits',hsimap
           ;Create the AIA Map based on the RHESSI one.
           starttime=hsi_dates[e]+' '+curr_start_time
           endtime=hsi_dates[e]+' '+curr_end_time
           aiamap=0
           aia_load_data,starttime,endtime,wave,index,data,$
                         savefile=savefile,nodata=nodata,map=aiamap,norm=norm,archive=aiapath,/first
           data=0
           aiamap.data=sqrt(aiamap.data)
           aia_lct,r,g,b,wavelnth=index.wavelnth,/load
           tvlct,rr,gg,bb,/get
           
           if keyword_set(ps) then device,file=file+'.eps',/inches,xsize=9.0,ysize=9.0,$
                                          /encaps,/color,/helvetica $
           else wdef,0,800,800
           
           plot_map,aiamap,fov=hsimap,/log,/limb,dmin=10,dmax=50
           plot_map,hsimap,/over,/rotate,thick=rhessi_thick,color=0,levels=[0.55,0.65,0.75,0.85,0.95]*max(hsimap.data)
           stren=strtrim(string(fix(energy_range[0])),2)+'-'+strtrim(string(fix(energy_range[1])),2)
           xyouts,0.26,0.88,'RHESSI '+stren+' keV '+hsi_dates[e]+' '+curr_start_time+'-'+curr_end_time+' UT',/normal,$
                  color=rhessi_color,charsize=rhessi_charsize,charthick=rhessi_charthick

           if keyword_set(ps) then device,/close else write_png,file+'.png',tvrd(),rr,gg,bb
           cc++
;UNDER CONSTRUCTION
           ;Determine the start and end times for the next epoch
           tmp=strtrim(string(new_start_times[e,obs,1]+cc*min_augment),2)
           if tmp lt 10 then tmp='0'+tmp
           curr_start_time=new_start_times[e,obs,0]+':'+tmp+':'+new_start_times[e,obs,2]
           filetime=new_start_times[e,obs,0]+tmp+new_start_times[e,obs,2]
           tmp=strtrim(string(new_start_times[e,obs,1]+(cc+1)*min_augment),2)
           if tmp lt 10 then tmp='0'+tmp
           curr_end_time=new_start_times[e,obs,0]+':'+tmp+':'+new_start_times[e,obs,2]
;HERE, DEVELOP FUNCTIONALITY TO ZERO THE MINUTES AND AUGMENT THE HOURS
;FOR THE START/END TIMES.
;           tmp=fix(new_start_times[e,obs,1])+min_augment
;           if new_start_times[e,obs,0] eq '59' then begin
;              
;           endif
           ;print,curr_start_time+'   '+hsi_end_times[e,obs]
           ;print,curr_start_time eq hsi_end_times[e,obs]
           ;stop
        endwhile
        ;stop
     endfor   
  endfor
;UNDER CONSTRUCTION
set_plot,'x'
end
