pro test_make_rhessi_maps

event=load_events_info(label='131106_01')
hsimap=make_rhessi_maps(event)
stop
end


function make_rhessi_maps,event,energy_range=energy_range,time_bin=time_bin
;PURPOSE
;This procedure reads RHESSI data for a given interval and prepares 
;
;CATEGORY:
;AIA/General
;
;INPUTS:
;	event - an event structure, created by calling load_events_info
;
;
;OPTIONAL INPUT:
;      energy_range - an array of electron energy values [keV], for
;                     which to return RHESSI data.
;                     Default is [10, 20, 50, 100, 200, 300]
;      time_bin - observation binning [sec]. Default is 8
;OUTPUT:
;      Returns a mapcube with images and headers for all timesteps, in
;      all energy bins
;
;OPTIONAL OUTPUT:
;
;DEPENDENCIES:
;      search_network, index2map
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 11/2013

  search_network, /enable
  start_time=event.st
  end_time=event.et
  if not keyword_set(time_bin) then timbin=8 else timbin=time_bin

  clean_niter=175
  fov=256
  pix_size=[1.0,1.0]
  dim=fov/pix_size
  det=[0, 0, 1, 1, 1, 1, 1, 1, 1]
  if not keyword_set(energy_range) then erange=[10, 20., 50., 100., 200., 300.]
  offset=0
  o=hsi_image()
  o -> set, im_time_interval= [start_time,end_time]
  o->set, im_time_bin = timbin
  o -> set, im_energy_binning= erange
  o -> set, image_algorithm= 'clean'
  o -> set, det_index_mask=det
  o->set, use_rate=1
  o->set, use_local_average =1
  o->set, use_single_return_mode =0 ;if 0, return all the images at once, if 1 - return them one by one.
  o->set, use_flare_xyoffset=1
  if offset[0] ne 0 then o->set, xyoffset=offset
  o -> set, pixel_size=pix_size
  o -> set, image_dim=dim
  o->set, uniform_weighting=0		;;you can either see uniform weighting OR natural - see documentation
  o->set, natural_weighting= 1
  o->set, use_phz_stacker= 1
  o->set, clean_niter=clean_niter	;;max number of clean iterations, this can be set higher or lower
  o->set, clean_mark=0
  o->set, clean_progress_bar=0
  o->set, clean_show_maps=0     
  im=o->getdata()
  if max(im) eq 0.0 then begin
     print,''
     print,'No data for this time interval. Continuing...'
     print,''
  endif
  print,o->get(/xyoffset)
  print,o->get(/im_tim_bin)
                                ;Record the xy offset for the second plot
  ffset=o->get(/xyoffset)
                                ;Write to a file
  o->fitswrite, this_out_filename='tmp.fits'
  mreadfits,'tmp.fits',index,data,outsize=outsize,header=fhead,_extra=extra
  index2map,index,data,hsimap,err=err,/no_copy,_extra=extra
  stop
  return,hsimap

end
