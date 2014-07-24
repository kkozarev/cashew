pro test_aia_transform_px2arcsec
  event=load_events_info(label='110511_01')
  aia_transform_px2arcsec,event,arcsec_array
  stop
end


pro aia_transform_px2arcsec,event,axarray,ayarray,asdata=asdata
;Testing a procedure to transform the pixel positions of AIA images
;into other coordinates, such as arcseconds from Sun center.
  
  wav='193'
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  eventname='AIA_'+date+'_'+label+'_'+wav
  
  aiafile=event.savepath+'normalized_'+eventname+'_subdata.sav'
  restore,aiafile
  subdata=0
  RSUN=subindex[0].rsun_ref/1000. ;Solar radius in km.  
  KMPX=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)
  pscale=subindex.imscl_mp      ;arcsecs per pixel
  
  
                                ;Reconstruct the array
  nx=event.aiafov[0]
  ny=event.aiafov[1] 
  asdata=replicate({x:0.,y:0.},nx,ny)
  axarray=fltarr(event.aiafov[0])
  ayarray=fltarr(event.aiafov[1])
 
  for xx=0,nx-1 do begin
	axarray[xx]=(subindex[0].subroi_X0+xx-2048.)*pscale[0]
     for yy=0,ny-1 do begin
	if xx eq 0 then ayarray[yy]=(subindex[0].subroi_Y0+yy-2048.)*pscale[0]
        asdata[xx,yy].x=(subindex[0].subroi_X0+xx-2048.)*pscale[0]
        asdata[xx,yy].y=(subindex[0].subroi_Y0+yy-2048.)*pscale[0]
     endfor
  endfor
  
end
