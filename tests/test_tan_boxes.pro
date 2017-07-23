pro test_tan_boxes
;Testing automatic generation of boxes tangential to the
;(time-dependent) shock surface
;Written by KAK, March 28, 2014
  path='/home/kkozarev/Desktop/IONIZATION_RAYMOND/'
  event=load_events_info(label='test')
  wav='193'
  evnum=event.label
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  eventname='AIA_'+date+'_'+evnum+'_'+wav
  aiafile=event.savepath+'normalized_'+eventname+'_subdata.sav'
  ;Find a file to load with the latest results of applying the CSGS model
  csgsfile=find_latest_file(event.mfhcpath+'csgs_results_*') 
  if csgsfile eq '' then begin
     print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif
  
;+==============================================================================
;LOAD THE DATA
  print,''
  print,'Loading data...'
  ;Load the CSGS model results
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
  
  ;Load the AIA observations
  print,'Loading AIA File '+aiafile
  restore,aiafile
  
;-==============================================================================

rad_angle=atan((sc[1]-suncenter[1])/(sc[0]-suncenter[0]))
total_ar_rad=sqrt((sc[0]-suncenter[0])^2+(sc[1]-suncenter[1])^2)
newrad=radius+total_ar_rad

shock_x=newrad*cos(rad_angle)+suncenter[0]
shock_y=newrad*sin(rad_angle)+suncenter[1]

xx=findgen(2000)*cos(rad_angle)+suncenter[0]
yy=findgen(2000)*sin(rad_angle)+suncenter[1]

wdef,0,1024
tvscl,sqrt(subdata[*,*,20])
plots,shock_x,shock_y,psym=2,/device



stop

end
