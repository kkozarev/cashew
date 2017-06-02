pro test_aia_archive_local_data
;Test aia_archive_local_data

  labels=['110307_02','130522_01','130605_01']
labels=['160316_01','151223_01','151219_01','151204_01','151109_01','151029_01','151015_01','150920_01','150725_01','150625_01','150601_01','150512_01','150509_01','150421_01','150309_01','150303_01','150209_01','141205_01','141105_02','141105_01','140901_01']
  for ev=0,n_elements(labels)-1 do begin
     label=labels[ev]
     event=load_events_info(label=label)
     aia_archive_local_data,event,/force
  endfor
  
end

pro aia_archive_local_data_main,files,locarc,force=force
;Copy the files from the CfA to the local archive one by one, checking their filenames and putting them
;in the appropriate folders.
  nf=n_elements(files)
  cc=0
  for f=0,nf-1 do begin
     file=files[f]
     if file eq '' then continue
     tmp=strsplit(file,'AIA',/extract)
     fname='AIA'+tmp[2]
     tmm=strsplit(tmp[2],'_.',/extract)
     yy=strmid(tmm[0],0,4)
     mm=strmid(tmm[0],4,2)
     dd=strmid(tmm[0],6,2)
     hh=strmid(tmm[1],0,2)
     outpath=locarc+yy+'/'+mm+'/'+dd+'/H'+hh+'00/'

;check whether the file already was copied and skip unless forcing an overwrite.
     if file_exist(outpath+fname) and not keyword_set(force) then continue 
     cc++
     print,'Copying file '+outpath+file
     exec='cp '+file+' '+outpath
     spawn,exec  
  endfor
  if cc eq 0 then print,'       No data to update.'
  
end





pro aia_archive_local_data,event,force=force
;PURPOSE:
;Copy AIA files from the CfA archive to the user's personal
;data archive (set in the $CORWAV_DATA global variable)
;
;CATEGORY:
;AIA/General
;
;INPUTS:
;
;KEYWORDS:
;  force - if set, will overwrite existing files
; 
;OUTPUTS:
;
;DEPENDENCIES:
;aia_file_search, aia_check_dirs, aia_archive_local_data (this file)
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 09/23/2013   
;
  cfaarc='/Data/SDO/AIA/level1/'
  locarc=getenv('CORWAV_DATA')+'AIA_data/'
  wave=['171','193','211','335','094','131']
  locarc=event.aia_datapath
  print,''
  for w=0,n_elements(wave)-1 do begin
     wav=wave[w]
     print,'EVENT '+event.label+' - Copying '+wav+' channel AIA data between '+event.st+' and '+event.et
     files=aia_file_search(event.st,event.et,wav,loud=loud,missing=cfamissing,path=cfaarc)
     print,n_elements(files)
     aia_check_dirs,locarc,event.st,event.et,report=report ;check whether the local folders exist
     print,''
     aia_archive_local_data_main,files,locarc
  endfor
end


