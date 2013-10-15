pro batch_archive_local_data
;PURPOSE:
;Copy AIA files from the CfA archive to the local archive in Backscratch
;
;CATEGORY:
;AIA/General
;
;INPUTS:
;
;KEYWORDS:
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
  locarc='/Volumes/Backscratch/Users/kkozarev/AIA_data/'

  wave=['171','193','211','335','094','131','304']
  sts=['2011/01/25 11:50:00','2011/01/28 00:40:00','2011/02/11 12:25:00','2011/05/11 02:05:00',$
       '2011/08/04 03:45:00','2011/08/09 07:55:00','2011/10/20 03:00:00','2011/11/09 12:55:00',$
       '2012/04/24 07:20:00','2012/05/26 20:25:00','2012/07/28 20:30:00','2012/09/15 22:45:00',$
       '2012/10/07 20:10:00','2013/04/23 18:00:00','2013/05/01 02:10:00','2013/05/17 08:40:00']

;The ending times are not inclusive in the minutes - that is, if the
;ending time for an event is '2011/01/25 12:26:00', an image taken at
;12:26:10 won't be read in.
  ets=['2011/01/25 12:30:00','2011/01/28 01:20:00','2011/02/11 13:05:00','2011/05/11 02:45:00',$
       '2011/08/04 04:25:00','2011/08/09 08:25:00','2011/10/20 03:40:00','2011/11/09 13:35:00',$
       '2012/04/24 08:00:00','2012/05/26 21:05:00','2012/07/28 21:10:00','2012/09/15 23:25:00',$
       '2012/10/07 20:50:00','2013/04/23 18:40:00','2013/05/01 02:50:00','2013/05/17 09:15:00']
  
  ;choose which of all the events to load data for
  ;events2run=[0,1,2,3,4,5,6,7,8,9]
  events2run=15
  waves2run=indgen(7)
  
  for j=0,n_elements(events2run)-1 do begin
     ev=events2run[j]
     print,''
     for w=0,n_elements(waves2run)-1 do begin
        wav=wave[waves2run[w]]
        print,'EVENT #'+strtrim(string(ev+1),2)+' - Copying '+wav+' channel AIA data between '+sts[ev]+' and '+ets[ev]
        files=aia_file_search(sts[ev],ets[ev],wav,loud=loud,missing=cfamissing,path=cfaarc)
        aia_check_dirs,locarc,sts[ev],ets[ev]
        aia_archive_local_data,files,locarc
     endfor
     print,''
  endfor
end


pro aia_archive_local_data,files,locarc
;Copy the files from the CfA to the local archive one by one, checking their filenames and putting them
;in the appropriate folders.
  nf=n_elements(files)
  
  for f=0,nf-1 do begin
     file=files[f]
     if file eq '' then continue
     tmp=strsplit(file,'AIA',/extract)
     tmm=strsplit(tmp[2],'_.',/extract)
     yy=strmid(tmm[0],0,4)
     mm=strmid(tmm[0],4,2)
     dd=strmid(tmm[0],6,2)
     hh=strmid(tmm[1],0,2)
     outpath=locarc+yy+'/'+mm+'/'+dd+'/H'+hh+'00/'
     
     exec='cp '+file+' '+outpath
     spawn,exec
     
  endfor
  
  
  
end
