pro test_aia_report_local_data

  one=1
  if one eq 1 then begin
     labels=['110307_02','130522_01','130605_01']
     nlabels=n_elements(labels)
     reports=strarr(nlabels)
     for ev=0,nlabels-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        aia_report_local_data,event,report
        reports[ev]=report
     endfor
  endif

  all=0
  if all eq 1 then begin
     events=load_events_info()
     nevents=n_elements(events)
     reports=strarr(nevents)
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        aia_report_local_data,event,report,event_struct=instruct
        if ev eq 0 then report_struct=replicate(instruct,nevents)
        report_struct[ev]=instruct
        reports[ev]=report
     endfor
  endif
end

pro aia_report_local_data_main,files,locarc,report
;Check the data for individual events in the local data archive.
  nf=n_elements(files)
  report=''
  newline = string(10B)
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

;check whether the file already was copied.
     if file_exist(outpath+fname) then continue
     ;if cc eq 0 then report+='Missing file(s): '+newline
     cc++
     report+=file+newline
  endfor
  ;if cc eq 0 then report+=' ALL DATA PRESENT'+newline
  
end





pro aia_report_local_data,event,report,event_struct=event_struct
;PURPOSE:
;Report what data is available in the user's personal
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
;aia_file_search, aia_check_dirs, aia_report_local_data (this file)
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 05/30/2016   
;
  event_struct={label:event.label,missing:replicate({wav:'',files:''},6)}
  report=''
  newline = string(10B)
  cfaarc='/Data/SDO/AIA/level1/'
  locarc=getenv('CORWAV_DATA')+'AIA_data/'
  wave=['094','131','171','193','211','335']
  locarc=event.aia_datapath
  for w=0,n_elements(wave)-1 do begin
     wav=wave[w]
     event_struct.missing[w].wav=wav
     print,'Reporting on event '+event.label+' - '+wav+' channel AIA data between '+event.st+' and '+event.et
     report+='EVENT '+event.label+' - '+wav+' channel AIA data between '+event.st+' and '+event.et+newline
     files=aia_file_search(event.st,event.et,wav,loud=loud,missing=cfamissing,path=cfaarc)
     ;aia_check_dirs,locarc,event.st,event.et,report=report ;check whether the local folders exist
     aia_report_local_data_main,files,locarc,wav_report
     if wav_report eq '' then report+='ALL DATA PRESENT'+newline else $
        report+=wav_report+newline
     event_struct.missing[w].files=wav_report
  endfor
end


