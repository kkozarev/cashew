pro test_aia_load_data
;Run for one or a few events like this:
  one=1
  if one eq 1 then begin
     labels=['140708_01','131212_01','130517_01','130423_01','120915_01','120526_01',$
	  '120424_01','110607_01','110211_02','110125_01']
     labels=['110211_02','110125_01'] 
    for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        ;wav='193'
	wavelengths=['94','131','171','211','335']
        for w=0,n_elements(wavelengths)-1 do begin
           wav=wavelengths[w]
        ;aia_load_data,event.st,event.et,wav,event=event,/nodata
        aia_load_data,event.st,event.et,wav,event=event,/subroi
        endfor
     endfor
  endif
  
  ;Alternatively, run for all the events:
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wav=wavelengths[w]
           aia_load_data,event.st,event.et,wav,event=event,subdata=subdata,subindex=subindex,/subroi,/force
        endfor
     endfor
  endif

end
 

function aia_get_fname,s,st,wv,loud=loud
;print,'enter aia_get_fname'
;print,s
;print,st
;print,wv
sec=strtrim(string(s),2)
   if sec eq 0 then sec='0'
   min=fix(st[4])
   hour=fix(st[3])
   if sec lt 0 then begin
      sec = strtrim(string(sec+60-st[5]+1),2)
      min = strtrim(string(min-1),2)
      if min eq 0 then min = '0'
      if min lt 0 then begin
	 min = strtrim(string(59),2)
	 hour = strtrim(string(hour-1),2)
	 if hour eq 0 then hour = '0'
      endif
   endif else begin
	 if sec gt 59 then begin
	    sec = strtrim(string(sec-60),2)
	    min = min+1
	    if min eq 60 then begin
	       min = '0'
	       hour=strtrim(string(hour+1),2)
	    endif
	 endif
   endelse
if sec lt 10 then sec='0'+strtrim(string(sec),2)
if min lt 10 then min='0'+strtrim(string(min),2)
if hour lt 10 then hour='0'+strtrim(string(hour),2)
fname='AIA'+st[0]+st[1]+st[2]+'_'+strtrim(string(hour),2)+strtrim(string(min),2)+strtrim(string(sec),2)+'_0'+wv+'.fits'
if loud eq 1 then print, fname
return, fname
end
;-----------------------------------------------------------------------


;-----------------------------------------------------------------------

pro aia_load_data,stt,ett,wav,index,data,savefile=savefile,nodata=nodata,map=map,submap=submap,$
                  original=original,noprep=noprep,quiet=quiet,local=local,archive=archive,$
                  first=first,remove_aec=remove_aec,event=event,coords=coords,$
                  force=force,subroi=subroi,subdata=subdata,subindex=subindex
;PURPOSE
;This procedure reads in a sequence of AIA fits images from the CfA
;archive and returns/saves a prepped data cube and index
;
;CATEGORY:
;AIA/General
;
;INPUTS:
;	stt - a string date
;	(Example: '2011/01/15 00:00:10')	
;	ett - a string date
;	(Example: '2011/01/15 00:00:50')
;       NOTE - any format is accepted, as long as the numbers are ordered as
;       YYYY MM DD hh mm ss, and the delimiters are ' /:,.-T'.
;
;       wave - a string or a string array for the wavelength/s you want.
;
;OPTIONAL INPUT:
;      nodata - if keyword is set, no data is downloaded.
;      map - saves a map of the data in the provided variable
;      original - do not normalize the data to one second exposure
;      noprep - do not run aia_prep to make level 1.5 data
;      savefile - 
;
;OUTPUT:
;
;       data - a datacube with the (prepped) data
;       index - a structure of indices for the data
;
;OPTIONAL OUTPUT:
;       savefile - if a filename is specified here, the data and index
;                  will be written into a file.
;
;DEPENDENCIES:
;aia_file_search,aia_check_dirs,vso_search,index2map, read_sdo
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/2010
;Added a keyword remove_aec to check for automatic exposure control
;(AEC) images and remove them from the datacube - KAK 09/30/2013
;Fixed the savefile keyword - KAK 05/27/2014

;===========================================================
;Constants and definitions
;===========================================================
cfaarc='/Data/SDO/AIA/level1/'
locarc=getenv('CORWAV_DATA')+'AIA_data/'
if keyword_set(archive) then locarc=archive
;===========================================================

loud=1
if keyword_set(quiet) then loud=0
if not keyword_set(coords) then if keyword_set(event) then $
       coords=[event.coordX,event.coordY] else coords=[0.0,0.0]
if not keyword_set(event) then label='event' else label=event.label
if not keyword_set(event) then begin
   tmp=strsplit(stt,'/ ',/extract)
   date=tmp[0]+tmp[1]+tmp[2]
endif else date=event.date
if keyword_set(force) then print,'FORCE!'
;First thing to do is check if the data has already been loaded into data cubes.

if keyword_set(event) then path=event.savepath else path='./'
if keyword_set(event) then aiafov=event.aiafov else aiafov=[1024,1024]

if keyword_set(savefile) then begin
   if savefile eq '' then begin
      datfname=path+'normalized_AIA_'+date+'_'+label+'_'+wav+'.sav'
      subfname=path+'normalized_AIA_'+date+'_'+label+'_'+wav+'_subdata.sav'
   endif else begin
      datfname=savefile+'.sav'
      subfname=savefile+'_subdata.sav'
   endelse
endif else begin
   datfname=path+'normalized_AIA_'+date+'_'+label+'_'+wav+'.sav'
   subfname=path+'normalized_AIA_'+date+'_'+label+'_'+wav+'_subdata.sav'
endelse


if keyword_set(event) then begin
   datfname=path+event.aia_savename+wav+'.sav'
   subfname=path+event.aia_savename+wav+'_subdata.sav'
endif


if not keyword_set(force) and keyword_set(subroi) and file_exist(subfname) then begin
   restore,subfname
   if keyword_set(submap) then index2map,subindex,subdata,submap
   return
endif


if not keyword_set(force) and file_exist(datfname) then begin
   restore,datfname
   if not file_exist(subfname) and keyword_set(subroi) then begin
      newcoords=aia_autoselect_subroi(index[0],coords,event=event)
      subdata=aia_inspect_data(index,data,autoregion=newcoords,event=event)
      subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],aiafov,coords)
      save,subindex,subdata,filename=subfname
   endif else begin
      restore, subfname
   endelse
   if keyword_set(submap) then index2map,subindex,subdata,submap
   return
endif else begin

;check if wave is an array or not...
   wave=wav
   nwav=n_elements(wave)
   starttime=stt
   endtime=ett
   
;Record the times in string arrays
   if n_elements(starttime) eq 1 then begin
      st=strsplit(starttime,' /:,.-T',/extract)
      if n_elements(st) eq 4 then st=[st,'00']
      if n_elements(st) eq 5 then st=[st,'00']
   endif else begin
      st=starttime
   endelse
   
   if n_elements(endtime) eq 1 then begin
      et=strsplit(endtime,' /:,.-T',/extract)
      if n_elements(et) eq 4 then et=[et,'00']
      if n_elements(et) eq 5 then et=[et,'00']
   endif else begin
      et=endtime
   endelse
   
   if loud eq 1 then print,''
   
   if st[0] ne et[0] or st[1] ne et[1] or st[2] ne et[2] then begin
      print,'Please choose data from the same date only. Exiting...'
      print,''
      return
   endif
   
   if st[3] gt et[3] then begin
      print,'Wrong date input. Exiting...'
      print,''
      return
   endif
   if st[3] eq et[3] and st[4] gt et[4] then begin
      print,'Wrong date input. Exiting...'
      print,''
      return
   endif
   
   if loud eq 1 then begin
      print,'-----------------LOCAL ARCHIVE---------------------'
      print,''
      print,'Local archive is at '+locarc
   endif
   
;First search on the local archive in the CfA and JSOC formats
   files=aia_file_search(starttime,endtime,wave,loud=loud,missing=locmissing,path=locarc)
   if files[0] eq '' then files=aia_file_search(starttime,endtime,wave,loud=loud,missing=locmissing,/jsoc,path=locarc,remove_aec=remove_aec)
   
;Then check the cfa archive...
   if files[0] eq '' then begin
      if loud eq 1 then begin
         print, 'Nothing in the personal archive...'
         print,'Checking CfA archive at '+cfaarc+':'
      endif
      files=aia_file_search(starttime,endtime,wave,loud=loud,missing=cfamissing,path=cfaarc,remove_aec=remove_aec)
   endif
   
   
;If there is nothing in the local archive, check the JSOC archive...
   if files[0] eq '' then begin
      if loud eq 1 then begin
         print, 'Nothing in the CfA archive...'
         print,'---------------------------------------------------'
      endif
      
;If the user wants, check the JSOC archive.
      if not keyword_set(local) then begin
         if loud eq 1 then begin
            print,''
            print,'--------------JSOC REMOTE ARCHIVE------------------'
            print,''
         endif
         
;Do a search on the vso remote database
         for w=0,nwav-1 do begin
            vsosearch=vso_search(st[0]+'/'+st[1]+'/'+st[2]+' '+st[3]+':'+st[4]+':'+st[5],$
                                 et[0]+'/'+et[1]+'/'+et[2]+' '+et[3]+':'+et[4]+':'+et[5],$
                                 instr='aia',wave=wave[w],/url)
            if w eq 0 then rec=vsosearch else rec=[rec,vsosearch]
         endfor
         if not keyword_set(nodata) and loud eq 1 then print,'Downloading files from JSOC archive...'
         numrec=n_elements(rec)
         
                                ;check if the record is a structure
         if size(rec,/type) ne 8 then begin
            print,''
            print,'The record is incomplete! No data from JSOC.'
            print,'---------------------------------------------'
            print,''
            return
         endif
                                ;before writing the files, check that
                                ;the directories exist, and make them
                                ;if not.
         
         aia_check_dirs,locarc,st,et
                                ;copy files in their appropriate directories
         if st[3] ne et[3] then begin
            spl=strarr(numrec)
            for i=0,numrec-1 do begin
               rh=strsplit(rec[i].time.start,' /:,.T-',/extract)
               spl[i]=rh[3]
            endfor
            for t=fix(st[3]),fix(et[3]) do begin
               if t lt 10 then hr='0'+strtrim(string(t),2) else hr=strtrim(string(t),2)
               ind=where(spl eq hr)
               if not keyword_set(nodata) then $
                  sock_copy,rec[ind].url,out_dir=locarc+st[0]+'/'+st[1]+'/'+st[2]+'/H'+hr+'00/'
               if keyword_set(first) then break
            endfor
         endif else begin
            if not keyword_set(nodata) then $
               sock_copy,rec.url,out_dir=locarc+st[0]+'/'+st[1]+'/'+st[2]+'/H'+st[3]+'00/'
         endelse
         if not keyword_set(nodata) then files=aia_file_search(starttime,endtime,wave,loud=loud,missing=locmissing,/jsoc,remove_aec=remove_aec,path=locarc)
         
         if files[0] eq '' then begin
            if loud eq 1 then print,'There was a problem getting data from the remote archive!'
         endif
      endif
   endif
   
   if loud eq 1 then begin
      print,''
      print,'---------------------------------------------------'    
      print,''
   endif
   
   nfiles=n_elements(files)
   
   
;if not keyword_set(nodata) and not keyword_set(remote) then begin
;go and search for every single file
;    n=0
;    for i=0,numrec-1 do begin
;        stp=strsplit(rec[i].time._end,' /:,.-T',/extract)
;        dirpath=basepath+stp[0]+'/'+stp[1]+'/'+stp[2]+'/H'+stp[3]+'00/'
;        wv=strtrim(string(floor(rec[i].wave.max)),2)
   
;since the naming convention is messed up, the times on the
;filenames don't correspond necessarily to the times in the
;structures. Search several seconds around the original time.
;        sec=fix(stp[5])
;        for s=sec-2,sec+2 do begin
;            fname=aia_get_fname(s,stp,wv)
;                                ;print,dirpath+fname
;            file=file_search(dirpath+fname)
;            if file ne '' then begin
;                print,dirpath+fname
;                if n gt 0 then files=[files,file]
;                if n eq 0 then begin
;                    files=file
;                    n=1
;                endif
;                break
;            endif
;        endfor
;    endfor
;endif   
   
   
;if exist(files) eq 0 or keyword_set(remote) then begin
;   print,'The files do not exist on the local CfA archive - try another time/wavelength'
;   print,''
;   print,'------------------------------------------------'
;   print,''
;   return
;endif
   
   
   
;print,''
;print,'Total number of files in local archive: '+strtrim(string(nfiles),2)
;print,''
   
   
;===========================================================
   
   
;===========================================================
;Load the data
   if keyword_set(first) then begin
      files=files[0]
      nfiles=1
   endif
   
   if keyword_set(nodata) then begin
      read_sdo,files,index,data,/nodata
      return
   endif else begin   
      read_sdo,files,ind,dat,/uncomp_delete
   endelse
   
   if keyword_set(noprep) then begin
      data=dat
      index=ind
      dat=0
      ind=0
   endif else begin
      aia_prep,ind,dat,index,data
      ind=0
      dat=0
   endelse
   
endelse                         ;This if-else statement is the fits vs. save file loading     

if keyword_set(first) then begin
   data=data[*,*,0]
   index=index[0]
endif

;Normalize the data
if not keyword_set(original) then for i=0,nfiles-1 do data[*,*,i]/=index[i].exptime


if loud eq 1 then print,"making the map in aia_load_data"
index2map,index,data,map

;Make subroi data array.
if keyword_set(subroi) or keyword_set(subdata) or keyword_set(subindex) then begin
   if keyword_set(event) then fov=event.aiafov else fov=[1024,1024]
   newcoords=aia_autoselect_subroi(index[0],coords,event=event)
   subdata=aia_inspect_data(index,data,autoregion=newcoords,event=event)
   subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],fov,coords)
   if subfname ne '' then save,subindex,subdata,filename=subfname
endif else begin
   if datfname ne '' then save,index,data,filename=datfname
endelse


;   if not keyword_set(map) then begin
;      print,''
;      print,'The /map keyword should be selected! Making map for you...'
;      index2map,index,data,map
;      aia_inspect_map,map,submap=submap
;   endif else begin
;      aia_inspect_map,map,submap=submap
;   endelse


;if keyword_set(archive) then begin
;   save,index,data,filename=path+datfname
;   if keyword_set(subroi) then save,subindex,subdata,filename=path+subfname
;endif

end
