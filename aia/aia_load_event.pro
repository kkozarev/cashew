pro test_aia_load_event
;Test the loading of data

;Load the information about events
events=load_events_info(label='130411_01')
st=events[0].st
;Create a short interval - add 4 mins to the start time of the event
;et=aia_augment_timestring(events[0].st,240)
et=events[0].et
wav='193'
coords=[events[0].coordX,events[0].coordY]
;run aia_load_event - remove AEC images, return the subdata array and index
;aia_load_event,events[0],wav,index,data,coords=coords,subdata=subdata,subindex=subindex,/remove_aec,/subroi
aia_load_event,events[0],wav,index=index,data=data,/nodata
end


pro aia_load_event,event,wav,index=index,data=data,coords=coords,map=map,subdata=subdata,$
                   subindex=subindex,submap=submap,remove_aec=remove_aec, subroi=subroi,$
                   nodata=nodata,first=first,local=local,nosave=nosave,force=force,_extra=_extra
;PURPOSE:
;This procedure will load full AIA data for a wavelength and time interval,
;then allow the user to select a subregion, and create sub-arrays and submaps
;from that selection. When this is done the data can be easily manipulated.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;       event - the event structure
;	wave - wavelength of the AIA channel, string - 94,131,171,193,211,304,335
;
;KEYWORDS:
;       subroi - if set, return the sub-region of interest data
;       nosave - don't save the loaded data in a file
;       nodata - don't load data
;
;OUTPUTS:
;       (for example,savefile='/Volumes/PLUME/AIA_data/normalized_noprep_100818_193')
;       NB! DO NOT INCLUDE THE '.sav' or other extension in the filename.
;       map - if keyword is set, the map of the data will also be created.
;       subdata - if set, determine the subregion and return it. Also
;                 return subindex
;       submap - if selected, create a submap that corresponds to the subdata.
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/2011
;Added a keyword remove_aec to check for automatic exposure control
;(AEC) images and remove them from the datacube - KAK 09/30/2013
;Added the AR coordinates as a required parameter - KAK 09/30/2013
;Added a nodata keyword, streamlined to work with event structure - KAK 11/14/2013
;Turned into a proper wrapper - KAK 05/27/2013

  wave=wav[0]
  map=1
  aiafov=event.aiafov
  ;if keyword_set(subroi) then subroi=1
  savefile=event.savepath+event.aia_savename+wav
  if keyword_set(nosave) then savefile=''
;1. Load the data, prep it. Use aia_data_load
;   if keyword_set(map) then begin
      aia_load_data,event.st,event.et,wave,index,data,savefile=savefile,map=map,remove_aec=remove_aec,$
                    event=event,nodata=nodata,first=first,local=local,subdata=subdata,subindex=subindex,$
                    coords=coords,subroi=subroi,force=force,_extra=_extra
;   endif else begin
;      aia_load_data,event.st,event.et,wave,index,data,savefile=savefile,remove_aec=remove_aec,event=event,$
;                    nodata=nodata,first=first,local=local,subdata=subdata,subindex=subindex,coords=coords,subroi=subroi
;   endelse

end
