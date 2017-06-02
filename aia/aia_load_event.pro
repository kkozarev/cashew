pro test_aia_load_event
;Test the loading of aia data for events
  
  labels=['151104_01','151104_01']
  for ev=0,n_elements(labels)-1 do begin
     label=labels[ev]
     event=load_events_info(label=label)
        aia_load_event,event
  endfor
  
end


pro aia_load_event,event,index=index,data=data,coords=coords,map=map,subdata=subdata,$
                   subindex=subindex,submap=submap,remove_aec=remove_aec, subroi=subroi,$
                   first=first,local=local,force=force,_extra=_extra
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

if keyword_set(subindex) then help,subindex
  waves=['94','131','171','193','211','335']
  if keyword_set(force) then force=1
;1. Load the subroi data for all EUV wavelengths. Use aia_load_data
  for w=0,n_elements(waves)-1 do begin
     wav=waves[w]
     aia_load_data,event.st,event.et,wav,savefile=savefile,map=map,remove_aec=remove_aec,$
                   event=event,nodata=nodata,first=first,local=local,$
                   coords=coords,force=force,_extra=_extra,subroi=subroi,subdata=subdata,subindex=subindex
  endfor
  
;2. Load the full data for the 193 and 211 bands.
;  waves=['193','211']
;  for w=0,n_elements(waves)-1 do begin
;     wav=waves[w]
;    aia_load_data,event.st,event.et,wav,savefile=savefile,map=map,remove_aec=remove_aec,$
;                   event=event,nodata=nodata,first=first,local=local,subdata=subdata,subindex=subindex,$
;                   coords=coords,force=force,_extra=_extra
;  endfor
  
  
end
