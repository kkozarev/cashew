pro test_aia_load_event
;Test the loading of data

;Load the information about events
events=load_events_info()
st=events[0].st
;Create a short interval - add 4 mins to the start time of the event
et=aia_augment_timestring(events[0].st,240)
wav='193'
coords=[events[0].coordX,events[0].coordY]
;run aia_load_event - remove AEC images, return the subdata array and index
aia_load_event,st,et,wav,index,data,coords=coords,subdata=subdata,subindex=subindex,/remove_aec,/subroi
end



pro aia_load_event,st,et,wav,index,data,coords=coords,savefile=savefile,map=map,subdata=subdata,subindex=subindex,submap=submap,remove_aec=remove_aec, subroi=subroi,event=event,nodata=nodata,first=first,local=local
;PURPOSE:
;This procedure will load full AIA data for a wavelength and time interval,
;then allow the user to select a subregion, and create sub-arrays and submaps
;from that selection. When this is done the data can be easily manipulated.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;	st - start time string, format 'YYYY/MM/DD hh:mm:ss'
;	et - ending time string, format 'YYYY/MM/DD hh:mm:ss'
;	wave - wavelength of the AIA channel, string - 94,131,171,193,211,304,335
;
;KEYWORDS:
; subroi - if set, return the sub-region of interest data
;
;OUTPUTS:
;       savefile - a string to hold the output save file
;       (for example, savefile='/Volumes/PLUME/AIA_data/normalized_noprep_0818_3_A193.sav')
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
;Added a nodata keyword - KAK 11/14/2013
;
  wave=wav
  if keyword_set(event) then aiafov=event.aiafov else aiafov=[1024,1024]

;1. Load the data, prep it. Use aia_data_load
if keyword_set(savefile) then begin
   if keyword_set(map) then begin
      aia_load_data,st,et,wave,index,data,savefile=savefile,map=map,/norm,remove_aec=remove_aec,event=event,nodata=nodata,first=first,local=local
   endif else begin
      aia_load_data,st,et,wave,index,data,savefile=savefile,/norm,remove_aec=remove_aec,event=event,nodata=nodata,first=first,local=local
   endelse
endif else begin
   if keyword_set(map) then begin
      aia_load_data,st,et,wave,index,data,map=map,/norm,remove_aec=remove_aec,event=event,nodata=nodata,first=first,local=local
   endif else begin
      aia_load_data,st,et,wave,index,data,/norm,remove_aec=remove_aec,event=event,nodata=nodata,first=first,local=local
   endelse
endelse

;2. Make 1k data array.
if keyword_set(subroi) then begin
   newcoords=aia_autoselect_subroi(index[0],coords,event=event)
   subdata=aia_inspect_data(index,data,autoregion=newcoords,event=event)
   subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],aiafov,coords)
endif


if keyword_set(submap) then begin
      if not keyword_set(map) then begin
      print,''
      print,'The /map keyword should be selected! Making map for you...'
      index2map,index,data,map
      aia_inspect_map,map,submap=submap
   endif else begin
      aia_inspect_map,map,submap=submap
   endelse
endif


end
