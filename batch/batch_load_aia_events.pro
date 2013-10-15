pro batch_load_aia_events,inspect=inspect

;This batch file will load AIA event data onto the PLUME drive from
;the CfA archive.
;Written by Kamen Kozarev on Jun 22, 2011

;The times and coordinates were taken from a search in the HEK of eruptive events
;between January 01 and May 31, 2011. An additional constraint is that
;the events have to be fast, so most prominence events are excluded.
;The data time windows are a maximum of 30 minutes - this is because
;the data are too big and cumbersome to analyze for long periods of time.

;Update June 24, 2011 - Decided to only load the 5-star events for
;now, from the file tentativeAIAEvents.xls

;Update July 1, 2011
;Procedure for loading data:
;1. First start by just loading 1 minute of data for all events and
;storing it (batch_load_aia_events_small.pro)
;2. Restore the data for each event, run the automatic ROI detection routine
;(aia_autoselect_subroi), and inspect whether the ROI position is good. 
;The subroi is 1k by 1k pixels. Once the 
;3. Once the subroi has been determined, save it in the coords
;variable for each event.
;4. Then run batch_load_aia_events to load the full data for all events, and only save
;the subrois.
;5. Update the px location of the sun center in the new index.
;6. Convert into a map, save that as well.
;7. Then, repeat this for different wavelengths.

path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
wave=['193','211']


;================================================================
evnum='38'
wav='193'
coords=[-843,-200] ;arcsecond coordinates of event center from the HEK
st='2011/05/29 10:05:00'
et='2011/05/29 10:35:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse



;================================================================
;NOTE: THERE'S NO DATA FOR THIS EVENT AT CFA.
;evnum='37'
;wav='193'
;st='2011/05/11 02:10:00'
;et='2011/05/11 02:40:00'
;print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
;std=strsplit(st,'/ :',/extract)
;savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
;if not keyword_set(inspect) then begin
;    aia_load_event,st,et,coords,wav,index,data,savefile=savefile+'.sav'
;    save,filename=savefile+'_map'+'.sav',map
;endif else begin
;
;endelse


;================================================================
evnum='29'
wav='193'
coords=[832,-632]
st='2011/04/07 11:10:00'
et='2011/04/07 11:40:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse



;================================================================
evnum='22'
wav='193'
coords=[720,-600]
st='2011/03/09 02:28:00'
et='2011/03/09 02:58:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
;    save,filename=savefile+'_map'+'.sav',map
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse



;================================================================
evnum='20'
wav='193'
coords=[-1069,-150]
st='2011/03/08 03:30:00'
et='2011/03/08 04:00:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse



;================================================================
evnum='19'
wav='193'
coords=[812,701]
st='2011/03/07 19:35:00'
et='2011/03/07 19:05:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse




;================================================================
evnum='18'
wav='193'
coords=[-1100,300]
st='2011/03/05 18:20:00'
et='2011/03/05 18:50:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse



;================================================================
evnum='13'
wav='193'
coords=[-1073,0]
st='2011/02/11 12:30:00'
et='2011/02/11 13:00:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse




;================================================================
evnum='11'
wav='193'
coords=[1010,100]
st='2011/02/11 07:40:00'
et='2011/02/11 08:10:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse




;================================================================
evnum='07'
wav='193'
coords=[777,100]
st='2011/01/28 00:45:00'
et='2011/01/28 01:15:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse




;================================================================
evnum='05'
wav='193'
coords=[-955,-50]
st='2011/01/25 11:56:00'
et='2011/01/25 12:26:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
if not keyword_set(inspect) then begin
    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
endif else begin
;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
endelse





;================================================================
;This is a non-event!
;evnum='04'
;wav='193'
;coords=[-957,514]
;st='2011/01/18 03:10:00'
;et='2011/01/18 03:40:00'
;print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
;std=strsplit(st,'/ :',/extract)
;savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
;if not keyword_set(inspect) then begin
;    aia_load_event,st,et,coords,wav,index,data
;    save,filename=savefile+'.sav',index,data  
;endif else begin
;;Then, to inspect the data:
;    restore,savefile+'.sav'
;    newcoords=aia_autoselect_subroi(index[0],coords)
;    subdata=aia_inspect_data(index,data,autoregion=newcoords)
;    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
;    save,filename=savefile+'_subdata'+'.sav',subindex,subdata
;endelse






end
