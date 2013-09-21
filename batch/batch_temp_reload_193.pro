pro batch_temp_reload

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

;Update July 18, 2011
;Some of the events' times were not quite appropriate, and
;others did not show anything. This procedure serves to reload those
;corrected times, and add new ones that Kelly and I decided to study.
;In addition, I added a line that imprints a dark mask on the solar
;disk to enhance off-limb features.


path='/Volumes/PLUME/AIA_data/studies/2011events/'
wave=['193','211']

;================================================================
evnum='37'
wav='193'
coords=[785,399]
st='2011/05/11 02:10:00'
et='2011/05/11 02:40:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

    aia_load_event,st,et,wav,index,data
    save,filename=savefile+'.sav',index,data

;Then, to inspect the data:
    restore,savefile+'.sav'
    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)

    save,filename=savefile+'_subdata'+'.sav',subindex,subdata



;================================================================
evnum='35'
wav='193'
coords=[1057,157]
st='2011/05/07 04:00:00'
et='2011/05/07 04:30:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,wav,index,data
save,filename=savefile+'.sav',index,data

restore,savefile+'.sav'
newcoords=aia_autoselect_subroi(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)

;New step - this sets all pixels on the disk to zero
;subdata[*,*,i]=make_circ(subdata[*,*,i],subindex[0].R_sun+9,indices,cent=[subindex[0].x0_mp,subindex[0].y0_mp],val=0)    

save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='27'
wav='193'
coords=[-907,411]
st='2011/03/26 05:10:00'
et='2011/03/26 05:40:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

    aia_load_event,st,et,wav,index,data
    save,filename=savefile+'.sav',index,data

    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata




;================================================================
evnum='19'
wav='193'
coords=[812,750]
st='2011/03/07 19:35:00'
et='2011/03/07 20:05:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

    aia_load_event,st,et,wav,index,data
    save,filename=savefile+'.sav',index,data

    newcoords=aia_autoselect_subroi(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata





;================================================================
evnum='18'
wav='193'
coords=[-1100,350]
st='2011/03/05 18:45:00'
et='2011/03/05 19:15:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,wav,index,data
save,filename=savefile+'.sav',index,data

newcoords=aia_autoselect_subroi(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata






;================================================================
evnum='11'
wav='193'
coords=[1010,100]
st='2011/02/11 07:50:00'
et='2011/02/11 08:20:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,wav,index,data
save,filename=savefile+'.sav',index,data

newcoords=aia_autoselect_subroi(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata



end
