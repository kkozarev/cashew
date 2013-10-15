pro batch_load_additional_events

path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
wave=['335']

for i=0,n_elements(wave)-1 do begin
wav=wave[i]

;================================================================
evnum='32'
coords=[-835,545]
st='2011/04/27 02:05:00'
et='2011/04/27 02:25:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+evnum+'/normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
    newcoords=aia_autoselect_subroi(index[0],coords)
    arcoords=aia_get_arcoords(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)

    save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='38'
coords=[-843,-200] ;arcsecond coordinates of event center from the HEK
st='2011/05/29 10:00:00'
et='2011/05/29 10:30:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+evnum+'/normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

    aia_load_event,st,et,coords,wav,index,data
    save,filename=savefile+'.sav',index,data
    newcoords=aia_autoselect_subroi(index[0],coords)
    arcoords=aia_get_arcoords(index[0],coords)
    subdata=aia_inspect_data(index,data,autoregion=newcoords)
    subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
    save,filename=savefile+'_subdata'+'.sav',subindex,subdata



;================================================================
evnum='06'
coords=[729,-76]
st='2011/01/27 11:50:00'
et='2011/01/27 12:20:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata



;================================================================
evnum='23'
coords=[771,163]
st='2011/03/12 15:20:00'
et='2011/03/12 15:50:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)

save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='20'
coords=[-1069,-150]
st='2011/03/08 03:30:00'
et='2011/03/08 04:00:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='13'
coords=[-1073,-70]
st='2011/02/11 12:30:00'
et='2011/02/11 13:00:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='37'
coords=[785,399]
st='2011/05/11 02:10:00'
et='2011/05/11 02:40:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='19'
coords=[812,750]
st='2011/03/07 19:35:00'
et='2011/03/07 20:05:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata





 endfor

end



pro sklad

;================================================================




;================================================================
evnum='06'
coords=[729,-76]
st='2011/01/27 11:50:00'
et='2011/01/27 12:20:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata



;================================================================
evnum='23'
coords=[771,163]
st='2011/03/12 15:20:00'
et='2011/03/12 15:50:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav

aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)

save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='20'
coords=[-1069,-150]
st='2011/03/08 03:30:00'
et='2011/03/08 04:00:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='13'
coords=[-1073,-70]
st='2011/02/11 12:30:00'
et='2011/02/11 13:00:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='37'
coords=[785,399]
st='2011/05/11 02:10:00'
et='2011/05/11 02:40:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata


;================================================================
evnum='19'
wav='211'
coords=[812,750]
st='2011/03/07 19:35:00'
et='2011/03/07 20:05:00'
print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
std=strsplit(st,'/ :',/extract)
savefile=path+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
aia_load_event,st,et,coords,wav,index,data
save,filename=savefile+'.sav',index,data
newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=savefile+'_subdata'+'.sav',subindex,subdata


end
