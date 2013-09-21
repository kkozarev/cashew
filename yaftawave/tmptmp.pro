
filim='/Volumes/PLUME/AIA_data/studies/2011events/normalized_AIA_20110211_13_193'
restore,filim+'.sav'

;aia_radial_filter_v2,index,data,outdata,/radial
;data=outdata

evnum='13'
wav='193'
coords=[-1073,0]
st='2011/02/11 12:30:00'
et='2011/02/11 13:00:00'
std=strsplit(st,'/ :',/extract)

newcoords=aia_autoselect_subroi(index[0],coords)
arcoords=aia_get_arcoords(index[0],coords)
subdata=aia_inspect_data(index,data,autoregion=newcoords)
subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,arcoords=arcoords)
save,filename=filim+'_subdata.sav',subindex,subdata

end
