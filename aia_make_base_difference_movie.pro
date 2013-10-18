pro test_make_difference_movie

wave=['193']
events=load_events_info()
label='37'
event=events[where(events.label eq label)]
sts=event.st
ets=event.et

for w=0,n_elements(wave)-1 do $
make_base_difference_movie,sts,ets,wave[w],coords,label,savepath=savepath

end


pro make_base_difference_movie, sts, ets, wav, coords, label,savepath=savepath
;PURPOSE:
;This procedure will make png files for movies of wave events
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;	st - start time string, format 'YYYY/MM/DD hh:mm:ss'
;	et - ending time string, format 'YYYY/MM/DD hh:mm:ss'
;	wav - wavelength of the AIA channel, string - 94,131,171,193,211,304,335
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 01/2012
  if not keyword_set(savepath) then  savepath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'

  loadct,8,/silent              ; set color table
  wdef,4,1024
  
  aia_load_event,sts,ets,wav,index,data,coords=coords,subdata=subdata,subindex=subindex,/remove_aec,/subroi
  nsteps=n_elements(index)
  
  ;Set up base image
  base = subdata[*,*,0]
  for i=1,4 do begin
     base = base + subdata[*,*,i]
  endfor
  base = base / 5.0
  
  ;Loop over all time steps
  for i=0,nsteps-1 do begin
     ind=strtrim(string(i),2)
     if ind lt 100 then ind='0'+ind
     if ind lt 10 then ind='0'+ind
                                ;im=subdata[*,*,i]*basetime/subindex[i].exptime - base
                                ;scimage=bytscl(im,max=30,min=-50)
     scimage = (bytscl((subdata[*,*,i])-(subdata[*,*,0]), max=30, min=-50)+0)
     tvscl,scimage
     xyouts,0.01,0.97,subindex[i].date_obs+' / '+wav,charsize=3,charthick=3,color=200
     xyouts,0.94,0.97,ind,charsize=3,charthick=3,color=200
     tvlct,rr,gg,bb,/get
     image=tvrd(true=1)
     strin=strsplit(subindex[i].date_obs,'-:/T .',/extract)
     strout=strin[0]+strin[1]+strin[2]+'_'+strin[3]+strin[4]+strin[5]
     infname='normalized_AIA_'strout+'_'+label+'_'+wav
     svfile=savepath+label+'/'
     write_png,svfile+'_subdata_base_'+ind+'.png',image,rr,gg,bb
  endfor
end                             ; EOF
