pro test_aia_make_images
;Test the procedure aia_make_images
  
  ;You can run this for a single or few events, like so
  one=0
  if one eq 1 then begin
     wavelengths=['193','211']
     label=['131107_01','131119_01']
     events=load_events_info(label=label)
     for w=0,n_elements(wavelengths)-1 do for ev=0,n_elements(events)-1 do $
        aia_make_images,events[ev],wavelengths[w],/base,/run;,/raw
  endif
  
  ;Alternatively, run it for all events
  all=1
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     nevents=n_elements(events)
     for ev=0,nevents-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_make_images,event,wavelength,/raw,/base,/run,/force
        endfor
     endfor
  endif
end



pro aia_make_images, event, wave, savepath=savepath,force=force,raw=raw,base=base,run=run
;PURPOSE:
;This procedure will make png image files for movies of wave events
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;	event - the event structure returned by load_events_info()
;	wav - wavelength of the AIA channel, string - 94,131,171,193,211,304,335
;
;KEYWORDS:
; savepath - where to save the images
; raw - make raw, inverted images
; base - make base difference images
; run - make running difference images
; force - force the program to overwrite images
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 11/2013


  print,''
  print,'Making images for event '+event.label
  print,''

  if not keyword_set(savepath) then  savepath=event.savepath+'png/'
  wav=wave
  coords=[event.coordX,event.coordY]
  label=event.label
  sts=event.st
  ets=event.et
  date=event.date
  
  ;The default type is 'raw'
  image_type=''
  if keyword_set(raw) then begin
     imgtyp='raw'
     image_type=[image_type,imgtyp]
     infname='normalized_AIA_'+date+'_'+event.label+'_'+wav+'_subdata_'+imgtyp+'*.png'
     if file_exist(savepath+infname) and not keyword_set(force) then begin
        print,''
        print,'These files exist. To overwrite, rerun with /force. Quitting...'
        print,''
        return
     endif
  endif
  if keyword_set(base) then begin
     imgtyp='base'
     image_type=[image_type,imgtyp]
     infname='normalized_AIA_'+date+'_'+event.label+'_'+wav+'_subdata_'+imgtyp+'*.png'
     if file_exist(savepath+infname) and not keyword_set(force) then begin
        print,''
        print,'These files exist. To overwrite, rerun with /force. Quitting...'
        print,''
        return
     endif
  endif
  if keyword_set(run) then begin
     imgtyp='run'
     image_type=[image_type,imgtyp]
     if file_exist(savepath+infname) and not keyword_set(force) then begin
        print,''
        print,'These files exist. To overwrite, rerun with /force. Quitting...'
        print,''
        return
     endif
  endif
  if image_type[0] eq '' and n_elements(image_type) eq 1 then begin
     print,''
     print,'Please rerun with type of image as keyword - /raw, /base, or /run . Quitting...'
     print,''
     return
  endif
  image_type=image_type[1:*]
  

  
  ;Load the data
  ;restore,event.savepath+'normalized_AIA_20110511_37_193_subdata.sav'
  aia_load_event,sts,ets,wav,index,data,coords=coords,subdata=subdata,subindex=subindex,/remove_aec,/subroi,event=event
  nsteps=n_elements(subindex)
  
  ;Set up base image
  base = subdata[*,*,0]
  if nsteps ge 5 then begin
     for i=1,4 do begin
        base = base + subdata[*,*,i]
     endfor
     base = base / 5.0
  endif
  
  
  set_plot,'z'
  device, set_resolution=event.aiafov, SET_PIXEL_DEPTH=24, DECOMPOSED=0
  !P.font=0
  text_scaling=avg(event.aiafov)/1024.0
  chsize=1.2*text_scaling
  chthick=1.0*text_scaling

  for it=0,n_elements(image_type)-1 do begin
     imgtype=image_type[it]
     ;Loop over all time steps
     for i=1,nsteps-1 do begin     
        ind=strtrim(string(i),2)
        if ind lt 100 then ind='0'+ind
        if ind lt 10 then ind='0'+ind
        
        if imgtype eq 'raw' then begin
           loadct,0,/silent
           tvlct,rr,gg,bb,/get
           tvlct,reverse(rr),reverse(gg),reverse(bb)
           scimage = (bytscl(sqrt(subdata[*,*,i]), max=50, min=0)+0)
           tvscl,scimage
           polyfill,[0.0,0.315*text_scaling,0.315*text_scaling,0.0],[0.98,0.98,1.0,1.0],color=255,/norm
           polyfill,[0.968-(1.0-0.968)/text_scaling,1.0,1.0,0.968-(1-0.968)/text_scaling],[0.98,0.98,1.0,1.0],color=255,/norm
           xyouts,0.005,0.985,subindex[i].date_obs+' / AIA:'+wav,charsize=chsize,charthick=chthick,color=0,/norm
           xyouts,0.970-(1.-0.97)/text_scaling,0.985,ind,charsize=chsize,charthick=chthick,color=0,/norm
           image=tvrd(/true)
           loadct,0,/silent
        endif
        
        if imgtype eq 'base' then begin
           loadct,0,/silent     ; set color table
           tvlct,rr,gg,bb,/get
           scimage = (bytscl((subdata[*,*,i])-base, max=30, min=-50)+0)
           tvscl,scimage
           polyfill,[0.0,0.315*text_scaling,0.315*text_scaling,0.0],[0.98,0.98,1.0,1.0],color=0,/norm
           polyfill,[0.968-(1.0-0.968)/text_scaling,1.0,1.0,0.968-(1-0.968)/text_scaling],[0.98,0.98,1.0,1.0],color=0,/norm
           xyouts,0.005,0.985,subindex[i].date_obs+' / AIA:'+wav+' B',charsize=chsize,charthick=chthick,color=255,/norm
           xyouts,0.970-(1.-0.97)/text_scaling,0.985,ind,charsize=chsize,charthick=chthick,color=255,/norm
           image=tvrd()
           loadct,0,/silent
        endif
        
        if imgtype eq 'run' then begin
           loadct,0,/silent              ; set color table
           tvlct,rr,gg,bb,/get
           scimage = (bytscl((subdata[*,*,i])-(subdata[*,*,i-1]), max=30, min=-50)+0)
           tvscl,scimage
           polyfill,[0.0,0.315*text_scaling,0.315*text_scaling,0.0],[0.98,0.98,1.0,1.0],color=0,/norm
           polyfill,[0.968-(1.0-0.968)/text_scaling,1.0,1.0,0.968-(1-0.968)/text_scaling],[0.98,0.98,1.0,1.0],color=0,/norm
           xyouts,0.005,0.985,subindex[i].date_obs+' / AIA:'+wav+' R',charsize=chsize,charthick=chthick,color=255,/norm
           xyouts,0.970-(1.-0.97)/text_scaling,0.985,ind,charsize=chsize,charthick=chthick,color=255,/norm
           image=tvrd()
           loadct,0,/silent              ; set color table
        endif

        finsavpath=savepath+imgtype+'/'+wav+'/'
        if not dir_exist(finsavpath) then spawn,'mkdir '+finsavpath   
        infname='normalized_AIA_'+date+'_'+event.label+'_'+wav+'_subdata_'+imgtype+'_'+ind+'.png'
        write_png,finsavpath+infname,image,rr,gg,bb
       ;stop
     endfor
  endfor
set_plot,'x'
!P.font=0
end; EOF

           ;polyfill,[0.0,0.4,0.4,0.0],[0.96,0.96,1.0,1.0],color=0
           ;polyfill,[0.93,1.0,1.0,0.93],[0.96,0.96,1.0,1.0],color=0
           ;xyouts,0.01,0.97,subindex[i].date_obs+' / AIA:'+wav+' R',charsize=3,charthick=3,color=255
           ;xyouts,0.94,0.97,ind,charsize=3,charthick=3,color=255
