pro test_sync_event_webfolders
  ;Run for a single event like this:
 one=0
 if one eq 1 then begin
  label='130411_01'
  event=load_events_info(label=label)
  sync_event_webfolders,event
 endif

 
  ;Alternatively, run for all the events:
 all=1
 if all eq 1 then begin
  events=load_events_info()
  for ev=0,n_elements(events)-1 do begin
     event=events[ev]
     sync_event_webfolders,event
  endfor
 endif
end

pro sync_event_webfolders,event,force=force, local=local
;PURPOSE:
;This procedure will sync the event webfolders and subfolders
;
;CATEGORY:
; AIA/General
; 
;INPUTS:
;	event - an event structure, returned from load_events(info)
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
;Written by Kamen Kozarev, 01/2014
  folders=['radio','annulusplot','kinematics','pfss','swap','ionization','particles','png','movies','dem','yaftawave','euvi']
  subfolders={radio:['NRH','RSTN','Callisto'],annulusplot:['araw','abase','arun'],png:['raw','base','run'],dem:['aschwanden','weber']}
  usergroup='corwav'
  
  if size(event,/type) ne 8 then return
  
  if keyword_set(local) then begin
     events_path='/Volumes/Scratch/Users/kendrick/web/events/'+event.label+'/'
     path=events_path
  endif else begin
     events_path=getenv('CORWAV_WEB')+'events/'
     path=event.webpath
  endelse 

  if not dir_exist(events_path) then spawn, 'mkdir -m 775 '+events_path
  if not dir_exist(path) then spawn,'mkdir -m 775 '+path
  

  for f=0,n_elements(folders)-1 do begin
     folder=folders[f]
     if not dir_exist(path+folder) then spawn,'mkdir -m 775 '+path+folder
     
     if folder eq 'kinematics' then begin
        files = file_search(path+folder, '*.png')
        if files[0] eq '' or keyword_set(force) then begin
           spawn,'cp '+event.kinematicspath+'*.png '+path+folder
        endif
     endif
     
     if folder eq 'pfss' then begin
        files = file_search(path+folder+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.pfsspath+'*.png '+path+folder
     endif
     
     if folder eq 'swap' then begin
        files = file_search(path+folder+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.swappath+'*.png '+path+folder
     endif
     
     if folder eq 'swap' then begin
        files = file_search(path+folder+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.euvipath+'*.png '+path+folder
     endif
     
     if folder eq 'particles' then begin
        files = file_search(path+folder+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.particlespath+'*.png '+path+folder
     endif

     
     ;sync the radio subfolders
     if folder eq 'radio' then begin
        for i=0,n_elements(subfolders.radio)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.radio[i]) then $
              spawn,'mkdir -m 775 '+path+folder+'/'+subfolders.radio[i]
        
        files = file_search(path+folder+'/'+subfolders.radio[0]+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.nrhpath+'*.png '+path+folder+'/'+subfolders.radio[0]
        files = file_search(path+folder+'/'+subfolders.radio[1]+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.rstnpath+'*.png '+path+folder+'/'+subfolders.radio[1]
        files = file_search(path+folder+'/'+subfolders.radio[2]+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.callistopath+'*.png '+path+folder+'/'+subfolders.radio[2]
        
     endif
     
     ;sync the annulusplot subfolders
     if folder eq 'annulusplot' then begin
        files = file_search(path+folder+'/*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.annuluspath+'*.png '+path+folder
     endif
     
     ;sync the movies subfolders
     if folder eq 'movies' then begin
        files = file_search(path+folder+'/arun*193*.mp4')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'arun*193*.mp4 '+path+folder
        files=file_search(path+folder+'/araw*193*.mp4')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'araw*193*.mp4 '+path+folder
        files=file_search(path+folder+'/run*193*.mp4')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'run*193*.mp4 '+path+folder
        files=file_search(path+folder+'/raw*193*.mp4')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'raw*193*.mp4 '+path+folder
        files=file_search(path+folder+'/aschdem*_teem_map.mp4')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'aschdem*_teem_map.mp4 '+path+folder
        files=file_search(path+folder+'/pfss_shock*.mp4')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'pfss_shock*.mp4 '+path+folder
     endif
     
     ;sync the dem subfolders
     if folder eq 'dem' then begin
        for i=0,n_elements(subfolders.dem)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.dem[i]) then $
              spawn,'mkdir -m 775 '+path+folder+'/'+subfolders.dem[i]
        files = file_search(path+folder+'/'+subfolders.dem[0]+'/aschdem*series*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.aschdempath+'aschdem*series*.png '+path+folder+'/'+subfolders.dem[0]
        files = file_search(path+folder+'/'+subfolders.dem[1]+'/aschdem*series*.png')
        if files[0] eq '' or keyword_set(force) then $
           spawn,'cp '+event.weberpath+'aschdem*series*.png '+path+folder+'/'+subfolders.dem[1]
     endif
     ;Make sure everyone in the user group can write to the folders
     spawn,'chgrp -R '+usergroup+' '+path+'*'
     spawn,'chmod -R ug+rw '+path+'*'
  endfor


print,'' 
print,'Successfully sync-ed web folders for event '+event.label
print,''
end
