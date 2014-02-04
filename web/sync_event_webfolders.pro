pro test_sync_event_webfolders
  ;Run for a single event like this:
 one=0
 if one eq 1 then begin
  label='131119_01'
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

pro sync_event_webfolders,event,force=force
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
  subfolders={radio:['NRH','IPS','Callisto'],annulusplot:['araw','abase','arun'],png:['raw','base','run'],dem:['aschwanden','weber']}
  
  
  if size(event,/type) ne 8 then return
  path=event.webpath

  for f=0,n_elements(folders)-1 do begin
     folder=folders[f]
     if not dir_exist(path+folder) then spawn,'mkdir '+path+folder
     
     if folder eq 'kinematics' then begin
        if file_search(path+folder+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.kinematicspath+'*.png '+path+folder
     endif
     
     if folder eq 'pfss' then begin
        if file_search(path+folder+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.pfsspath+'*.png '+path+folder
     endif
     
     if folder eq 'swap' then begin
        if file_search(path+folder+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.swappath+'*.png '+path+folder
     endif
     
     if folder eq 'swap' then begin
        if file_search(path+folder+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.euvipath+'*.png '+path+folder
     endif
     
     if folder eq 'particles' then begin
        if file_search(path+folder+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.particlespath+'*.png '+path+folder
     endif
     
     ;sync the radio subfolders
     if folder eq 'radio' then begin
        for i=0,n_elements(subfolders.radio)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.radio[i]) then $
              spawn,'mkdir '+path+folder+'/'+subfolders.radio[i]

        if file_search(path+folder+'/'+subfolders.radio[0]+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.nrhpath+'*.png '+path+folder+'/'+subfolders.radio[0]
        if file_search(path+folder+'/'+subfolders.radio[1]+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.ipspath+'*.png '+path+folder+'/'+subfolders.radio[1]
        if file_search(path+folder+'/'+subfolders.radio[2]+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.callistopath+'*.png '+path+folder+'/'+subfolders.radio[2]
        
     endif
     
     ;sync the annulusplot subfolders
     if folder eq 'annulusplot' then begin
        if file_search(path+folder+'*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.annuluspath+'*.png '+path+folder
     endif
     
     ;sync the movies subfolders
     if folder eq 'movies' then begin
        if file_search(path+folder+'arun*193*.mp4') eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'arun*193*.mp4 '+path+folder
        if file_search(path+folder+'araw*193*.mp4') eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'araw*193*.mp4 '+path+folder
        if file_search(path+folder+'run*193*.mp4') eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'run*193*.mp4 '+path+folder
        if file_search(path+folder+'raw*193*.mp4') eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'raw*193*.mp4 '+path+folder
        if file_search(path+folder+'aschdem*.mp4') eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'aschdem*.mp4 '+path+folder
        if file_search(path+folder+'pfss_shock*.mp4') eq '' or keyword_set(force) then $
           spawn,'cp '+event.moviepath+'pfss_shock*.mp4 '+path+folder
     endif
     
     ;sync the dem subfolders
     if folder eq 'dem' then begin
        for i=0,n_elements(subfolders.dem)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.dem[i]) then $
              spawn,'mkdir '+path+folder+'/'+subfolders.dem[i]
        if file_search(path+folder+'/'+subfolders.dem[0]+'aschdem*series*.png') eq '' or keyword_set(force) then $
           spawn,'cp '+event.aschdempath+'aschdem*series*.png '+path+folder+'/'+subfolders.dem[0]
        if file_search(path+folder+'/'+subfolders.dem[1]+'aschdem*series*.png') eq '' or keyword_set(force) then $
        spawn,'cp '+event.weberpath+'aschdem*series*.png '+path+folder+'/'+subfolders.dem[1]
     endif
  endfor

print,''
print,'Successfully sync-ed web folders for event '+event.label
print,''
end
