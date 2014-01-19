pro test_sync_event_webfolders
  ;Run for a single event like this:
  ;label='110511_01'
  ;event=load_events_info(label=label)
  ;sync_event_webfolders,event
  
  ;Alternatively, run for all the events:
  events=load_events_info()
  for ev=0,n_elements(events)-1 do begin
     event=events[ev]
     sync_event_webfolders,event
  endfor
end

pro sync_event_webfolders,event
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
        spawn,'cp '+event.kinematicspath+'*.png '+path+folder
     endif
     
     if folder eq 'pfss' then begin
        spawn,'cp '+event.pfsspath+'*.png '+path+folder
     endif
     
     if folder eq 'swap' then begin
        spawn,'cp '+event.swappath+'*.png '+path+folder
     endif
     
     if folder eq 'swap' then begin
        spawn,'cp '+event.euvipath+'*.png '+path+folder
     endif
     
     if folder eq 'particles' then begin
        spawn,'cp '+event.particlespath+'*.png '+path+folder
     endif
     
     ;sync the radio subfolders
     if folder eq 'radio' then begin
        for i=0,n_elements(subfolders.radio)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.radio[i]) then $
              spawn,'mkdir '+path+folder+'/'+subfolders.radio[i]
        spawn,'cp '+event.nrhpath+'*.png '+path+folder+'/'+subfolders.radio[0]
        spawn,'cp '+event.ipspath+'*.png '+path+folder+'/'+subfolders.radio[1]
        spawn,'cp '+event.callistopath+'*.png '+folder+'/'+subfolders.radio[2]
        
     endif
     
     ;sync the annulusplot subfolders
     if folder eq 'annulusplot' then begin
        spawn,'cp '+event.annuluspath+'*.png '+path+folder
     endif
     
     ;sync the movies subfolders
     if folder eq 'movies' then begin
        spawn,'cp '+event.moviepath+'arun*193*.mp4 '+path+folder
        spawn,'cp '+event.moviepath+'araw*193*.mp4 '+path+folder
        spawn,'cp '+event.moviepath+'run*193*.mp4 '+path+folder
        spawn,'cp '+event.moviepath+'raw*193*.mp4 '+path+folder
        spawn,'cp '+event.moviepath+'aschdem*.mp4 '+path+folder
        spawn,'cp '+event.moviepath+'aia_pfss_shock*.mp4 '+path+folder
     endif
     
     ;sync the dem subfolders
     if folder eq 'dem' then begin
        for i=0,n_elements(subfolders.dem)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.dem[i]) then $
              spawn,'mkdir '+path+folder+'/'+subfolders.dem[i]
        spawn,'cp '+event.aschdempath+'aschdem*series*.png '+path+folder+'/'+subfolders.dem[0]
        spawn,'cp '+event.weberpath+'aschdem*series*.png '+path+folder+'/'+subfolders.dem[1]
     endif
  endfor

print,''
print,'Successfully sync-ed web folders for event '+event.label
print,''
end
