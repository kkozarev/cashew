pro test_create_event_folders,web=web
  ;Run for a single event like this:
  one=1
  if one eq 1 then begin
     label='130411_01'
     event=load_events_info(label=label)
     create_event_folders,event
     create_event_folders,event,/web
  endif

  ;Alternatively, run for all the events:
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        create_event_folders,event,web=web
     endfor
  endif
end

pro create_event_folders,event,web=web
 ;PURPOSE:
;This procedure will create event folders and subfolders
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
;Written by Kamen Kozarev, 01/2012
  folders=['radio','annulusplot','kinematics','pfss','swap','ionization','particles','png','movies','dem','yaftawave','euvi']
  subfolders={radio:['NRH','IPS','Callisto'],annulusplot:['araw','abase','arun'],png:['raw','base','run'],dem:['aschwanden','weber']}
  

 ; if keyword_set(label) then event=load_events_info(label)
  if size(event,/type) ne 8 then return
  path=event.savepath
  if keyword_set(web) then path=event.webpath
  if not dir_exist(path) then spawn,'mkdir -m 775 '+path


  for f=0,n_elements(folders)-1 do begin
     folder=folders[f]
     if not dir_exist(path+folder) then spawn,'mkdir -m 775 '+path+folder
     
     ;make the radio subfolders
     if folder eq 'radio' then $
        for i=0,n_elements(subfolders.radio)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.radio[i]) then $
              spawn,'mkdir -m 775 '+path+folder+'/'+subfolders.radio[i]
     
     ;make the radio subfolders
     if folder eq 'annulusplot' then $
        for i=0,n_elements(subfolders.annulusplot)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.annulusplot[i]) then $
              spawn,'mkdir -m 775 '+path+folder+'/'+subfolders.annulusplot[i]
     
     ;make the png subfolders
     if folder eq 'png' then $
        for i=0,n_elements(subfolders.png)-1 do $
           if not dir_exist(path+folder+'/'+subfolders.png[i]) then $
              spawn,'mkdir -m 775 '+path+folder+'/'+subfolders.png[i]
     
     ;make the dem subfolders
     if folder eq 'dem' then $
        for i=0,n_elements(subfolders.dem)-1 do $
           if not dir_exist(path+folders[f]+'/'+subfolders.dem[i]) then $
              spawn,'mkdir -m 775 '+path+folders[f]+'/'+subfolders.dem[i]
  endfor

print,''
print,'Successfully created folders for event '+event.label
print,''
end
