pro update_webpage
;PURPOSE:
;This procedure will sync event folders and update the webpage
;
;CATEGORY:
; AIA/General
; 
;INPUTS:
;
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
;Written by Alex Kendrick, 06/2014

;First update all webfolders

events=load_events_info()
for ev=0, n_elements(events)-1 do begin
   event=events[ev]
   sync_event_webfolders, event
endfor

;Now create the website
path=getenv('CORWAV_WEB')+'events/'
fname='coronalwaves.content'
create_coronalshocks_page,path+fname
