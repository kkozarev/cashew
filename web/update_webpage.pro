pro update_webpage, All=all, webfilename=webfilename,_extra=_extra

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
;First update the proper webfolders

events=load_events_info()
path=getenv('CORWAV_WEB')+'events/'

if (~keyword_set(all)) then begin
   for ev=0, n_elements(events)-1 do begin
      event=events[ev]
      if event.web eq 0 then continue
      sync_event_webfolders, event, /force
   endfor
endif else begin
   for ev=0, n_elements(events)-1 do begin
      event=events[ev]
      sync_event_webfolders, event,/force
   endfor
endelse
   
;Now create the website
fname=path+'coronalwaves.content'
if keyword_set(webfilename) then fname=webfilename

if keyword_set(all) then begin
   create_coronalshocks_page, fname, /all
endif else begin
   create_coronalshocks_page, fname
endelse


;Finally, sync the data files into the actual website location. 
;This is CfA specific.
spawn,getenv('CORWAV_WEB')+'../sync_events'

end
