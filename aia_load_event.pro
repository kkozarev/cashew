pro aia_load_event,st,et,wave,index,data,savefile=savefile,map=map,subdata=subdata,submap=submap
;PURPOSE:
;This procedure will load full AIA data for a wavelength and time interval,
;then allow the user to select a subregion, and create sub-arrays and submaps
;from that selection. When this is done the data can be easily manipulated.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;	st - start time string, format 'YYYY/MM/DD hh:mm:ss'
;	et - ending time string, format 'YYYY/MM/DD hh:mm:ss'
;	wave - wavelength of the AIA channel, string - 94,131,171,193,211,304,335
;
;KEYWORDS:
; 
;
;OUTPUTS:
;       savefile - a string to hold the output save file
;       (for example, savefile='/Volumes/PLUME/AIA_data/normalized_noprep_0818_3_A193.sav')
;       map - if keyword is set, the map of the data will also be created.
;       subdata - if set, an interactive subroutine will allow the
;                 user to select a subregion
;       submap - if selected, create a submap that corresponds to the subdata.
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/2011
;

;1. Load the data, prep it. Use aia_data_load
if keyword_set(savefile) then begin
   if keyword_set(map) then begin
      aia_load_data,st,et,wave,index,data,savefile=savefile,map=map,/norm
   endif else begin
      aia_load_data,st,et,wave,index,data,savefile=savefile,/norm
   endelse
endif else begin
   if keyword_set(map) then begin
      aia_load_data,st,et,wave,index,data,map=map,/norm
   endif else begin
      aia_load_data,st,et,wave,index,data,/norm
   endelse
endelse

;2. Make 1k data array. Plot 1k movie. Select a subregion (interactive). 
;Convert the selection coords back to 4k coords by simply multiplying them by 4. 
;Write a new procedure to do this, aia_inspect_event. Use/improve roiselect.pro for this.
if keyword_set(subdata) then begin
   aia_inspect_data,index,data,subdata=subdata,/bdiff
endif

if keyword_set(submap) then begin
      if not keyword_set(map) then begin
      print,''
      print,'The /map keyword should be selected! Making map for you...'
      index2map,index,data,map
      aia_inspect_map,map,submap=submap
   endif else begin
      aia_inspect_map,map,submap=submap
   endelse
endif


end
