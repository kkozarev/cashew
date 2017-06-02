pro test_polyfill_process_simple
  wave=['193']
  event=load_events_info(label='test')
  strin=strsplit(event.st,'-:/T .',/extract)
  date=strin[0]+strin[1]+strin[2]
  
  run_polyfill_process_simple, event, wave
end


pro run_polyfill_process_simple, event, wavelength, REVISION_NUM=REVISION_NUM, PATH = path, DYNAMIC_RANGE = dynamic_range, TIME = time, RAD = rad, RESTORE = restore, DATA_THIN_WAVE = data_thin_wave, DATA_SUBINDEX = data_subindex, DATA_ROTATION_ANGLE = data_rotation_angle, DATA_DATE = data_date, DATA_EVNUM = data_evnum, START = data_start, INNER_X = data_inner_x_index
;PURPOSE:
;
; This program runs polyfill_process on restored_data.
; It is assumed that the output is a replica, that is, a plot was
; already made using the given data set.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
; Date: Write the date in MMDDYY format. (eg 042313)
; Event_num: Each event has an a unique number. Leave off the initial 'e'
; Wavelength: The wavelength of the event
; Revision_num: A string of length 3 containing a number to be used in
; the name of the file that will be saved. An integer is also acceptable.
; However, revision_num = 0 is bad. Use revision_num = '000'
;
;KEYWORDS:
; 
;OUTPUTS:
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Michael Hammer, 07/2013
;UPDATE: 10/2013, Kamen Kozarev - adapted for the framework

; Restore Data
   ; Set Path
   if not keyword_set(path) then path=event.savepath
   strin=strsplit(event.st,'-:/T .',/extract)
   date=strin[0]+strin[1]+strin[2]
   label=event.label

   ; Set File Names
   if not keyword_set(REVISION_NUM) then revision_num=0
   revision_num_str = strtrim(string(revision_num),2)
   if revision_num lt 10 then revision_num_str='0'+revision_num_str
   if revision_num lt 10 then revision_num_str='0'+revision_num_str
   
   suffix = + label + '_' + wavelength + '_r' + revision_num_str + '.sav'
   data_file = path + 'jmap_data_' + suffix
   info_file = path + 'jmap_info_' + suffix
   if file_exist(data_file) then begin
      restore, data_file
   endif else begin
      print,''
      print,'File ' + data_file + ' does not exist! Quitting...'
      print,''
   endelse

   if not keyword_set(DYNAMIC_RANGE) then dynamic_range = [-10, 100]

  polyfill_process_simple, date, label, wave, PATH = event.savepath,$
                           savename='jmap_data_'+label+'_'+wavelength+'_r'+revision_num_str,$
                           /restore,dynamic_range=[-10,100]

end ; EOF
