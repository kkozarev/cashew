pro test_run_polyfill_process
  wave=['193']
  events=load_events_info()
  label='37'
  event=events[where(events.label eq label)]
  strin=strsplit(event.st,'-:/T .',/extract)
  date=strin[0]+strin[1]+strin[2]
  run_polyfill_process, date, label, wave, PATH = event.savepath,/restore,dynamic_range=[-10,100]
end


pro run_polyfill_process, date, label, wavelength, REVISION_NUM=REVISION_NUM, PATH = path, DYNAMIC_RANGE = dynamic_range, TIME = time, RAD = rad, RESTORE = restore, DATA_THIN_WAVE = data_thin_wave, DATA_SUBINDEX = data_subindex, DATA_ROTATION_ANGLE = data_rotation_angle, DATA_DATE = data_date, DATA_EVNUM = data_evnum, START = data_start, INNER_X = data_inner_x_index
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
; There are two possibilities for running polyfill_process
; (1) You have not restored the data yet.
; If you feel this describes your situation, enter the keyword /RESTORE.
; You must also enter the date, event_num, wavelength, and revision_num
; (2) You have already restored the data.
; In that case, you don't need any of the basic parameters
; (date, event_num, wavelength, revision_num).
; However, you need to enter ALL of the data keywords --- ALL OF THEM!
; Okay, most of them will probably work, not all of them. But, be
; careful if you do not enter all of them. I do not know what could happen.
; 
; Note to self: Create to two (2) more run_polyfill_process programs.
; One of them will assume the data has not been restored. The other will.
;
; 
;DEPENDENCIES:
; polyfill_process
;
;MODIFICATION HISTORY:
;Written by Michael Hammer, 07/2013
;

; Restore Data
if keyword_set(RESTORE) then begin
   ; Set Path
   if not keyword_set(path) then path = '/Volumes/Backscratch/Users/kkozarev/AIA/events/' + label + '/'
   
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
endif

if not keyword_set(DYNAMIC_RANGE) then dynamic_range = [-1000000, 1000000]

if keyword_set(TIME) and keyword_set(RAD) then $
   polyfill_process, data_thin_wave, data_subindex, data_rotation_angle, data_date, label, WAVELENGTH = data_wavelength, START = data_start, REVISION_NUM = data_revision_num_str, INNER_X = data_inner_x_index + 50, DYNAMIC_RANGE = dynamic_range, TIME = time, RAD = rad, EXTREME_START_RADIUS = 1.1, /REPLICA $
else $
   polyfill_process, data_thin_wave, data_subindex, data_rotation_angle, data_date, label, WAVELENGTH = data_wavelength, START = data_start, REVISION_NUM = data_revision_num_str, INNER_X = data_inner_x_index, DYNAMIC_RANGE = dynamic_range, TIME = time, RAD = rad, /REPLICA

end ; EOF
