pro polyfill_process,subdata,subindex,rotation_angle,date,evnum,WAVELENGTH = wavelength, START = start, REVISION_NUM = revision_num, SIMPLE_TIME = simple_time, REPLICA = replica, COLOR_TABLE_TEST = color_table_test, INNER_X = inner_x, DYNAMIC_RANGE = dynamic_range, EXTREME_START_TIME = extreme_start_time, EXTREME_FINAL_TIME = extreme_final_time, EXTREME_START_RADIUS = extreme_start_radius, EXTREME_FINAL_RADIUS = extreme_final_radius, TIME = time, RAD = rad, SAVE_OPTION = save_option
;PURPOSE:
;
; This program takes an array of image data as a function of radius
; and time and plots that data by assembling a grid of polyfill squares. 
; If the time array does not have an even cadence, polyfill should
; draw rectangles instead of squares due to the uneven times.
;
;CATEGORY:
; AIA/Kinematic
;
;INPUTS:
; Subdata: A 2-D array where subdata[t,r] = the data at time t and pixel r
; Subindex: The subindex from the restored data
; Rotation Angle: The angle at which the images were rotated. This is
; for display in the plot only, so it is best if it were given
; relative to the radial axis of rotation.
; Date: Write the date in MMDDYY format. (eg 042313)
; Evnum: Each event has an a unique number. Leave off the initial 'e'
;
;KEYWORDS:
;      Wavelength: The wavelength of the event (Note: There is no default. If
; the keyword is not set, the program will prompt the user to type in
; a wavelength in the terminal.)
;      Start: The start frame of the event
;      Revision_Num: A string of length 3 containing a number to be used in
; the name of the file that will be saved. An integer is also acceptable.
; However, revision_num = 0 is bad. Use revision_num = '000'
;      Simple_Time: If this keyword is set, an even cadence of 12 seconds
; is assumed. If there are bad exposures, this is an unrealistic
; assumption.
;      Replica: If this keyword is set, the program will recognize that the
; revision_num has already been used and will create the next lettered
; copy. For example, if revision_num = '003' and both '003' and '003a'
; exist, the saved file will be called '003b'
;      Color_Table_Test: If this keyword is set, the program will create a
; new directory containing the plots of all 40 color tables.
;      Dynamic_Range: This keyword should be an array of length 2. If it is
; set, the program will use dynamic_range[0] as a minimum and
; dynamic_range[1] as a maximum for assigning colors.
;      Time: If this keyword is set, the time coordinates array can be returned.
;      Rad: If this keyword is set, the radial coordinates array can be returned.
;      Save_Option: If this keyword is set, the program will prompt the
; user whether or not the data should be saved.
;
; Sample Replica Calls:
; polyfill_process, data_thin_wave, data_subindex, data_rotation_angle, data_date, data_evnum, WAVELENGTH = data_wavelength, START = data_start, REVISION_NUM = data_revision_num_str, INNER_X = data_inner_x_index, /REPLICA
;
; polyfill_process, data_thin_wave, data_subindex,
; data_rotation_angle, data_date, data_evnum, WAVELENGTH = 193, START
; = 52, REVISION_NUM = '000', INNER_X = data_inner_x_index, /REPLICA
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Michael Hammer, 07/2013
;

data = subdata ; Make a copy (It never was the full subdata)
index = subindex

if keyword_set(wavelength) then $
   wave_str = strtrim(string(wavelength),1) $
else begin
   wave_str = ''
   read,wave_str,prompt = 'What wavelength is the data in the plot? '
   print,''
endelse

if keyword_set(start) then $
   start_str = strtrim(string(start),1) $
else begin
   start_str = ''
   read,start_str,prompt = 'What is the start frame for the data in the plot? '
   print,''
endelse

frames = n_elements(data[*,0]) ; The time coordinates are dim = 1
if keyword_set(SIMPLE_TIME) then begin
   dt = 12.0 ; time cadence in seconds
   time = findgen(frames)*dt ; time array
endif else begin
   dt = fltarr(frames-1) 
   time = fltarr(frames) ; time array (not necessarily constant)
   for i=0,frames-2 do begin
      dt[i] = anytim(index[i+1].date_obs) $
              - anytim(index[i].date_obs)
      time[i+1] = time[i] + dt[i]
   endfor
   print, dt
   print, time
endelse

; This is commented out because it doesn't work properly.
;coords=[index[0].arx0,index[0].ary0]
;substart=[index[0].subroi_x0,index[0].subroi_y0]
;ar_coords=aia_get_arcoords(index,coords,substart,arlonlat=ar_lonlat)
;longitude = ar_lonlat[0]
;geometric_factor = 1.0/sin(longitude*!PI/180.0)

geometric_factor = 1.0

RSUN_PIXELS = median(index.imscl_mp / index.rsun_obs) ; pixels in Rsun
num_pixels = n_elements(data[0,*]) ; The radial coordinates are dim = 2

; Warning: geomtric_factor could be NaN, which is bad
if geometric_factor eq geometric_factor then begin
   print, 'Geometric correction factor has been applied'
   rad = (findgen(num_pixels)*RSUN_PIXELS + 1.0) $
        *geometric_factor ; (Distance to Limb + 1 Solar Radii) * correction
endif else $
   rad = findgen(num_pixels)*RSUN_PIXELS + 1.0 ; geometric_factor = NaN

; Plot from inner_x to end only
if keyword_set(inner_x) then begin
   rad = rad[inner_x:*]
   data = data[*,inner_x:*]
   num_pixels = n_elements(data[0,*])
endif

; Get min & max for color table
if keyword_set(DYNAMIC_RANGE) then begin
   min = dynamic_range[0] ; Pre-set
   max = dynamic_range[1]
endif else begin
   if not keyword_set(EXTREME_START_TIME) then extreme_start_time = 0
   if not keyword_set(EXTREME_FINAL_TIME) then extreme_final_time = 10000
   if not keyword_set(EXTREME_START_RADIUS) then extreme_start_radius = 1.0
   if not keyword_set(EXTREME_FINAL_RADIUS) then extreme_final_radius = 2.5

   ; Note: 't#' is a dummy variable. The parameter is the return value.
   t1 = min(abs(time - extreme_start_time), start_frame)
   t2 = min(abs(time - extreme_final_time), final_frame)
   
   t3 = min(abs(rad - extreme_start_radius), start_r)
   t4 = min(abs(rad - extreme_final_radius), final_r)
   
   min = min(data[start_frame:final_frame,start_r:final_r])
   max = max(data[start_frame:final_frame,start_r:final_r])
endelse

;print, min,max

loadct,0,/silent ; Blue & Gold Color Table
tvlct,rr,gg,bb,/get
;tvlct,reverse(rr),reverse(gg),reverse(bb)

plot_title = 'Wave Tracker for ' + 'e' + evnum + ' at ' + wave_str + $
             ': Rotated ' + strtrim(string(rotation_angle),1) + ' degrees'

wdef,0,1000,600
!P.position=[0.1,0.14,0.95,0.92]
!P.font=-1
plot,time,rad,yrange=[min(rad),max(rad)],xrange=[0.0,max(time)],$
     /xs,/ys,background=255,color=0,/nodata,charsize=2,$
     xtitle='Seconds since frame '+start_str+' at '+index[0].date_obs,ytitle='Radial distance, R!Dsun!N',$
     title=plot_title

ii = 16 ; Select One Color Table
jj = ii

if keyword_set(COLOR_TABLE_TEST) then begin
   ii = 0
   jj = 39
endif

for a = ii,jj do begin

loadct,a,/silent
tvlct,rr,gg,bb,/get

for t=0,frames-2 do begin
   xmin=time[t] ; Left x-cor
   if t eq frames-1 then xmax=time[t] + dt else xmax=time[t+1]  ; Right x-cor
   for r=0,num_pixels-2 do begin
      ymin=rad[r] ; Bottom y-cor            ; Below: Top y-cor
      if r eq num_pixels-1 then ymax=rad[r] + RSUN_PIXELS $
      else ymax=rad[r+1]
      
      color=255.0*(data[t,r]-min)/(max-min) ; Scale Colors
      if color lt 0 then color = 0
      if color gt 255 then color = 255
      ;print,color	
      ;stop
      ;print, xmax-xmin, ymax-ymin
      polyfill,[xmin,xmax,xmax,xmin],[ymin,ymin,ymax,ymax],color=color,/data
      ;print, xmin,xmax,ymin,ymax
   endfor
   ;stop
endfor

im=tvrd(true=1)
if keyword_set(COLOR_TABLE_TEST) then begin
   write_png,'~/ColorTablesOmega/CT'+strtrim(string(a),1)+'.png',im,rr,gg,bb
endif else begin
   scratch = '/Volumes/Scratch/Users/mhammer/'
   year = '20' + strmid(date,4,2)
   save_path = scratch + 'Polyfill_Plots/' + year + '/e' + evnum + '/'
   if not keyword_set(REVISION_NUM) then begin
      next_revision = 1         ; Boolean
      revision_num = 0
      while next_revision eq 1 do begin
         revision_num_str = string(revision_num, FORMAT = '(I03)')
         files = file_search(save_path + '*' + wave_str + '*' $
                             + revision_num_str + '*', $
                             count = count)
         if count gt 0 then begin
            next_revision = 0
         endif else begin
            revision_num += 1
         endelse
      endwhile
      save_name = 'tracker_' + evnum + '_' + wave_str + '_'
      save_num = 'r' + revision_num_str + '.png'
      save_filename = save_path + save_name + save_num 
      print, save_filename
      stop
      write_png,save_filename,im,rr,gg,bb
      print, 'Polyfill Plot Saved'
      ; Also Save Plot Data
      ;save_name = 'plot_data_' ;+ iteration + '_'
      ; TBD if this is necessary
   endif else begin
      if revision_num gt 999 then revision_num = 0
      revision_num_str = string(revision_num, FORMAT = '(I03)')
      save_name = 'tracker_' + evnum + '_' + wave_str + '_'
      if keyword_set(REPLICA) then begin
         letters = ['a','b','c','d','e','f','g','h','i','j','k','l','m',$
                    'n','o','p','q','r','s','t','u','v','w','x','y','z',$
                    'A','B','C','D','E','F','G','H','I','J','K','L','M',$
                    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
         next_revision = 1 ; Boolean
         revision_letter_num = 0
         while next_revision eq 1 do begin
            revision_letter = letters[revision_letter_num]
            files = file_search(save_path + '*' + wave_str + '*' $
                                + revision_num_str $
                                + revision_letter + '*', $
                                count = count)
            if count eq 0 then begin
               next_revision = 0
            endif else begin
               revision_letter_num += 1
            endelse
         endwhile
         save_num = 'r' + revision_num_str + revision_letter + '.png'
      endif else begin
         save_num = 'r' + revision_num + '.png'
      endelse
      save_filename = save_path + save_name + save_num
      print, save_filename
      stop
      write_png,save_filename,im,rr,gg,bb
      print, 'Polyfill Plot Saved'
      ; Also Save Plot Data
      ;save_name = 'plot_data_' ;+ iteration + '_'
      ; TBD if this is necessary
   endelse
endelse

endfor

wait,0.3

end ; EOF
