pro test_aia_jmap_extract_profiles
  wave=['193']
  event=load_events_info(label='test')
  aia_jmap_extract_profiles, event, WAV = wave
end


pro aia_jmap_extract_profiles, event, PATH = path, $
WAV = wav, START = start, FINISH = finish, $
BAS_INDEX = base_index, RUN = run, BASE = base
;PURPOSE:
; This program rotates the Sun by a user-selected angle
; that is chosen so that the wave is propogating horizontally.
; Ideally, this should place the wave along [*,N/2,*] where N is the
; number of elements in the array (probably 1024).
; In practice, instead of N/2, the y-cor of the active
; region--active_y--will be used as the center of the wave propagation.
; The program then averages the data nearby the center of the
; shockwave to form a two-dimensional array.
; The data averaged will be from [*,N/2-offset,*] to [*,N/2+offset,*]
; where the offset will probably be ~5.
; The pixel values of this data will then be plotted 
; as a function of time.
; If there is a shock wave, it should be appear in the plot in a
; roughly parabolic shape (since it is accelerating).
; The shock wave should be able to be distinguished based on its
; positive relative to other features that appear in the plot and
; evolve over time.
; This parabolic shape will then be fitted to a second order polynomial
; so that velocity and acceleration measurements can be obtained.
;
; Sample Calls:
; ; (0)
; This call is the most basic. No keywords are used.
; aia_jmap_extract_profiles, date, evnum, hemisphere
; Sample 1:
; aia_jmap_extract_profiles, '050113', '0501E', 'E'
; Sample 2:
; aia_jmap_extract_profiles, '080911', '45', 'W'
; (1)
; This call is more practical if you are already familiar with the
; event and know approximately when the event starts and ends.
; aia_jmap_extract, date, evnum, hemisphere, start = *START*, end = *END*
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', start = 50, end = 100
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', start = 30, end = 90
; (2)
; This call is identical to the previous (1). However, in this case, you
; have aleady seen the movie and don't want to see it ever
; again out of anger and hatred. Choose the start and end frames to be
; similar so that when the movie plays initially, you don't
; have to watch too many frames.
; Note: With this option, when prompted to select the start and end
; frames while the program is running, do it. The default options won't 
; help you at all.
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', start = 88, end = 90
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', start = 39, end = 40
; (3)
; This call is basic, except that you don't want the wavelength
; to be 193. You probably want it to be 211, but there are other options.
; aia_jmap_extract, date, evnum, hemisphere, wavelength = *WAVELENGTH*
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', wavelength = 171
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', wavelength = 211
; (4)
; This call is basic, except that there is a problem with frame 0 in
; your data. Thus, you want to choose a different frame to subtract
; when calculating the base difference images.
; aia_jmap_extract, date, evnum, hemisphere, bas_index = *BAS_INDEX*
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', bas_index = 40
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', bas_index = 3
; (5)
; This call is basic, except that you don't like base
; difference images. You want run difference images.
; aia_jmap_extract, date, evnum, hemisphere, /RUN
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', /RUN
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', /RUN
; (6)
; This call is basic, except that you want a strong base image.
; Instead of frame 0, you want an average of 5 frames, 0-4.
; aia_jmap_extract, date, evnum, hemisphere, /BASE
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', /BASE
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', /BASE
; (7)
; This call is identical to the previous except that you want to
; choose a different set of 5 frames to average for the base.
; This can be done by setting bas_index to i. Then, the frames that
; will be averaged are i, i+1, i+2, i+3, & i+4
; aia_jmap_extract, date, evnum, hemisphere, bas_index = *BAS_INDEX*, /BASE
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', bas_index = 48, /BASE
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', bas_index = 5, /BASE
; (8)
; This call is basic, except that you are not on my computer. Thus,
; the path needs to be changed so that it actually exists.
; aia_jmap_extract, date, evnum, hemisphere, path = *PATH*
; Sample 1:
; aia_jmap_extract, '042313', '0423W', 'W', path = '~/Documents/events/'
; Sample 2:
; aia_jmap_extract, '080911', '45', 'W', path = '~/Documents/events/'
; (9)
; All of the calls above are really basic. I have more convoluted
; problems and want to use multiple keywords. Great. Then, do it... :-\
; Sample 1:
; In this example, you are on someone else's computer, want to
; use an averaged base image from 15-19, know that the start frame is
; 25, the end frame is 85, and the wavelength of choice is 211.
; aia_jmap_extract, '042313', '0423W', 'W', start = 25, finish = 85,
; path = '~/Documents/my_events/2013/something/', wavelength = 211,
; bas_index = 15, /BASE
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
; Date: Write the date in MMDDYY format. (eg 042313)
; Evnum: Each event has a unique label.
; Hemisphere: 'E' or 'W', namely east or west
;
;KEYWORDS:
; Path: If the path is not an expected path, include it as a keyword.
; Wavelengths: The default wavelength is '193'. Otherwise, change it.
; A recent modification of this program is to allow for an array of
; different wavelengths. If that is the case, the program will assume
; that the inital wavelength is the one in which the user will select
; their inputs. The program will then carry out the process for
; the remaining wavelengths with those inputs. If you want to select
; inputs separately for different wavelengths, do not enter an array
; of more than one wavelength. 
;
; Start: The first frame to be plotted. The default is 0.
; Finish: The last frame to be plotted. The default is the last frame.
; Bas_Index: This is the frame that will be subtracted from all other
; frames or the initial frame if /base is selected. The default is 0.
; Run: If run is set, a running difference movie will be shown.
; (The default is a base difference with only one image.)
; Base: If base is set, a base difference movie will be created where
; an average of the first 5 frames after base_index will be subtracted.
; (The default is a base difference with only one image.) 
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Michael Hammer, 07/2013
;10/17/2013, KAK - made some changes to integrate into the framework.

date=event.date
label=event.label
hemisphere=event.hemisphere

; Global Variables
RSUN = 6.96e5 ; radius of the Sun in km
!P.thick = 3 ; thickness of lines that will be drawn


; Determine Wavelength
; Possible Wavelengths: 171, 193, 211, 335, 094, 131, 304
if not keyword_set(WAV) then wavelengths = '193' else wavelengths=wav
num_waves = n_elements(wavelengths)
wavelength = wavelengths[0] ; set wavelength

; Determine Path
scratch = getenv('CORWAV')
scratch = scratch+'events/'
year = strmid(date,0,4)
if not keyword_set(PATH) then path=event.savepath


; Set Date: Places date in YYYYMMDD that is used in file names
formatted_date = date

; Restore subdata from .sav file
eventname='AIA_'+formatted_date+'_'+label+'_'+wavelength
print, 'Loading '+wavelength+' channel AIA data for event #'+label
restore,path+'/normalized_'+eventname+'_subdata.sav'

; Make copies of the data
data = subdata
index = subindex
; The above is important since stuff might be deleted.

frames = n_elements(data[0,0,*]) ; # of timesteps
if not keyword_set(start) then start = 0
if not keyword_set(finish) then finish = frames - 1
iterations = finish - start + 1 ; # of timesteps to be plotted

; Set Up Base for Base_0, Base_i, Base_0-4, or Base_i-i+4
if not keyword_set(BAS_INDEX) then base_index = 0
if keyword_set(BASE) then begin
  avg=data[*,*,base_index]
  for i=base_index+1,base_index+4 do $
     avg=avg+data[*,*,i]
  base_image=avg/5.0
endif else base_image = data[*,*,base_index]


; Display images so that user knows which rotation angle to select.
if keyword_set(preview) then begin
   loadct,9,/silent
   wdef,1,1024
   
   print,''
   print,"Here is a preview of the image sequence!"
   print,''
   print,"While you are watching, figure out which frame you want to use " $
         +"to select the rotation angle."
   print,''
   
   rp = 1                       ; If debugging, rp = 1, Else rp = 2
; IDL Style Note: if then else should be on the same line (hence the '$'s)
   if keyword_set(RUN) then $
      play_movie_subroutine,rp,base_image,data,index,wavelength, $
                            START=start,FINISH=finish,RUN=run, /CHECK_EXPOSURES $
   else if keyword_set(BASE) then $
      play_movie_subroutine,rp,base_image,data,index,wavelength, $
                            START=start,FINISH=finish,BASE=base, /CHECK_EXPOSURES $
   else $
      play_movie_subroutine,rp,base_image,data,index,wavelength, $
                            START=start,FINISH=finish, /CHECK_EXPOSURES
endif

   loadct,9,/silent
   wdef,1,1024
   ;user_input=''
   ;print,''
   ;read,user_input,prompt="What frame do you want to use to select the angle of the shockwave? "
   ;print,''
   ;chosen_frame = fix(user_input) ; int_of_string
   chosen_frame=frames/3
   
   
; Display the chosen frame and ask user to select the rotation angle.
if keyword_set(RUN) then $
  play_movie_subroutine,1,base_image,data,index,wavelength, $
  START=chosen_frame,FINISH=chosen_frame,RUN=run $
else if keyword_set(BASE) then $
  play_movie_subroutine,1,base_image,data,index,wavelength, $
  START=chosen_frame,FINISH=chosen_frame,BASE=base $
else $
  play_movie_subroutine,1,base_image,data,index,wavelength, $
  START=chosen_frame,FINISH=chosen_frame

; Draw an outline of the limb (one solar radius)
r_sun = index[0].r_sun
x_c = index[0].X0_MP
y_c = index[0].Y0_MP
circle = aia_circle(x_c,y_c,r_sun,/plot)

print,''
print,'Select a point to mark the active region on the Sun. This point will also help determine the rotation angle.'
print,''

cursor,active_x,active_y,/device,/down
print, 'You selected ' + strtrim(active_x,1) + ' ' + strtrim(active_y,1)

; Draw a radial line through the active region.
aia_oplot_radial,index,[active_x,active_y],hemisphere

;DEBUG - KAK 11/29/2013
;print,''
;read,num_angles_str, $
;     prompt = 'How many other non-radial angles on the shock wave do you want to select? '
;print,''
;num_angles = fix(num_angles_str)
num_angles=0
;DEBUG

num_points = 2*num_angles+1
; Initialize Arrays
wavepoint_x = fltarr(num_points)
wavepoint_y = fltarr(num_points)
wave_angle = fltarr(num_points)

for i=0,num_angles do begin
   j = i+1 ; j is the number variable here. i is the index variable.
   i_str = strtrim(string(i),1)
   num_points_str = strtrim(string(num_points),1)
   num_angles_str = strtrim(string(num_angles),1)
 ;  print, ''
 ;  print, 'Select a point on the shock wave through which to determine the rotation angle. This will be angle ' + i_str + ' out of ' + num_angles_str + ' angles that you will select.'
 ;  print, ''
 ;  if i eq 0 then begin
 ;     print, 'NOTE: Since this is your 0th angle, select the radial direction. Do it regardless of any reason not to!'
 ;     print, ''
 ;  endif else begin
 ;     print, 'NOTE: Since this is NOT your 0th angle, an angle symmetric to the one you selected will also be selected. This angle will pass through the selected point after it has been reflected over the radial axis.'
  ;    print,''
  ; endelse
   
   ;cursor,x,y,/down,/device
   x=active_x
   y=active_y
   x_str = string(x)
   y_str = string(y)
   ;print, 'You selected ' + strtrim(x_str,1) + ' ' + strtrim(y_str,1)
   if i eq 0 then begin
      wavepoint_x[0] = active_x
      wavepoint_y[0] = active_y
   endif else begin
      wavepoint_x[2*i-1] = x
      wavepoint_y[2*i-1] = y
   endelse

   ; Draw Lines through Points
   ;plots, [x,active_x], [y,active_y], /device, color = 55 
   ;rad_angle = atan(float(y - active_y)/float(x - active_x))
   rad_angle=atan((y_c-active_y)/(x_c-active_x))
   ;rad_angle = atan(float(0 - active_y)/float(0 - active_x))
   angle = round(rad_angle * 180. /!PI)
   ;print, 'This yields an angle of ' + strtrim(angle,1) + ' degrees'
   ;print, 'In radians, this is ' + strtrim(rad_angle,1) 
   
   
   
   if i eq 0 then begin
      wavepoint_x[0] = x
      wavepoint_y[0] = y
      wave_angle[0] = angle
      radial_rotation_angle = angle
      ; This marks the angle at which the wave is propagating radially.
      radial_x = x
      radial_y = y
      ; This marks the x-cor and y-cor of some point in the radial direction.
   endif else begin
      wavepoint_x[2*i-1] = x
      wavepoint_y[2*i-1] = y
      wave_angle[2*i-1] = angle ; in degrees, NOT radians

      ; Mark the symmetric angle reflected over the radial axis.
      delta_angle = angle - radial_rotation_angle
      sym_angle = radial_rotation_angle - delta_angle
      sym_rad_angle = sym_angle * !pi / 180
      wave_angle[2*i] = sym_angle

      d_squared = (x - active_x)^2 + (y - active_y)^2
      d = sqrt(d_squared) ; distance to active region

      if hemisphere eq 'W' then begin
         sym_x = active_x + d*cos(sym_rad_angle)
         sym_y = active_y + d*sin(sym_rad_angle)
      endif else begin
         sym_x = active_x - d*cos(sym_rad_angle)
         sym_y = active_y - d*sin(sym_rad_angle)
      endelse

      ; Draw Lines through Points
      plots, [sym_x,active_x], [sym_y,active_y], /device, color = 55 
      
      print, 'The symmetric angle is ' + strtrim(sym_angle,1) + ' degrees'
      print, 'In radians, this is ' + strtrim(sym_rad_angle,1)

      wavepoint_x[2*i] = sym_x
      wavepoint_y[2*i] = sym_y
      wave_angle[2*i] = sym_angle
   endelse
endfor


; Calculate rotation angles relative to the radial rotation angle
adjusted_wave_angle = wave_angle - radial_rotation_angle

print,''
display = ''
read,display,prompt='Do you want to see the rotated images? (y/n) '

print,''
plot_start_frame = ''
read,plot_start_frame,prompt='At what frame do you want to begin? (d = default start/*Frame Number*) '
print,''
plot_end_frame = ''
read,plot_end_frame,prompt='At what frame do you want to end? (d = default end/*Frame Number*) '

if plot_start_frame eq 'd' then plot_start_frame = start $
else plot_start_frame = fix(plot_start_frame)
if plot_end_frame eq 'd' then plot_end_frame = finish $
else plot_end_frame = fix(plot_end_frame)

; Save coordinates of active region.
; They will updated to reflect the larger dimensions of the rotation.
tmp_active_x = active_x
tmp_active_y = active_y

for w=0,num_waves-1 do begin ; Waves Loop
wavelength = wavelengths[w]

if w ne 0 then begin ; If w ne 0, then the data has not been loaded yet.
   ; Restore subdata from .sav file
   eventname='AIA_'+formatted_date+'_'+label+'_'+wavelength
   print, 'Loading '+wavelength+' channel AIA data for event #'+label
   restore,path+'e'+label+'/normalized_'+eventname+'_subdata.sav'

   ; Make copies of the data
   data = subdata
   index = subindex
   ; The above is important since stuff might be deleted.

   ; A different base image must be used.
   if not keyword_set(BAS_INDEX) then base_index = 0
   if keyword_set(BASE) then begin
      avg=data[*,*,base_index]
      for k=base_index+1,base_index+4 do $
         avg=avg+data[*,*,k]
      base_image=avg/5.0
   endif else base_image = data[*,*,base_index]
endif

; Save temporary copies (This is relevant for num_points > 1)
tmp_data = data
tmp_index = index

active_x_init = tmp_active_x
active_y_init = tmp_active_y

; Rotate the Sun! Make Plots!
for i=0,num_points-1 do begin ; Points Loop
  ; Restore temporary copies
  data = tmp_data
  index = tmp_index

  angle_str = strtrim(string(wave_angle[i]),1)
  print, "The image has been rotated by " + angle_str + " degrees."

  ; If the event is on the east limb, reflect the image.
  ; This will happen only in the final plot.
  if hemisphere eq 'E' then reflector = 1 else reflector = 0

  ; If the user doesn't want to see the rotated images, then
  ; you should still show one image so that the user can select
  ; an x-cor and a y-cor to use in the plot

  if display eq 'n' then begin
     r_start = chosen_frame
     r_finish = chosen_frame
  endif else begin
     r_start = plot_start_frame
     r_finish = plot_end_frame
  endelse

  ; Increase spatial array by 150% in x-direction and 50% in y-direction
  s_x = n_elements(data[*,0,0])
  s_y = n_elements(data[0,*,0])
  s_z = n_elements(data[0,0,*])
  e_x = (s_x * 2) ;+ (s_x / 2)
  e_y = (s_y * 2) - (s_y / 2)
  e_z = s_z
  expanse = intarr(e_x,e_y,e_z)
  base_ex = intarr(e_x,e_y)

  i_x = (e_x - s_x) / 2
  i_y = (e_y - s_y) / 2
  i_z = 0
  f_x = i_x + s_x - 1
  f_y = i_y + s_y - 1
  f_z = s_z - 1

  print, s_x, e_x, i_x, f_x
  print, s_y, e_y, i_y, f_y

  ; Copy subdata into expanse array
  expanse[i_x:f_x,i_y:f_y,i_z:f_z] = data
  base_ex[i_x:f_x,i_y:f_y] = base_image
  ; Since the expanse array is very large, ideally no data will be lost.
  active_x = active_x_init + i_x
  active_y = active_y_init + i_y

  ; Set up window
  loadct,9,/silent
  wdef,0,e_x ; Set window dimension to length of expanse array

  print, 'Prepare to delete data'

  if keyword_set(RUN) then $
     play_movie_subroutine,1,base_ex,expanse,index,wavelength,imgs, $
     good_exposures, $
     START=r_start,FINISH=r_finish,RUN=run, $
     ROTATION_ANGLE=wave_angle[i], $
     CENTER_X = active_x, CENTER_Y = active_y, /CHECK_EXPOSURES, $
     /PERMANENT $
  else if keyword_set(BASE) then $
     play_movie_subroutine,1,base_ex,expanse,index,wavelength,imgs, $
     good_exposures, $
     START=r_start,FINISH=r_finish,BASE=base, $
     ROTATION_ANGLE=wave_angle[i], $
     CENTER_X = active_x, CENTER_Y = active_y, /CHECK_EXPOSURES, $
     /PERMANENT $
  else $
     play_movie_subroutine,1,base_ex,expanse,index,wavelength,imgs, $
     good_exposures, $
     START=r_start,FINISH=r_finish, $
     ROTATION_ANGLE=wave_angle[i], $
     CENTER_X = active_x, CENTER_Y = active_y, /CHECK_EXPOSURES, $
     /PERMANENT

  ; If the movie was not displayed, the rotated data still needs to be gathered
  if r_start eq r_finish then begin
    r_start = plot_start_frame
    r_finish = plot_end_frame
    if keyword_set(RUN) then $
      play_movie_subroutine,1,base_ex,expanse,index,wavelength,imgs, $
      good_exposures, $
      START=r_start,FINISH=r_finish,RUN=run, $
      ROTATION_ANGLE=wave_angle[i], $
      CENTER_X = active_x, CENTER_Y = active_y, /HIDE_MOVIE, /CHECK_EXPOSURES, $
      /PERMANENT $
    else if keyword_set(BASE) then $
      play_movie_subroutine,1,base_ex,expanse,index,wavelength,imgs, $
      good_exposures, $
      START=r_start,FINISH=r_finish,BASE=base, $
      ROTATION_ANGLE=wave_angle[i], $
      CENTER_X = active_x, CENTER_Y = active_y, /HIDE_MOVIE, /CHECK_EXPOSURES, $
      /PERMANENT $
    else $
      play_movie_subroutine,1,base_ex,expanse,index,wavelength,imgs, $
      good_exposures, $
      START=r_start,FINISH=r_finish, $
      ROTATION_ANGLE=wave_angle[i], $
      CENTER_X = active_x, CENTER_Y = active_y, /HIDE_MOVIE, /CHECK_EXPOSURES, $
      /PERMANENT
  endif

  ; Draw horizontal reference line
  middle = n_elements(expanse[0,*,0])/2
  plots, [0, n_elements(expanse[*,0,0])-1], $
         [middle, middle], $
         /device, color = 55

  ;print,''
  ;print, 'Select an x-cor to indicate one solar radius'
  ;cursor,x,y,/down,/device
  plot_oneRSUN = abs(x_c)
  print, '1 Solar Radius: ' + strtrim(string(plot_oneRSUN),1)+' px'
  

  ;print,''
  plot_inner_x='n'
  ;read,plot_inner_x,prompt='Do you want to select an innermost x-position for the plot? (y/n) '
  ;print,''

  if plot_inner_x eq 'n' then plot_inner_x = plot_oneRSUN ;$
  ;else begin
  ;   print, 'Select an innermost x-position to start the plot'
  ;   cursor,x,y,/down,/device
  ;   plot_inner_x = x
  ;   print, 'Inner x: ' + strtrim(string(x),1)
  ;endelse
  
  ;print,''
  plot_outer_x='n'
  ;read,plot_outer_x,prompt='Do you want to select an outermost x-position for the plot? (y/n) '
  ;print,''

  if plot_outer_x eq 'n' then begin
     if hemisphere eq 'W' then plot_outer_x = n_elements(expanse[*,0,0]) - 1 $
     else if hemisphere eq 'E' then plot_outer_x = 0
  endif ;else begin
     ;print, 'Select an outermost x-position to start the plot'
     ;cursor,x,y,/down,/device
     ;plot_outer_x = x
     ;print, 'Outer x: ' + strtrim(string(x),1)
  ;endelse

  wait,1.0 ; Pause so that you have time to see the rotation
  offset = 5
  ; For 1-D shock wave array, use the y-cor of the active region, not N/2
  ; Actually, N/2 is the new y-cor of the active region
  shock_y = middle

  ; If hemisphere eq 'E' then that is bad since outer_x < inner_x
  if plot_outer_x ge plot_oneRSUN then begin
     plot_left_x = plot_oneRSUN
     plot_right_x = plot_outer_x
     plot_inner_x_index = plot_inner_x - plot_oneRSUN
     if plot_inner_x_index lt 0 then plot_inner_x_index = 0
  endif else begin
     plot_left_x = plot_outer_x
     plot_right_x = plot_oneRSUN
     plot_inner_x_index = plot_oneRSUN - plot_inner_x
     if plot_inner_x_index lt 0 then plot_inner_x_index = 0
  endelse

  ; With the bad exposures deleted, the frame indices need to be updated.
  t1 = min(abs(good_exposures - plot_start_frame), plot_start_frame_i)
  t2 = min(abs(good_exposures - plot_end_frame), plot_end_frame_i)

  thick_wave = imgs[plot_left_x:plot_right_x,shock_y-offset:shock_y+offset,$
                   plot_start_frame_i:plot_end_frame_i]
  index = index[plot_start_frame_i:plot_end_frame_i]
  ; x-cors: left_x to right_x
  ; y-cors: wave + or - offset
  ; t-cors: start_frame to end_frame
  if reflector eq 0 then $
     thin_wave = transpose(average(thick_wave,2)) $
     ; should average over the y-axis
  else $
     thin_wave = reverse(transpose(average(thick_wave,2)),2)

  ; Identify the revision number to save the files
  wave_str = strtrim(string(wavelength),1)
  search_path = path
  next_revision = 1 ; Boolean
  revision_num = 0
  revision_num_str = '000'
  while next_revision eq 1 do begin
     files = file_search(search_path + '*' + wave_str + '*' $
                         + revision_num_str + '*', $
                         count = count)
     if count eq 0 then begin
        next_revision = 0
     endif else begin
        revision_num += 1
        revision_num_str = string(revision_num, FORMAT = '(I03)')
     endelse
  endwhile

  print,revision_num_str
  
  ; Save Data so that it can be input to polyfill_process directly
  ; Be careful not to use repeat names
  ; This names should be consistent as they probably will be restored
  data_thin_wave = thin_wave
  data_subindex = index
  data_rotation_angle = adjusted_wave_angle[i]
  data_date = date
  data_label = label
  data_inner_x_index = plot_inner_x_index
  data_start = plot_start_frame_i
  data_wavelength = wavelength
  data_revision_num_str = revision_num_str

  ; Save the duplicate variables
  save_path = path+'kinematics/'
  save_name = 'jmap_data_' + label + '_' + wave_str ; + '_'
  ;save_num = 'r' + revision_num_str + '.sav'
  save_num = '.sav'
  save_filename = save_path + save_name + save_num
  save,data_thin_wave,data_subindex,data_rotation_angle,$
       data_date,data_label,data_inner_x_index,data_start,$
       data_wavelength,data_revision_num_str,$
       filename = save_filename
  
  ; Save Information so that user knows what is in the plot
  ; Be careful not to use repeat names
  ; These names are for reference and probably will not be restored
  active_region_x = active_x_init
  active_region_y = active_y_init
  plotted_frame_initial = plot_start_frame
  plotted_frame_final = plot_end_frame
  plotted_frame_initial_index = plot_start_frame_i
  plotted_frame_final_index = plot_end_frame_i
  angle_of_rotation = wave_angle[i]
  angle_of_rotation_to_radial_axis = adjusted_wave_angle[i]
  selected_point_x = wavepoint_x[i]
  selected_point_y = wavepoint_y[i]
  plot_x_one_solar_radius = plot_oneRSUN
  plot_x_innermost = plot_inner_x
  plot_x_outermost = plot_outer_x
  good_exposure_indices = good_exposures
  
  ; Save the duplicate variables
  info_path = save_path
  info_name = 'jmap_info_' + label + '_' + wave_str ; + '_'
  ;info_num = 'r' + revision_num_str + '.sav'
  info_num = '.sav'
  info_filename = info_path + info_name + info_num
  save,active_region_x,active_region_y,$
       plotted_frame_initial,plotted_frame_final,$
       plotted_frame_initial_index, plotted_frame_final_index,$
       angle_of_rotation, angle_of_rotation_to_radial_axis, $
       selected_point_x,selected_point_y,$
       plot_x_one_solar_radius,$
       plot_x_innermost,plot_x_outermost,$
       good_exposure_indices,$
       filename = info_filename
  print, 'J-map Data and Info have been saved'
  
 ; print,''
 ; print,'Next, you should probably run run_polyfill_process'
 ; polyfill_process_mhammer, thin_wave, index, adjusted_wave_angle[i], date, label, $
 ;                   wavelength = wavelength, $
 ;                   start = plot_start_frame_i, $
 ;                  revision_num = revision_num_str, $
 ;                   inner_x = plot_inner_x_index

endfor ; Points Loop

endfor ; Wavelength Loop

end ; EOF
