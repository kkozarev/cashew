pro play_movie_subroutine, num, base_image, subdata, subindex, wavelength, img_array, good_exposures, START = start, FINISH = finish, RUN = run, BASE = base, ROTATION_ANGLE = rotation_angle, REFLECT = reflect, CENTER_X = center_x, CENTER_Y = center_y, HIDE_MOVIE = hide_movie, CHECK_EXPOSURES = check_exposures, PERMANENT = permanent
;PURPOSE:
; Displays movies of a specified type
;
;CATEGORY:
; AIA/General
;
;INPUTS:
; Num: The number of times to play the movie
; Base_Image: The base to be subtracted should have already been set up
; Subdata: The restored subdata
; Start: The first frame to be plotted. The default is 0.
; Finish: The last frame to be plotted. The default is the last frame.
; NOTE: If start = finish, only one frame is shown.
;
; Img_array: The 'Return Image' if needed during a rotation
; Good_exposures: The indices of the frames that remain after bad
; exposures were deleted.
;
;KEYWORDS:
; Rotation_Angle: If rotation angle is set, the image will be rotated.
; Reflect = If reflect is set, the image will be reflected.
; Center_x: The x-cor of the center of a rotation, if there is one
; Center_y: The y-cor of the center of a rotation, if there is one
; Hide_Movie: If hide_movie is set, the movie will not be shown
; Check_Exposures: If check_exposures is set, bad exposures will be eliminated.
; Permanent: If permanent is set, subdata and subindex will be modified
; Run: If run is set, a run difference image sequence will be used.
; Base: If base is set, a base difference image sequence will be used.
;
;OUTPUTS:
;
;DEPENDENCIES:
; check_exposures
;
;MODIFICATION HISTORY:
;Written by Michael Hammer, 07/2013


data = subdata
index = subindex

; Warning: Use copies of data if you are deleting stuff.
; Note: You should be deleting stuff.
if keyword_set(CHECK_EXPOSURES) then $
   check_exposures, good_exposures_bool, good_exposures, $
       SUBDATA = data, SUBINDEX = index, /DELETE


frames = n_elements(data[0,0,*])

if not keyword_set(CHECK_EXPOSURES) then $
   good_exposures = indgen(frames)

if not keyword_set(START) then start = 0
if not keyword_set(FINISH) then finish = frames - 1

x = n_elements(data[*,0,0])
y = n_elements(data[0,*,0])
z = n_elements(data[0,0,*])
img_array = intarr(x,y,z)

; Finds the new start and finish in the arrays with deleted data
s = min(abs(good_exposures - start), start_i)
f = min(abs(good_exposures - finish), finish_i)

print, start_i, finish_i

for j=1,num do begin ; Replay
   for i=start_i,finish_i do begin
      ind=strtrim(string(good_exposures[i]),2)
      if ind lt 100 then ind='0'+ind
      if ind lt 10 then ind='0'+ind
      if keyword_set(RUN) then $
         im=data[*,*,i]-data[*,*,i-1] $
      else if keyword_set(BASE) then $
         im=data[*,*,i] - base_image $
      else im=data[*,*,i] - base_image
      if keyword_set(ROTATION_ANGLE) then begin
         im = rot(im,rotation_angle,1,center_x,center_y)
      endif
      if keyword_set(REFLECT) then begin
         if reflect eq 1 then im = reverse(im)
      endif
      scimage=bytscl(im,max=30,min=-50)
      if not keyword_set(HIDE_MOVIE) then begin
         tvscl,scimage
         polyfill,[0.01,0.4,0.4,0.01],[0.96,0.96,0.99,0.99],color=0
         xyouts,0.011,0.97,index[i].date_obs+' / AIA '+wavelength $
                ,charsize=2,charthick=2,color=255,/norm
         polyfill,[0.93,0.99,0.99,0.93],[0.96,0.96,0.99,0.99],color=0
         xyouts,0.94,0.97,ind,charsize=2,charthick=3,color=255,/norm
         wait,0.1
      endif
      img_array[*,*,i] = im     ; Store images if they need to be returned
   endfor
endfor

if keyword_set(PERMANENT) then begin
   ; This will make the deletions of bad exposures permanent
   subdata = data
   subindex = index
endif

end ; EOF
