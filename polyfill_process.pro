pro polyfill_process,subdata,subindex,rotation_angle,date,evnum,dynrange=dynrange, REVISION_NUM = revision_num, SIMPLE_TIME = simple_time, REPLICA = replica, COLOR_TABLE_TEST = color_table_test, time=time, rad=rad
; Parameters:
; Data: A 2-D array where data[t,r] = the data at time t and pixel r
; Subindex: The subindex from the restored data
; Rotation Angle: The angle at which the images were rotated
; Date: Write the date in MMDDYY format. (eg 042313)
; Evnum: Each event has an a unique number. Leave off the initial 'e'
;
; Revision_Num: A string of length 3 containing a number to be used in
; the name of the file that will be saved
; Simple_Time: If this keyword is set, an even cadence of 12 seconds
; is assumed. If there are bad exposures, this is an unrealistic
; assumption.
; Replica: If this keyword is set, the program will recognize that the
; revision_num has already been used and will create the next lettered
; copy. For example, if revision_num = 3 and both '003' and '003a'
; exist, the saved file will be called '003b'
; Color_Table_Test: If this keyword is set, the program will create a
; new directory containing the plots of all 40 color tables.
;
; What the Program Does:
; This program takes an array of image data as a function of radius
; and time and plots that data by assembling a grid of polyfill squares. 
; If the time array does not have an even cadence, polyfill should
; draw rectangles instead of squares due to the uneven times.

data = subdata ; Make a copy (It never was the full subdata)

frames = n_elements(data[*,0]) ; The time coordinates are dim = 1
if keyword_set(SIMPLE_TIME) then begin
   dt = 12.0 ; time cadence in seconds
   time = findgen(frames)*dt ; time array
endif else begin
   nt=frames
   dt=fltarr(nt-1)
   time=fltarr(nt)
   for t=1,nt-1 do begin
      dt[t-1]=anytim(subindex[t].date_obs)-anytim(subindex[t-1].date_obs)
      time[t]=time[t-1]+dt[t-1]
   endfor
endelse

geometric_factor = 1.0/sin(subindex[0].arlon*!PI/180.0)

RSUN_PIXELS = median(subindex.imscl_mp / subindex.rsun_obs) ; pixels in Rsun
num_pixels = n_elements(data[0,*]) ; The radial coordinates are dim = 2

; Warning: geomtric_factor could be NaN, which is bad
if geometric_factor eq geometric_factor then $
   rad = (findgen(num_pixels)*RSUN_PIXELS + 1.0) $
        *geometric_factor $ ; (Distance to Limb + 1 Solar Radii) * correction
else rad = findgen(num_pixels)*RSUN_PIXELS + 1.0 ; geometric_factor = NaN

;rad = findgen(num_pixels)*2
;time = findgen(frames)*2 * num_pixels/frames

;profile = data[0,*] ; Time Zero Profile
;zero_indices = where(profile eq 0, count)
;if count ne 0 then $
;   profile[zero_indices] = 1
;for t=0,frames-1 do begin
;   data[t,*]/=profile ; Divide Data by Profile
;endfor

min = min(data[*,*])
max = max(data[*,*])
if keyword_set(dynrange) then begin
   min=dynrange[0]
   max=dynrange[1]
endif

loadct,0,/silent ; Blue & Gold Color Table
tvlct,rr,gg,bb,/get
;tvlct,reverse(rr),reverse(gg),reverse(bb)

plot_title = 'Shock Wave Tracker: Rotated ' $
             + strtrim(string(rotation_angle),1) + ' degrees'

wdef,0,1000,600
!P.position=[0.1,0.14,0.95,0.92]
!P.font=-1
plot,time,rad,yrange=[min(rad),max(rad)],xrange=[0.0,max(time)],$
     /xs,/ys,background=255,color=0,/nodata,charsize=2,$
     xtitle='Seconds since '+subindex[0].date_obs,ytitle='Radial distance, R!Dsun!N',$
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
      if color lt 0.0 then color=0.0
      if color gt 255.0 then color=255.0
      ;print,color	
      ;stop
      polyfill,[xmin,xmax,xmax,xmin],[ymin,ymin,ymax,ymax],color=color,/data
      ;print, xmin,xmax,ymin,ymax
   endfor
   ;stop
endfor

im=tvrd(true=1)
if keyword_set(COLOR_TABLE_TEST) then begin
   write_png,'CT'+strtrim(string(a),1)+'.png',im,rr,gg,bb
endif else begin
   write_png,'test.png',im,rr,gg,bb; FILL THIS IN <<<<<<--------
endelse


endfor

wait,0.3

end ; EOF
