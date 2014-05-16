pro test_polyfill_process
  wave=['193']
  event=load_events_info(label='110511_01')
  strin=strsplit(event.st,'-:/T .',/extract)
  date=strin[0]+strin[1]+strin[2]
  path=event.savepath

     ; Set File Names
   revision_num=0
   revision_num_str = strtrim(string(revision_num),2)
   if revision_num lt 10 then revision_num_str='0'+revision_num_str
   if revision_num lt 10 then revision_num_str='0'+revision_num_str
   
   suffix = + label + '_' + wave + '_r' + revision_num_str + '.sav'
   data_file = path + 'jmap_data_' + suffix
   info_file = path + 'jmap_info_' + suffix
   if file_exist(data_file) then begin
      restore, data_file
   endif else begin
      print,''
      print,'File ' + data_file + ' does not exist! Quitting...'
      print,''
   endelse
  
   polyfill_process, data_thin_wave, data_subindex,data_rotation_angle, data_date, label, PATH = event.savepath,$
                            savefile='jmap_data_'+label+'_'+wave+'_r'+revision_num_str,$
                            dynamic_range=[-10,100]
end


pro polyfill_process,subdata,subindex,rotation_angle,date,evnum,dynamic_range=dynamic_range, REVISION_NUM = revision_num, SIMPLE_TIME = simple_time, REPLICA = replica, COLOR_TABLE_TEST = color_table_test, time=time, rad=rad,savefile=savefile, PATH = PATH
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
if not keyword_set(path) then path='./'
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
if keyword_set(dynamic_range) then begin
   min=dynamic_range[0]
   max=dynamic_range[1]
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
   
   if keyword_set(savefile) then outfile=path+savefile+'.png' else outfile=path+'test.png'
   write_png,outfile,im,rr,gg,bb
endelse


endfor

end ; EOF
