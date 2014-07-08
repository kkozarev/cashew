pro test_fit_parabola

;Load the time height data and any additional information
;datafile='restore_me_007.sav'
;infofile='info_007.sav'

path='./restore/'

; e37 (0511W)
;datafile=path+'restore_me_37_193_r003.sav'
;infofile=path+'info_37_193_r003.sav'
;radrange=[1.1,1.65] ; radial extent of the measurement

; 0423W
;datafile=path+'restore_me_0423W_193_r003.sav'
;infofile=path+'info_0423W_193_r003.sav'
;radrange=[1.1,1.65] ; radial extent of the measurement

; 0526W
;datafile=path+'restore_me_0526W_193_r003.sav'
;infofile=path+'info_0526W_193_r003.sav'
;radrange=[1.1,1.65] ; radial extent of the measurement

; e53 (1109E)
;datafile=path+'restore_me_53_193_r003.sav'
;infofile=path+'info_53_193_r003.sav'
;radrange=[1.05,1.65] ; radial extent of the measurement

; e50 (0804W)
;datafile=path+'restore_me_50_193_r003.sav'
;infofile=path+'info_50_193_r003.sav'
;radrange=[1.05,1.65] ; radial extent of the measurement

; e0424E
datafile=path+'restore_me_0424E_211_r003.sav'
infofile=path+'info_0424E_211_r003.sav'
radrange=[1.05,1.65] ; radial extent of the measurement

numplotmax=5 ;number of maxima to track
dynrange=[1000,4500]

fit_parabola,datafile,infofile,path=path,numplotmax=numplotmax,allmaximind=allmaximind,allmaxima=allmaxima,nmax=nmax,allgfits=allgfits,time=time,distance=distance,dynrange=dynrange,radrange=radrange;/gaussfit,

end


function gaussianfit,x,aa
zz=(x-aa[1])/aa[2]
return,[[aa[0]*exp(-0.5*zz^2)+aa[3]+x*aa[4]+x^2*aa[5]],$  ;function
        [exp(-0.5*zz^2)],$                  ;function derivative wrt aa[0]
        [aa[0]/aa[2]*zz*exp(-0.5*zz^2)],$   ;function derivative wrt aa[1]
        [aa[0]/aa[2]*zz^2*exp(-0.5*zz^2)],$ ;function derivative wrt aa[2]
        [1.0],$                             ;function derivative wrt aa[3]
        [x],$                               ;function derivative wrt aa[4]
        [x^2]]                              ;function derivative wrt aa[5]
end



PRO fit_parabola,datafile,infofile,gaussfit=gaussfit,radrange=radrange,path=path,numplotmax=numplotmax,allmaximind=allmaximind,allmaxima=allmaxima,nmax=nmax,allgfits=allgfits,time=time,distance=distance,dynrange=dynrange,refine=refine
;Adapt the Callisto routine to detect AIA maxima.

!P.font=1
!p.position=[0.1,0.13,0.93,0.92]

if not keyword_set(path) then path = './'
restore,datafile
restore,infofile
;data_subindex=data_subindex[plotted_frame_initial:plotted_frame_final]
wav=strtrim(string(data_subindex[0].wavelnth),2)
if not keyword_set(dynrange) then dynrange=[-100,100]

;How many maxima to track
if not keyword_set(numplotmax) then numplotmax=2

;Plot the time-height map
;polyfill_process, data_thin_wave, data_subindex,data_rotation_angle,data_date,data_evnum,time=time,rad=rad,dynrange=dynrange

inner_offset = data_inner_x_index ; Use this to reset data later

extreme_start_radius = 1.13 ; to remove, 1.0
extreme_final_radius = 2.5 ; to revove, 2.5
extreme_start_time = 250 ; to remove, 0
extreme_final_time = 350 ; to remove, 10000

polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,wavelength = wav, start = data_start, inner_x = inner_offset, time=time,rad=rad,EXTREME_START_TIME = extreme_start_time, EXTREME_FINAL_TIME = extreme_final_time, EXTREME_START_RADIUS = extreme_start_radius, EXTREME_FINAL_RADIUS = extreme_final_radius,dynamic_range=dynrange, /SAVE_OPTION

distance=rad
loadct,0,/silent
tvlct,rr,gg,bb,/get
tvlct,reverse(rr),reverse(gg),reverse(bb)

ntimes=n_elements(time)
;dtime=time[1:ntimes-1]-time[0:ntimes-2]
nrad=n_elements(rad)

; Plot relevant radial range
if not keyword_set(radrange) then begin
   radrange=[1.08,max(rad)]
endif else begin
   if radrange[0] lt min(rad) then radrange[0]=min(rad)
   if radrange[1] gt max(rad) then radrange[1]=max(rad)
endelse
radrange=double(radrange)

;oplot,[min(time),max(time)],[radrange[0],radrange[0]],thick=3,linestyle=2,color=0
;oplot,[min(time),max(time)],[radrange[1],radrange[1]],thick=3,linestyle=2,color=0

data=data_thin_wave[*,inner_offset:*] ; Here is where you reset the data
index=data_subindex[0:ntimes-1]


;+============================================================================
;Ask the user for the points on the parabola.
;=========================================================

print, ''
print, 'Select starting point:'
cursor,x,y,/down,/data
print,x,y
plots,x,y,psym=5,symsize=2,thick=2,color=255
start_point = min(where(fix(time - x) gt 0.0)) - 1

print, ''
print, 'Select end point:'
cursor,x,y,/down,/data
print,x,y
plots,x,y,psym=5,symsize=2,thick=2,color=255
end_point = min(where(fix(time - x) gt 0.0)) - 1

print, start_point, end_point

; Between start & end points, ask user to select points at each time.
parabola_x = fltarr(end_point - start_point + 1) ; t = time
parabola_y = fltarr(end_point - start_point + 1) ; r = radius
for i = start_point,end_point do begin
   ; Draw vertical reference line
   delta = (time[i+1] - time[i]) / 2
   m = time[i] + delta
   last_radial_pixel = n_elements(rad) - 1
   oplot, [m,m], [0, last_radial_pixel], thick = 3, color = 255

   user_x = -1 ; t = time
   user_y = -1 ; r = radius
   while user_x lt time[i] or user_x gt time[i+1] do begin
      print, 'Select a point along the vertical line on the shock wave'
      cursor,user_x,user_y,/down,/data
      print, 'You selected ', user_x, user_y
   endwhile
   plots,[user_x],[user_y], psym = 4, thick = 4, color = 0
   
   ; Store user input
   ;parabola_x[i - start_point] = user_x ; Don't use these
   parabola_x[i - start_point] = time[i] ; Always use the start of the frame
   parabola_y[i - start_point] = user_y
   print, time[i], user_y
endfor

print,'Are you ready to fit a parabola?'
stop
   
; Overplot just the points to be fitted.
polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,wavelength = wav, start = data_start, inner_x = inner_offset,EXTREME_START_TIME = extreme_start_time, EXTREME_FINAL_TIME = extreme_final_time, EXTREME_START_RADIUS = extreme_start_radius, EXTREME_FINAL_RADIUS = extreme_final_radius,dynamic_range=dynrange, /REPLICA, /SAVE_OPTION
; Above: Careful with the REPLICA keyword

; Re-Plot "Parabola" Points on Shock Wave
loadct,0,/silent ; Switch Color Table
plots, parabola_x, parabola_y, psym = 4, thick = 4, color = 255

loadct,39,/silent ; Switch Color Table (Why?)

; Fit to a Parabola
print,''
print,'Fitting a second-order polynomial to the wave peak positions...'
bootstrap_sdo, parabola_y, parabola_x, fit_line, p1, p2, p3, s1, s2, s3

kinetic_model = p1[0] + p2[0] * (time[start_point:end_point]) + 0.5 * p3[0] * (time[start_point:end_point])^2
oplot,time[start_point:end_point],kinetic_model,thick=3

;initial height
r0 = parabola_y[0]
print,'R0 = '+strtrim(string(r0),2)+' +/-'+strtrim(string(s1[0],format='(f9.5)'),2)+' Rs'

;final height
rf = parabola_y[end_point - start_point]
print,'Rf = '+strtrim(string(rf),2)+' Rs'

;initial speed
v0=p2[0]*6.955e5 ; from R_sun to km
errv0=s2[0]*6.955e5 ; from R_sun to km
print,'V0 = '+strtrim(string(v0),2)+' +/-'+strtrim(string(errv0),2)+' km/s'

;final speed
tmp=p2[0]+p3[0]*(time[start_point:end_point]-time[start_point])
vf=tmp[n_elements(tmp)-1]*6.955e5 ; from R_sun to km
print,'Vf = '+strtrim(string(vf),2)+ ' km/s'

; Acceleration
accel=p3[0]*6.955e8 ; from R_sun to km
erraccel=s3[0]*6.955e8 ; from R_sun to km
print,'a = '+strtrim(string(accel),2)+' +/-'+strtrim(string(erraccel),2)+' m/s^2'
print,''




;record the image
image=tvrd(/true)
savname='e'+data_evnum+'_'+data_date+'_'+wav+'emission_maxima.png'
if keyword_set(outpath) then savname=outpath+savname
write_png,savname,image

;Save the output:
wave_times = time[start_point:end_point]
wave_rads = parabola_y
wave_indices = parabola_x
ind=data_subindex[start_point:end_point]
wave_data=data_thin_wave[start_point:end_point]

savname='e'+data_evnum+'_'+data_date+'_'+wav+'_jmap_measurements.sav'
if keyword_set(outpath) then savname=outpath+savname
save,filename=savname,time,rad,ind,wave_times,wave_rads,wave_indices,wave_data,r0,rf,v0,vf,errv0,accel,erraccel

end





;+
; NAME:
;     badpar
; PURPOSE: (one line)
;     Validate an input parameter against valid entries.
; DESCRIPTION:
;
;     This is a general parameter checking function for validating input
;     quantities in other procedures and functions.  This routine will
;     generate an error message indicating what is wrong with the item.
;
;     Example of use:
;
;     pro foo,array
;     if badpar(array,[4,5],2,CALLER='foo') then return
;        .
;        . code for foo .
;        .
;     end
;
;
;     This would cause an immediate return to the routine that called foo
;     with an error message if the input was not either floating or double
;     and 2 dimensional.
;
;     As of IDL v3.0, these are the recognized type codes (see 1-218 in
;        reference guide).
;
;        Type
;        Code     Data Type
;        ----    -----------------------------
;          0      Undefined
;          1      Byte
;          2      Integer
;          3      Longword integer
;          4      Floating point
;          5      Double-precision floating
;          6      Complex floating
;          7      String
;          8      Structure
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;     val = badpar(param,goodtype,goodrank)
; INPUTS:
;     param    - IDL variable to validate.
;     goodtype - Scalar or vector of type codes that are valid.
;     goodrank - Scalar or vector of valid ranks.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;     CALLER   - String identifying the calling routine.
;     DEFAULT  - Value to return in param if undefined and undefined allowed.
;     DIMEN    - Dimensions of variable.
;     NPTS     - Total number of elements in variable.
;     RANK     - Rank of variable.
;     TYPE     - Type of variable.
; OUTPUTS:
;     Return value is true if the parameter is bad.  False if good.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/24/93 - Written by Marc W. Buie, Lowell Observatory.
;     4/27/93 - MWB, added TYPE and DEFAULT keywords.
;-
function badpar,param,goodtype,goodrank, $
            CALLER=caller, DEFAULT=default, DIMEN=dimen, $
            NPTS=npts, RANK=rank, TYPE=type

errval = 0

sz = size(param)

rank = sz[0]
type = sz[rank+1]
npts = sz[rank+2]

err1=''
err2=''

if rank eq 0 then dimen=0 else dimen = sz[1:rank]

z=where(goodtype eq type, count)
if count eq 0 then begin
   case type of
      0 :    err1 = 'Undefined variable is not allowed.'
      1 :    err1 = 'Byte variable type is not allowed.'
      2 :    err1 = 'Integer variable type is not allowed.'
      3 :    err1 = 'Longword integer variable type is not allowed.'
      4 :    err1 = 'Floating point variable type is not allowed.'
      5 :    err1 = 'Double-precision floating point variable type is not allowed.'
      6 :    err1 = 'Complex floating point variable type is not allowed.'
      7 :    err1 = 'String variable type is not allowed.'
      8 :    err1 = 'Structure variable type is not allowed.'
      else : err1 = 'Unrecognized variable type code.  Impossible!'
   endcase
   errval=1
endif

if type ne 0 then begin
   z=where(goodrank eq rank, count)
   if count eq 0 then begin
      case rank of
         0 :    err2 = 'Scalar variables are not allowed.'
         1 :    err2 = 'Vector variables are not allowed.'
         2 :    err2 = '2-D variables are not allowed.'
         3 :    err2 = '3-D variables are not allowed.'
         4 :    err2 = '4-D variables are not allowed.'
         5 :    err2 = '5-D variables are not allowed.'
         6 :    err2 = '6-D variables are not allowed.'
         7 :    err2 = '7-D variables are not allowed.'
         8 :    err2 = '8-D variables are not allowed.'
         else : err2 = 'Unrecognized variable rank.  Impossible!'
      endcase
      errval=1
   endif
endif

if errval then begin
   if not keyword_set(caller) then caller = ''
   print,caller,'Illegal variable encountered.'
   if err1 ne '' then print,err1
   if err2 ne '' then print,err2
   return,errval
endif

if type eq 0 then begin
   szd = size(default)
   if szd[szd[0]+1] ne 0 then begin
      param = default
      sz    = size(param)
      rank  = sz[0]
      type  = sz[rank+1]
      npts  = sz[rank+2]
   endif
endif

return,errval

end


;+
; NAME:
;  lclxtrem
; PURPOSE:
;  Find local minima or maxima in a 1-d vector.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  idx=lclxtrem(vec,width,[/MAXIMA])
; INPUTS:
;  vec - Input vector of data points.
;  width - size of zone to search, minima (or maxima) separated by less than
;            width are never returned.  (Default = 5)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  MAXIMA  - Flag, if set, causes program to search for local maxima, the default
;            is to search for local minima
; OUTPUTS:
;  Returns indicies into vec that give local extrema.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  97/12/5, Written by Marc W. Buie, Lowell Observatory
;
;-
FUNCTION lclxtrem,vec,in_width,MAXIMA=maxima

   IF badpar(vec,[2,3,4,5],1,caller="LCLXTREM: (vec)") THEN return,-1
   IF badpar(in_width,[0,2,3,4,5],0,caller="LCLXTREM: (width)",default=5) THEN return,-1
   IF badpar(maxima,[0,1,2,3],0,caller="LCLXTREM: (MAXIMA)", $
                                                          default=0) THEN return,-1

   n=n_elements(vec)

   width=fix(in_width/2)*2+1
   wa=lindgen(width)
   idx=lonarr(n-width+1)
   FOR i=0,n_elements(idx)-1 DO BEGIN
      IF maxima THEN BEGIN
         z=where(vec[wa+i] eq max(vec[wa+i]))
      ENDIF ELSE BEGIN
         z=where(vec[wa+i] eq min(vec[wa+i]))
      ENDELSE
      idx[i]=z[0]+i
   ENDFOR

   idx=idx[uniq(idx)]

   FOR i=0,n_elements(idx)-2 DO BEGIN
      IF maxima THEN BEGIN
         rmin = min(vec[idx[i]:idx[i+1]])
         IF rmin eq vec[idx[i]] THEN BEGIN
            j=i
            old=idx[j]
            REPEAT BEGIN
               idx[j] = idx[i+1]
               if j ne 0 then j=j-1
            ENDREP UNTIL j eq 0 or idx[j] ne old
         ENDIF ELSE IF rmin eq vec[idx[i+1]] THEN BEGIN
            idx[i+1] = idx[i]
         ENDIF

      ENDIF ELSE BEGIN
         rmax = max(vec[idx[i]:idx[i+1]])
         IF rmax eq vec[idx[i]] THEN BEGIN
            j=i
            old=idx[j]
            REPEAT BEGIN
               idx[j] = idx[i+1]
               if i gt 0 then j=j-1 else j=0
            ENDREP UNTIL j eq 0 or idx[j] ne old
         ENDIF ELSE IF rmax eq vec[idx[i+1]] THEN BEGIN
            idx[i+1] = idx[i]
         ENDIF
      ENDELSE
   ENDFOR

   idx=idx[uniq(idx)]

   return,idx
END
