
;+====================================================================
pro test_callisto_plot_spectrum_new
  
;Run the program like this to combine four files at different times
;and frequency ranges
;Order the files like this - rows are time, cols are frequency (from high freq to low)
files = [['BIR_20111105_101500_02.fit.gz','BIR_20111105_103000_02.fit.gz'],$
         ['BIR_20111105_101500_01.fit.gz','BIR_20111105_103000_01.fit.gz']]
timerange='2011-NOV-05 10:' + ['20:30','36:00']
freqrange=[40,160]
station='Birr'
callisto_plot_spectrum_new,files,timerange=timerange,station=station,freqrange=freqrange

stop


files = ['ALMATY_20110809_080000_59.fit']
timerange='2011-AUG-09 08:' + ['02:00','04:30']
freqrange=[47,450]
station='Almaty'

;plot this to see everything in the file (frequency and time)
callisto_plot_spectrum_new,files

stop

;run this to zoom in on a range of frequencies and times
callisto_plot_spectrum_new,files,timerange=timerange,station=station,freqrange=freqrange,plot=plot;,fit=fit

end




PRO callisto_plot_spectrum_new,files,timerange=timerange,station=station,freqrange=freqrange,fit=fit,plot=plot
;Example on how to plot 2d spectrogram of Callisto data.

loadct,9
tvlct,rr,gg,bb,/get
tvlct,reverse(rr),reverse(gg),reverse(bb)
!P.font=1
!p.position=[0.1,0.13,0.93,0.92]

nfiles=n_elements(files)
ntf=n_elements(files[*,0])
nff=n_elements(files[0,*])

if not keyword_set(station) then station=''

;Read the radio files
for ff=0,nff-1 do begin
   for tf=0,ntf-1 do begin
      if find_file(files[tf,ff]) eq '' then begin
         print,''
         print,'Error: File "'+ files[tf,ff]+'" was not found. Exiting...'
         print,''
         return
      endif
      radio_spectro_fits_read,files[tf,ff],z1,x1,y1
      if tf eq 0 then begin
         z=z1
         x=x1
      endif else begin
         z=[z,z1]
         x=[x,x1]
      endelse
   endfor
   if ff eq 0 then y=y1 else y=[y,y1]
   if ff eq 0 then zz=z else zz=[[zz],[z]]
endfor

;Fix the frequencies
if nff gt 1 then elimwrongchannels, zz, x, y

;Do a constant background subtraction on the intensities
zb=constbacksub(zz, /auto)

;Select the frequency ranges
if keyword_set(freqrange) then freqrng=freqrange else $
   freqrng = [y[n_elements(y)-1],y[0]]   
frng=[min(where(freqrng[1]-y gt 0.0)),min(where(freqrng[0]-y gt 0.0))]

;Select the time ranges
if keyword_set(timerange) then $
   timrng=[max(where(anytim(timerange[0])-x gt 0.0)),max(where(anytim(timerange[1])-x gt 0.0))] $
else $
   timrng=[0,n_elements(x)-1]


window,0,xsize=900,ysize=600; for XWIN only!
spectro_plot, zb, x,y ,/xs, /ys, $
xrange = timerange, $
yrange = [freqrng[0],freqrng[1]], ytitle = 'Frequency [MHz]', $
title='Callisto spectrum '+station,charsize=2.4,charthick=3
image=tvrd(/true)
write_png,'radio_spectrum.png',image


;======================================
;Part 2. Find and fit the spectrum maxima
if keyword_set(fit) then begin
wdef,1,900,500

allgfits=fltarr(100,5,timrng[1]-timrng[0]+1)
allmaxima=fltarr(100,timrng[1]-timrng[0]+1)
allmaximind=fltarr(100,timrng[1]-timrng[0]+1)
mymaxima=fltarr(100,timrng[1]-timrng[0]+1)
mymaximind=fltarr(100,timrng[1]-timrng[0]+1)
nmax=intarr(timrng[1]-timrng[0]+1)

for timind=timrng[0],timrng[1] do begin

;arr=smooth(reform(zb[timind,frng[0]:frng[1]]),4,/edge_truncate)
arr=reform(zb[timind,frng[0]:frng[1]])
plot,y[frng[0]:frng[1]],arr,xrange=[freqrng[0],freqrng[1]],$
     yrange=[min(arr),max(arr)],/xs,$
     charsize=2,charthick=3,xtitle = 'Frequency [MHz]' ;,thick=2
;Plot the mean value
;oplot,[freqrng[0],freqrng[1]],[mean(arr),mean(arr)],thick=2


;This function finds the local minima
ind=lclxtrem(arr,10)


;Record the maxima in each of the intervals set by the minima locations.
maxima=fltarr(100)
indmaxima=intarr(100)
gfit=fltarr(100,5)
cc=0

for ii=0,n_elements(ind)-1 do begin
   ;plot the ranges of the local maximum intervals
   oplot,[y[ind[ii]+frng[0]],y[ind[ii]+frng[0]]],[min(zb),max(zb)]

   ;find the local maximum in every interval
   if ii lt n_elements(ind)-1 then begin
      locarr=arr[ind[ii]:ind[ii+1]]
      locy=y[ind[ii]+frng[0]:ind[ii+1]+frng[0]]
      locmax=max(locarr)
      lmaxind=ind[ii]+!C+frng[0]
      
      ;This is a filter that lets through only the biggest maxima.
      ;locmax lt mean(arr) or 
      if (ind[ii+1]-ind[ii] le 6) then continue
      maxima[cc]=locmax
      indmaxima[cc]=lmaxind
      
      if keyword_set(gaussfit) then begin
         res=gaussfit(locy,locarr,aa,nterms=6)
;Follow up with a Levenberg-Marquardt fitting algorightm
         res=lmfit(locy,locarr,aa,/double,function_name='gaussianfit',sigma=sigma)
         if cc eq 0 then print,aa[1],rad[lmaxind]
         gfit[cc,0]=aa[1]                        ;X-location (radial) of the peak
         gfit[cc,1]=max(res)                     ;Y-location (intensity) of the peak
         gfit[cc,2]=2*sqrt(2*alog(2)*aa[2]^2)    ;The FWHM of the gaussian fit
         if gfit[cc,2] eq 'Inf' or gfit[cc,2] eq 'NaN' then gfit[cc,2]=1.0e-10
         gfit[cc,3]=sigma[1]    ;The error in radial position of the maximum
         gfit[cc,4]=sigma[0]    ;The error in the fitted peak value
         

         zz=(locy-aa[1])/aa[2]
                                ;6-term fit
         oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4]+locy^2*aa[5],linestyle=2,thick=2
                                ;5-term fit
                                ;oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4],linestyle=2,thick=2
                                ;4-term fit
                                ;oplot,locy,aa[3]+aa[0]*exp(-0.5*zz^2),linestyle=2,thick=2
         plots,y[lmaxind],locmax,psym=2,symsize=2,thick=2,/data
                                ;print,2*sqrt(2*alog(2)*aa[2]^2)  ;The
                                ;FWHM of the gaussian fit - doesn't
                                ;                           seem right
                                ;                           though...
      endif
      cc++
   endif
endfor
nmax[timind]=cc
maxima=maxima[0:cc-1]

indmaxima=indmaxima[0:cc-1]

sort=reverse(sort(maxima))
ordmaxima=maxima[sort]
ordindmaxima=indmaxima[sort]

allmaximind[0:cc-1,timind-timrng[0]]=ordindmaxima
allmaxima[0:cc-1,timind-timrng[0]]=ordmaxima

;for ii=0,n_elements(sort)-1 do begin
   ;oplot,[y[ordindmaxima[ii]],y[ordindmaxima[ii]]],[min(zb),max(zb)],linestyle=3,thick=3
   ;print,ordindmaxima[ii],ordmaxima[ii]
;endfor

;Other things to do:
;- Order the local maxima by size, and only record the N largest
;  maxima
;- Find the true FWHM of the gaussian fits
;- Do this for all time steps, plot the locations of the four largest maxima for every time on the
;spectrum

;wait,0.1

endfor

wdef,2,900,500

spectro_plot, zb, x,y ,/xs, /ys, $
xrange = timerange, $
yrange = [freqrng[0],freqrng[1]], ytitle = 'Frequency [MHz]', $
title='Callisto spectrum '+station,charsize=2,charthick=3

loadct,39,/silent
colors=[255,190,250,60,30]
numplotmax=3
for timind=timrng[0], timrng[1] do begin
   tmp=where(allmaximind[*,timind-timrng[0]] eq 0)
   nmax=min(tmp)
   
   for mm=0,nmax-1 do begin
      if mm eq numplotmax then break
      plots,x[timind],y[allmaximind[mm,timind-timrng[0]]],$
            color=colors[mm],psym=1,symsize=1,thick=4
   endfor
endfor

for mm=0,numplotmax-1 do begin
   if mm le 1 then polyfill,[0.933,0.97,0.97,0.933],[0.785,0.785,0.815,0.815]-mm*0.05,/norm,color=0
   plots,0.94,0.8-mm*0.05,psym=1,symsize=1,thick=4,/norm,color=colors[mm]
   xyouts,0.941,0.79-mm*0.05,' #'+strtrim(string(mm+1),2),color=colors[mm],/norm,charsize=1.6
endfor

image=tvrd(/true)
write_png,'emission_maxima.png',image
endif

end
;-====================================================================



;+====================================================================
;
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
