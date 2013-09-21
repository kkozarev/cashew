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

pro find_jmap_maxima,arr,freqrng,frng,timrng,zb,y,gaussfit=gaussfit
;procedure to find maxima in a time-Y image, where Y can be physical distance, or frequency.
;this is used in fitting EUV wave and radio tII burst positions and emission frequencies.

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

end




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
