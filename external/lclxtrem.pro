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
   if width gt in_width then width=in_width-1
   wa=lindgen(width)
   if n-width+1 eq 0 then stop
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
