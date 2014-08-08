;+================================================================================
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
;-================================================================================


;+================================================================================
function get_local_maxima, column,dist,yind=yind,gaussfit=gaussfit
;This function finds the local maxima and orders them by size

;set the distance range
  if keyword_set(yind) then yindrange=yind else yindrange=[0,n_elements(dist)-1]
  
;First find the local minima.
  minind=lclxtrem(column-smooth(column,20,/edge_truncate),10)
  
;Record the maxima in each of the intervals set by the minima
;locations. 
  maxima=replicate({val:0.0D,ind:0L,rad:0.0D,gfit:dblarr(5),nmax:0},100)
  cc=0
  
;Find the local maximum in every interval
  for ii=0,n_elements(minind)-1 do begin
     if ii lt n_elements(minind)-1 then begin
;This is a filter that lets through only the biggest maxima.
        if (minind[ii+1]-minind[ii] le 2) then continue
        
        locarr=column[minind[ii]:minind[ii+1]]
        locy=dist[minind[ii]+yindrange[0]:minind[ii+1]+yindrange[0]]
        maxima[cc].val=max(locarr)
        maxima[cc].ind=minind[ii]+!C+yindrange[0]
        maxima[cc].rad=dist[maxima[cc].ind]
       
;Optionally, do a gaussian fit to all maxima to find the max position           
        if keyword_set(gaussfit) then begin
;6-term fit

; For some strange reason I am receiving a compile time error when I
; include the nterms=6 argument to gaussfit. Looking at documenation
; it looks like it defaults to nterms=6 anyways so I'm removing
; this for now
;           res=gaussfit(locy,locarr,aa,nterms=6)
           res=gaussfit(locy,locarr,aa)
;Follow up with a Levenberg-Marquardt fitting algorigthm
           res=lmfit(locy,locarr,aa,/double,function_name='gaussianfit',sigma=sigma)
;           if cc eq 0 then print,aa[1],dist[lmaxind]
           maxima[cc].gfit[0]=aa[1]                        ;X-location (radial) of the peak
           maxima[cc].gfit[1]=max(res)                     ;Y-location (intensity) of the peak
           maxima[cc].gfit[2]=2*sqrt(2*alog(2)*aa[2]^2)    ;The FWHM of the gaussian fit
           if maxima[cc].gfit[2] eq 'Inf' or maxima[cc].gfit[2] eq 'NaN' then maxima[cc].gfit[2]=1.0e-10
           maxima[cc].gfit[3]=sigma[1]    ;The error in radial position of the maximum
           maxima[cc].gfit[4]=sigma[0]    ;The error in the fitted peak value
           zz=(locy-aa[1])/aa[2]
        endif
        cc++
     endif
  endfor
  
;Order the local maxima by size
  maxima=maxima[0:cc-1]
  maxima.nmax=cc
  sort=reverse(sort(maxima.val))
  ordmaxima=maxima[sort]
  
  return,ordmaxima
  
end
;-================================================================================



;+================================================================================
pro aia_jmap_find_maxima,data,time,dist,xrange=xrange,yrange=yrange,gaussfit=gaussfit,allgfits=allgfits,allmaxima=allmaxima,mymaxima=mymaxima,nmax=nmax,flipyaxes=flipyaxes,numplotmax=numplotmax
;PURPOSE:
;procedure to find maxima in a time-distance image.
;this is used in fitting EUV wave positions.
;
;CATEGORY:
;AIA/Kinemaics
;
;INPUTS:
;
;KEYWORDS:
; 
;OUTPUTS:
;
;DEPENDENCIES:
;   lclxtrem.pro, badpar.pro, lmfit, gaussfit
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 05/14/2014, as a special case of jmap_find_maxima
;
  resolve_routine,'gaussfit',/either,/compile_full_file,/no_recompile

;------------------------------------------------------------
;DEFINITIONS AND CONSTANTS
  ntime=n_elements(time)
  ndist=n_elements(dist)
  if not keyword_set(numplotmax) then numplotmax=1
;Select the X-Y ranges, apply keywords
  if keyword_set(xrange) then xrang=xrange else $
     xrang=[time[0],time[ntime-1]]
  xind=[min(where(time-xrang[0] ge -1.e-6)),min(where(time-xrang[1] ge -1.e-6))]
  if keyword_set(yrange) then yrang=yrange else $
     yrang = [dist[0],dist[ndist-1]]
  yind = [min(where(dist-yrang[0] ge -1.e-6)),min(where(dist-yrang[1] ge -1.e-6))]
  if keyword_set(flipyaxes) then begin
     yrang = reverse(yrang)
     yind = [max(where(dist-yrang[0] ge -1.e-6)),max(where(dist-yrang[1] ge -1.e-6))]
  endif
;Definitions
  tmp={val:0.0D,ind:0L,rad:0.0D,gfit:dblarr(5),nmax:0}
  allmaxima=replicate(tmp,100,xind[1]-xind[0]+1)
  mymaxima=replicate(tmp,numplotmax,xind[1]-xind[0]+1)
  allnmax=intarr(xind[1]-xind[0]+1)
;------------------------------------------------------------



;------------------------------------------------------------
;Find all local maxima in every column of data

;LOOP OVER TIME (X)
  for xx=xind[0],xind[1] do begin
     ;Set the array to hold a single column
     column=reform(data[xx,yind[0]:yind[1]])
     
     ;get an ordered list of all local maxima for this column ordered by size(intensity)
     column_maxima=get_local_maxima(column,dist,yind=yind)
     ncolmax=n_elements(column_maxima)
     if ncolmax le 0 then begin
        print,''
        print,'No Maxima found. Exiting...'
        print,''
        return
     endif     
     allnmax[xx-xind[0]]=ncolmax
     allmaxima[0:ncolmax-1,xx-xind[0]]=column_maxima
  endfor     
;------------------------------------------------------------


;Save to the overall maximum catalog
  for xx=xind[0],xind[1] do begin
     ncolmax=allnmax[xx-xind[0]]
     mymaxima[*,xx-xind[0]]= allmaxima[0:numplotmax-1,xx-xind[0]]
  endfor

end
;-================================================================================



