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
  
  if cc gt 0 then begin
;Order the local maxima by size
     maxima=maxima[0:cc-1]
     maxima.nmax=cc
  endif
  
  sort=reverse(sort(maxima.val))
  ordmaxima=maxima[sort]
  
  return,ordmaxima
  
end
;-================================================================================
