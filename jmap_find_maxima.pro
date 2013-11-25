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




pro jmap_find_maxima,zb,x,y,xrange=xrange,yrange=yrange,gaussfit=gaussfit,allgfits=allgfits,allmaxima=allmaxima,mymaxima=mymaxima,nmax=nmax,flipyaxes=flipyaxes,numplotmax=numplotmax
;PURPOSE:
;procedure to find maxima in a time-Y image, where Y can be physical distance, or frequency.
;this is used in fitting EUV wave and radio tII burst positions and emission frequencies.
;
;CATEGORY:
;AIA/Kinemaics/jmap
;
;INPUTS:
;
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
;lclxtrem.pro, badpar.pro, lmfit, gaussfit
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 09/11/2013   
  
  resolve_routine,'gaussfit',/either,/compile_full_file,/no_recompile
  
  nx=n_elements(x)
  ny=n_elements(y)
  if not keyword_set(numplotmax) then numplotmax=2
  
;Select the X-Y ranges
  if keyword_set(xrange) then xrang=xrange else $
     xrang=[x[0],x[nx-1]]
  xind=[min(where(x-xrang[0] ge -1.e-6)),min(where(x-xrang[1] ge -1.e-6))]
  
  if keyword_set(yrange) then yrang=yrange else $
     yrang = [y[0],y[ny-1]]
  yind = [min(where(y-yrang[0] ge -1.e-6)),min(where(y-yrang[1] ge -1.e-6))]
  
  if keyword_set(flipyaxes) then begin
     yrang = reverse(yrang)
     yind = [max(where(y-yrang[0] ge -1.e-6)),max(where(y-yrang[1] ge -1.e-6))]
  endif
  
  allgfits=fltarr(100,5,xind[1]-xind[0]+1)
  tmp={val:0.0D,ind:0L}
  allmaxima=replicate(tmp,100,xind[1]-xind[0]+1)
  mymaxima=replicate(tmp,100,xind[1]-xind[0]+1)
  nmax=intarr(xind[1]-xind[0]+1)
  
  
  
;Loop over the X-values (usually time)
  for xx=xind[0],xind[1] do begin
     
;arr=smooth(reform(zb[xx,frng[0]:frng[1]]),4,/edge_truncate)
     arr=reform(zb[xx,yind[0]:yind[1]])
     ;plot,y[yind[0]:yind[1]],arr-smooth(arr,20,/edge_truncate),xrange=[yrang[0],yrang[1]],$
     ;     yrange=[min(arr),max(arr)],/xs,$
     ;     charsize=2,charthick=3,xtitle = 'Frequency [MHz]' ;,thick=2

;Plot the mean value
;oplot,[freqrng[0],freqrng[1]],[mean(arr),mean(arr)],thick=2

;This function finds the local minima

     ind=lclxtrem(arr-smooth(arr,20,/edge_truncate),10)
     
;Record the maxima in each of the intervals set by the minima
;locations. 
     maxima=fltarr(100)
     indmaxima=intarr(100)
     gfit=fltarr(100,5)
     cc=0
     
     for ii=0,n_elements(ind)-1 do begin
                                ;plot the ranges of the local maximum intervals
        ;oplot,[y[ind[ii]+yind[0]],y[ind[ii]+yind[0]]],[min(zb),max(zb)]
        
                                ;find the local maximum in every interval
        if ii lt n_elements(ind)-1 then begin
           locarr=arr[ind[ii]:ind[ii+1]]
           locy=y[ind[ii]+yind[0]:ind[ii+1]+yind[0]]
           locmax=max(locarr)
           lmaxind=ind[ii]+!C+yind[0]
           
                                ;This is a filter that lets through only the biggest maxima.
                                ;locmax lt mean(arr) or 
           if (ind[ii+1]-ind[ii] le 6) then continue
           maxima[cc]=locmax
           indmaxima[cc]=lmaxind
           
           if keyword_set(gaussfit) then begin
              res=gaussfit(locy,locarr,aa,nterms=6)
;Follow up with a Levenberg-Marquardt fitting algorigthm
              res=lmfit(locy,locarr,aa,/double,function_name='gaussianfit',sigma=sigma)
              if cc eq 0 then print,aa[1],y[lmaxind]
              gfit[cc,0]=aa[1]                   ;X-location (radial) of the peak
              gfit[cc,1]=max(res)                ;Y-location (intensity) of the peak
              gfit[cc,2]=2*sqrt(2*alog(2)*aa[2]^2) ;The FWHM of the gaussian fit
              if gfit[cc,2] eq 'Inf' or gfit[cc,2] eq 'NaN' then gfit[cc,2]=1.0e-10
              gfit[cc,3]=sigma[1] ;The error in radial position of the maximum
              gfit[cc,4]=sigma[0] ;The error in the fitted peak value         
              
              zz=(locy-aa[1])/aa[2]
                                ;6-term fit
              ;oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4]+locy^2*aa[5],linestyle=2,thick=2
                                ;5-term fit
                                ;oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4],linestyle=2,thick=2
                                ;4-term fit
                                ;oplot,locy,aa[3]+aa[0]*exp(-0.5*zz^2),linestyle=2,thick=2
              ;plots,y[lmaxind],locmax,psym=2,symsize=2,thick=2,/data
                                ;print,2*sqrt(2*alog(2)*aa[2]^2)  ;The
                                ;FWHM of the gaussian fit - doesn't
                                ;                           seem right
                                ;                           though...
           endif
           cc++
        endif
     endfor
     nmax[xx-xind[0]]=cc
     maxima=maxima[0:cc-1]
     indmaxima=indmaxima[0:cc-1]
     gfit=gfit[0:cc-1,*]
     
;Order the local maxima by size
     sort=reverse(sort(maxima))
     ordmaxima=maxima[sort]
     ordindmaxima=indmaxima[sort]
     
;Save to the overall maximum catalog
     allmaxima[0:cc-1,xx-xind[0]].ind=ordindmaxima
     allmaxima[0:cc-1,xx-xind[0]].val=ordmaxima
     allgfits[0:cc-1,*,xx-xind[0]]=gfit
     
;Set an initial threshold in pixels for how much the maximum could have moved
     inithresh=5
     thresh=fltarr(numplotmax)+inithresh
     
     if xx eq xind[0] then begin
        mymaxima[0:cc-1,xx-xind[0]].ind=ordindmaxima
        mymaxima[0:cc-1,xx-xind[0]].val=ordmaxima
        
     endif else begin
                                ;1. Check if the Nth maximum is within +/-thresh/2.0 pixels of the position of the
                                ;Nth maximum from last step.
                                ;2. If yes, record as the same maximum from last time, calculate the speed
                                ;of the maximum.
                                ;3. If not, search for a maximum that is within +/-thresh/2.0 pixels this
                                ;time and assume that is the maximum we want.
        
                                ;go over the first numplotmax maxima that the code has found
                                ;Check if the pixel difference with the previous location is larger than the threshold
        
                                ;goto,jump
        
        for ii=0,numplotmax-1 do begin
           ci=ii
           if abs(ordindmaxima[ii] - mymaxima[ii,xx-xind[0]-1].ind) gt 2*thresh[ii] then begin
                                ;If yes, go and find another maximum
                                ;within that range and move it to the
                                ;current maximum position
                                ;for tt=ci,nmax[xx-xind[0]]-1 do begin
              ;print,ii+1,nmax[xx-xind[0]]-1
              ;print,xx-xind[0]-1
              ;stop
              if nmax[xx-xind[0]]-1 lt ii+1 then dmax=0 else $
              dmax=ordindmaxima[ii] - mymaxima[ii+1:nmax[xx-xind[0]]-1,xx-xind[0]-1].ind
              
                                ;check if there's a closer maximum - if not, keep this one.
              mmm=-1.0
              mult=1.0
              while mmm lt 0 do begin
                 if mult*thresh[ii] gt 3*inithresh then begin
                    mmm=ii
                    break
                 endif
                 
                 tmp=where(dmax le mult*thresh[ii] and dmax gt 0.0)
                 if tmp[0] ge 0 then mmm=min(tmp) else mult++
              endwhile
              
              mymaxima[ii,xx-xind[0]].ind=ordindmaxima[mmm]
              mymaxima[ii,xx-xind[0]].val=ordmaxima[mmm]
              
           endif else begin
                                ;Otherwise record the position for the same maximum
              mymaxima[ii,xx-xind[0]].ind=ordindmaxima[ii]
              mymaxima[ii,xx-xind[0]].val=ordmaxima[ii]
              if thresh[ii] gt inithresh then $
                 thresh=mymaxima[ii,xx-xind[0]].ind-mymaxima[ii,xx-xind[0]-1].ind
                                ;print,thresh[ii]
                                ;stop
           endelse
                                ;Update the threshold because the speeds will vary
                                ;if ii ge 3 then thresh=mean(deriv(mymaximind[ii-3:ii,xx-xind[0]-1]))
        endfor
                                ;stop
     endelse
     
;jump:
     
;for ii=0,n_elements(sort)-1 do begin
                                ;oplot,[rad[ordindmaxima[ii]],rad[ordindmaxima[ii]]],$
                                ;[min(data),max(data)],linestyle=3,thick=3
                                ;print,ordindmaxima[ii],ordmaxima[ii]
;endfor
     
  endfor

end
