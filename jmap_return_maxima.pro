pro jmap_return_maxima,arr,x,allmaxima=allmaxima,numplotmax=numplotmax
;PURPOSE:
;Procedure returns ordered maxima and their indices in a 1D array of values
;
;CATEGORY:
;AIA/Kinematics/jmap
;
;INPUTS:
; arr - 1D array of values.
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
;lclxtrem.pro, badpar.pro
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2013/09/19
;

  arr=reform(arr)
  nx=n_elements(x)
  smfac=20
  if nx lt smfac then smfac=nx-1
  if not keyword_set(numplotmax) then nmax=nx-1 else nmax=numplotmax
  
  tmp={val:0.0D,ind:0L}
  allmaxima=replicate(tmp,nmax)
  
;This function finds the local minima
  ind=lclxtrem(arr-smooth(arr,smfac,/edge_truncate),10)

;Record the maxima in each of the intervals set by the minima
;locations. 
     maxima=replicate(tmp,100)
     locmax=tmp
     cc=0
     

     plot,arr-smooth(arr,smfac,/edge_truncate),psym=10
     for ii=0,n_elements(ind)-1 do begin
        oplot,[ind[ii],ind[ii]],[-10,10]
     ;find the local maximum in every interval
        if ii lt n_elements(ind)-1 then begin
           locarr=arr[ind[ii]:ind[ii+1]]
           locmax.val=max(locarr,maxind)
           locmax.ind=maxind+ind[ii]
           oplot,[locmax.ind,locmax.ind],[-10,10],linestyle=2
           maxima[cc]=locmax
           cc++
        endif
     endfor
     stop

     totmaxnum=cc
     maxima=maxima[0:cc-1]
     

;Order the local maxima by size
     sort=reverse(sort(maxima[*].val))
     maxima=maxima[sort]
     for ii=0,nmax-1 do allmaxima[ii]=maxima[ii]
     stop


end
