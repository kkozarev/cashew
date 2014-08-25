function opflow3d,im,kmaxx,kmaxy,vx,vy,mxx,myy,mxy,camps=camps,reuse=reuse,$
  direct=direct,niter=niter,tol=tol,mat=mat,rhs=b,guess=guess,verbose=verbose
;----------------------------------------------------------------------------
; FUNCTION opflow3d
; this finds the velocity field v such that
; didt = -v*grad(im)
; assuming v is a constant vector field composed of sinusoids
; 
; INPUT
;	im 	- the data cube (nx,ny,nt) to be fit
;	kmaxx 	- the maximum wavenumbers to be used in the fit
;	kmaxy     the total number of modes used is 4*kmaxx*kmaxy
; OUTPUT
;	derived velocity field (nx,ny,2)
; KEYWORDS
;	camps 	- complex (2*kmaxx,2*kmaxy) fourier coefficients of fit
; 	reuse 	- if set reuses arrays vx,vy,mxx,mxy,myy instead of
;	  	    recomputing them
;	direct 	- use direct method instead of iterative. This is much slower
;		    and uses more memory, but the latter only works if the 
;		    matrix, mat is very diagonally dominant. Default = 0
;	niter	- number of iterations for iterative method. Default=10
;	tol	- error tolerance for iterative method. Default=1e-5
;	mat	- the full matrix and righthand side of equation to be solved:
;	rhs	    mat#coef = rhs (if DIRECT set)
; OTHER STUFF
;	vx,vy,mxx,mxy,myy - matrices used during computation
;			    If REUSE is set then they are not computed
;			    Else their computed values are returned
; METHOD
;	Optical flow method as described in
; 	 "A Spectral Optical Flow method for Determining Solar Surface Flow"
; 	  by Neal Hurlburt, in prepartion 1996
; ROUTINES USED
;	deriv, svd, svdc, la_least_squares
; AUTHOR
;	Neal Hurlburt, Lockheed-Martin Advanced Technology Center
;	November 1996
; NOTES
;       Replaced svd methods with la_least_squares.
;       Added option for using differnt methods using existing direct
;       keyword. If it is nonzero, its value determines which direct
;       method is used method=direct-1 for 1 < direct < 4
;------------------------------------------------------------------------
time0=systime(1)
sim=size(im) & nx=sim(1) & ny=sim(2) & nt=sim(3)
norm=1./sim(5)
if not keyword_set(niter) then niter=10
if not keyword_set(tol) then tol=1e-5
; compute the coeffients
if not keyword_set(reuse) then begin
   vx=fltarr(nx,ny)
   vy=fltarr(nx,ny)
   mxx=fltarr(nx,ny)
   myy=fltarr(nx,ny)
   mxy=fltarr(nx,ny)
   didx=fltarr(nx,ny)
;
   for it=1,nt-1 do begin
      imta=0.5*(im(*,*,it-1)+im(*,*,it))
      for i=0,ny-1 do didx(0,i)=deriv(imta(*,i))
      didy=transpose(imta)
      for i=0,nx-1 do didy(0,i)=deriv(didy(*,i))
      didy=transpose(didy)
      didt=im(*,*,it)-im(*,*,it-1)
      vx=vx+didt*didx
      vy=vy+didt*didy
      mxx=mxx+didx^2
      myy=myy+didy^2
      mxy=mxy+didy*didx
      endfor
   vx=norm*vx
   vy=norm*vy
   mxx=norm*mxx
   myy=norm*myy
   mxy=norm*mxy
   endif

; now generate the complex righthand side of the system
bf=shift(fft(vx,-1),kmaxx,kmaxy)
bc=complexarr(2*kmaxx,2*kmaxy,2)
bc(0,0,0)=-bf(0:2*kmaxx-1,0:2*kmaxy-1)
bf=shift(fft(vy,-1),kmaxx,kmaxy)
bc(0,0,1)=-bf(0:2*kmaxx-1,0:2*kmaxy-1)

; and the complex matrix
mxxf=shift(fft(mxx,-1),2*kmaxx,2*kmaxy)
myyf=shift(fft(myy,-1),2*kmaxx,2*kmaxy)
mxyf=shift(fft(mxy,-1),2*kmaxx,2*kmaxy)
mxxf=mxxf(0:4*kmaxx-1,0:4*kmaxy-1)
mxyf=mxyf(0:4*kmaxx-1,0:4*kmaxy-1)
myyf=myyf(0:4*kmaxx-1,0:4*kmaxy-1)

if not keyword_set(direct) then begin     ; use the iterative method
   a11=float(mxxf(2*kmaxx,2*kmaxy))
   a12=float(mxyf(2*kmaxx,2*kmaxy))
   a22=float(myyf(2*kmaxx,2*kmaxy))
   det=a11*a22-a12*a12
   a11=a11/det
   a12=a12/det
   a22=a22/det
   camps=complexarr(2*kmaxx,2*kmaxy,2)

   if keyword_set(guess) then begin
      vf=fft(guess(*,*,0),-1)
      vf=shift(vf,kmaxx,kmaxy)
      camps(0,0,0)=vf(0:2*kmaxx-1,0:2*kmaxy-1)
      vf=fft(guess(*,*,1),-1)
      vf=shift(vf,kmaxx,kmaxy)
      camps(0,0,1)=vf(0:2*kmaxx-1,0:2*kmaxy-1)
      endif else begin
      camps(0,0,0)=a22*bc(*,*,0)-a12*bc(*,*,1); the answer for diagonal system
      camps(0,0,1)=-a12*bc(*,*,0)+a11*bc(*,*,1)
      endelse

   db=dcomplex(bc)
   errmax=1.0
   iter=0
   jj=replicate(1,2*kmaxx)#(2*kmaxy-indgen(2*kmaxy))  ; use offsets instead of
   ii=(2*kmaxx-indgen(2*kmaxx))#replicate(1,2*kmaxy)  ; computing full matrix
   error=dcomplexarr(2*kmaxx,2*kmaxy,2)
   correct=complexarr(2*kmaxx,2*kmaxy,2)
   while (errmax gt tol) and (iter le niter) do begin
      iter=iter+1
      for l=0,2*kmaxy-1 do for k=0,2*kmaxx-1 do begin
        error(k,l,0)=db(k,l,0)-total(mxxf(k+ii,l+jj)*camps(*,*,0))-$
          total(mxyf(k+ii,l+jj)*camps(*,*,1))
        error(k,l,1)=db(k,l,1)-total(mxyf(k+ii,l+jj)*camps(*,*,0))-$
          total(myyf(k+ii,l+jj)*camps(*,*,1))
        endfor
      correct(0,0,0)=a22*error(*,*,0)-a12*error(*,*,1)
      correct(0,0,1)=-a12*error(*,*,0)+a11*error(*,*,1)
      camps=camps+correct
      errmax=max(abs(correct))
      if keyword_set(verbose) then print,'at iteration',iter,' max correction =',errmax
      endwhile
   if errmax gt tol then begin
      print,'opflow3d failed to converge after ',strtrim(niter,2),$
        ' iterations. Maximum error = ',strtrim(errmax,2)
      return,0
      endif
      print,'opflow3d converged after ',strtrim(iter,2),$
        ' iterations. Maximum error = ',strtrim(errmax,2)

   endif else begin ; use direct method

; generate the right hand side 
   b=fltarr(2*kmaxx,2*kmaxy,2,2) 
   b(0,0,0,0)=float(bc)
   b(0,0,0,1)=imaginary(bc)

; generate the full matrix
   matc=complexarr(2*kmaxx,2*kmaxy,2,2*kmaxx,2*kmaxy,2)
   for l=0,2*kmaxy-1 do for k=0,2*kmaxx-1 do begin   
    for j=0,2*kmaxy-1 do for i=0,2*kmaxx-1 do begin
      lmj=(l-j)+2*kmaxy
      kmi=(k-i)+2*kmaxx
      matc(i,j,0,k,l,0)=mxxf(kmi,lmj)
      matc(i,j,1,k,l,0)=mxyf(kmi,lmj)
      matc(i,j,0,k,l,1)=mxyf(kmi,lmj)
      matc(i,j,1,k,l,1)=myyf(kmi,lmj)
      endfor
     endfor
   length=8l*kmaxx*kmaxy
   matc=reform(matc,length,length)
   mat=fltarr(length,2,length,2)
   for i=0,length-1 do begin
     mat(0,0,i,0)=float(matc(*,i))
     mat(0,1,i,0)=-imaginary(matc(*,i))
     mat(0,0,i,1)=imaginary(matc(*,i))
     mat(0,1,i,1)=float(matc(*,i))
     endfor

; use least squares to solve the system
   length=16l*kmaxx*kmaxy
   mat=reform(mat,length,length)
   b=reform(b,length)
 ;  svdc,mat,w,u,v
 ;  sigma2=total((v*w)^2)
 ;  coef=reform(svsol(u,w,v,b),2*kmaxx,2*kmaxy,2,2)
   coef=reform(la_least_squares(mat,b,method=direct-1),2*kmaxx,2*kmaxy,2,2)
   camps=complexarr(2*kmaxx,2*kmaxy,2)
   camps(0,0,0)=coef(*,*,*,0)+complex(0,1)*coef(*,*,*,1)
   endelse

; now use the complex amplitudes to compute the velocity
v=fltarr(nx,ny,2)
vf=complexarr(nx,ny)
vf(0,0)=camps(*,*,0)
vf=shift(vf,-kmaxx,-kmaxy)
v(0,0,0)=float(fft(vf,1))
vf=vf*0
vf(0,0)=camps(*,*,1)
vf=shift(vf,-kmaxx,-kmaxy)
v(0,0,1)=float(fft(vf,1))

return,v
end
