pro make_aia_diff_coro_kak,wav=wav

set_plot,'z'

loadct,3

new_files = '/tmp/'
outpath = '/Users/Shared/aia/coronagraph/2010/09/'

nfiles = ulong(n_elements(files))
if not keyword_set(wav) then wav='193'


wdef,1,1024,1024


restore,filename = 'vars.sav'
restore,filename= '/home/kkozarev/Desktop/AIA/limbCMEs/06122010/results/193/regionData_0612_'+wav+'.sav'

nrad = arad
nmin = amin
nmax = amax
newgrad = xIavgrad

sub =4

g =ulong(0)


p = 38

   rring = 1.00005       ; inner radius of "occulting disk"
   nr    = 250        ; number of radial "bins" between Sun & outer corners

;=============================================

for k=g , nfiles-1, sub DO BEGIN
print,k
print,wav
print,nfiles
print, nfiles -k
loadct,3

data=totaldata[k,*,*]

stop

dataSUM0 = da00+da01+da02+da03+da04+da05 + da06
;dataSUM1 = da04+da05+da06+da07;+da08+da09

dataSUM = dataSUM0; - dataSUM0




;================define=====================
XCpix = double(in00.crpix1) 
YCpix = double(in00.crpix2)
RSpix = double(in00.r_sun)

dataCOR = dataSUM
datamax = 15000;60000;for 4 imgs50000;100000;max(dataSUM)
print,"data max of summed images is:"
print,datamax

   nx = 4096
   ny = 4096

   xsun  = (dindgen(nx)-XCpix)/RSpix
   ysun  = (dindgen(ny)-YCpix)/RSpix

    rmin = rring
    xmax = max(abs(xsun))
    ymax = max(abs(ysun))
    rmax = sqrt(xmax^2 + ymax^2)
    rad  = dindgen(nr)/double(nr-1)*(rmax-rmin)+rmin

    findexrad = dindgen(nr)
    coefR = poly_fit(rad,findexrad,1)

    xIavgrad = dblarr(nr)
    xIminrad = dblarr(nr)+1.1*datamax
    xImaxrad = dblarr(nr)
    fnumrad  = dblarr(nr)

; ===========corono =====================


rnow = dblarr(nx,ny)

   for i=0,nx-1 do begin
    for j=0,ny-1 do begin
      rnow(i,j) = sqrt(xsun(i)*xsun(i) + ysun(j)*ysun(j))
      if (rnow(i,j) le rring) then dataCOR(i,j) = 0
      ;if (dataCOR(i,j) lt 0.) then dataCOR(i,j) = 0.0
    endfor
    if ((i mod 50) eq 0) then print,' coronagraph masking ',i,' / ',(nx-1)
   endfor


;=============radial filter====================



;    for i=0,nx-1 do begin
;     for j=0,ny-1 do begin
;       if (rnow(i,j) ge rmin) then begin
;         finow = coefR(0) + coefR(1)*rnow(i,j)
;         inow  = fix(finow+0.5)
;         if (inow lt 0) then inow = 0
;         if (inow gt (nr-1)) then inow = nr-1
;         xIavgrad(inow) = xIavgrad(inow) + dataCOR(i,j)
;         fnumrad(inow)  = fnumrad(inow) + 1.
;         if (dataCOR(i,j) gt xImaxrad(inow)) then xImaxrad(inow)=dataCOR(i,j)
;         if (dataCOR(i,j) lt xIminrad(inow)) then xIminrad(inow)=dataCOR(i,j)
;       endif
;     endfor
;     if ((i mod 50) eq 0) then print,' assembling radial mean ',i,' / ',(nx-1)
;    endfor

 ;   for inow=0,nr-1 do begin
 ;     if (fnumrad(inow) ne 0.) then $
 ;       xIavgrad(inow) = xIavgrad(inow) / fnumrad(inow)
 ;     if (xIminrad(inow) lt 3.) then xIminrad(inow) = 3.
 ;  endfor

; created fitted/smoothed versions of max/min, for use in thresholding.
; *** some choice and/or discretion is required!

 ;   arad = alog10(rad)
;stop
  ;  amin = alog10(xIminrad)
;stop
  ;  amax = alog10(xImaxrad)
;stop

;stop

    arad = nrad
    amin = nmin
    amax = nmax 

    xIavgrad = newgrad

; nmin = fltarr(250)
; nmin[0] = 3.57
; for i = 0, 248 do nmin(i+1) = nmin(i) - .0125

; nmax = fltarr(250)
; nmax[0] = 4.69
; for i = 0, 248 do nmax(i+1) = nmax(i) - .013


;   coefdummy = poly_fit(arad,amax,5,amaxfit)
;   xImaxfit  = 10.^amaxfit
    xImaxfit  = xIavgrad * 3.5;2.0;1.4;for 4 images:2.0

    coefdummy = poly_fit(arad,amin,5,aminfit)
    xIminfit  = 10.^aminfit

; renormalize image to bring out the details

    xInew  = dblarr(nx,ny)

    for i=0,nx-1 do begin
     for j=0,ny-1 do begin
       if (rnow(i,j) ge rmin) then begin
         finow = coefR(0) + coefR(1)*rnow(i,j)
         inow  = fix(finow+0.5)
         if (inow lt 0) then inow = 0
         if (inow gt (nr-1)) then inow = nr-1
         xInew(i,j) = (dataCOR(i,j) - xIminfit(inow)) / $
                      (xImaxfit(inow) - xIminfit(inow))
       endif
     endfor
     if ((i mod 50) eq 0) then print,' making renormalized img ',i,' / ',(nx-1)
    endfor

;==========add back disk===============
disk = xInew*0


   for i=0,nx-1 do begin
    for j=0,ny-1 do begin
      rnow(i,j) = sqrt(xsun(i)*xsun(i) + ysun(j)*ysun(j))
      if (rnow(i,j) le rring) then begin
         disk(i,j) = (da00(i,j)) 
      endif
      ;if (dataCOR(i,j) lt 0.) then dataCOR(i,j) = 0.0
    endfor
    if ((i mod 50) eq 0) then print,' adding disk center ',i,' / ',(nx-1)
   endfor

;stop

    xIcutLO = 0.02
    xIcutHI = 0.95

;=============naming image====================

        year = strmid(file_171,3,4)
        mo = strmid(file_171,7,2)
        day = strmid(file_171,9,2)
        date = year +'/' +mo + '/' + day

        hour = strmid(file_171,12,2)
        min = strmid(file_171,14,2)
        sec = strmid(file_171,16,2)
        time = hour + ':' + min + ':' + sec

        ;data_infiles = rotate(data_infiles,7)
        ;data_infiles = alog(rebin(data_infiles,1024,1024))
        ;data_infiles = sqrt(rebin(data_infiles,1024,1024))
        ;data_infiles = -(data_infiles)
        
        

        ;a = bytscl(data_infiles,min=-2500,max=130)
        ;a = (data_infiles)
        ;pmm,a
        ;mini =-7.2
        ;maxi = -4.58
        ;xInew = bytscl(xInew,min=mini,max=maxi)
       ; xInew = rebin(xInew,1024,1024)
       ; disk = rebin(alog(disk),1024,1024)

   ;     xInew = rebin(xInew,1024,1024)
    ;    disk = rebin(alog(disk),1024,1024)





        xInew = xInew;[3072:4095,768:1791]
        disk = alog(disk)
        disk = disk;[3072:4095,768:1791]
; if it is a diff image then these should be for example -1 as a min
; and +1 for a max
        mini = -.10;-.4; for 4 img: -.15
        maxi = 1.50;1.0;1.25; for 4 img: 1.35;.85
        xInew = bytscl(xInew,min=mini,max=maxi)

        mini = 4.0
        maxi = 8.5
        disk = bytscl(disk,min=mini,max=maxi)

dist_circle,im,[4096,4096]

new_image = (disk)
inner = where(im le RSpix)
new_image[inner] = disk[inner]

outer = where(im gt RSpix)
new_image[outer] = xInew[outer]

new_image = rebin(new_image,1024,1024)

        ;new_image = new_image[3072:4095,768:1791]
        ;new_image = new_image[3072:4095,3072:4095]

; fov for 6/13 main image
        ;new_image = new_image[3000:3999,3000:3999]
; fov for zoom images 6/13
        ;new_image = new_image[3000:3799,3100:3899]

;fov for 8/13
        ;new_image = new_image[3096:4095,400:1399]
        ;new_image = new_image[2896:4095,400:1599]

;fov for 8/14
        ;new_image = new_image[3096:4095,1500:2499]

;fov for 7/28
        ;new_image = new_image[0:999,300:1299]

;for 7/27
        ;new_image = new_image[0:1535,512:3583]

;for 7/17
        ;new_image = new_image[0:4095,2048:4095]

        ;combo = xInew + disk
        ;tv, combo
        tv,new_image
        ;tv,disk
        ;tv,xInew



;stop
        ;tv,a
        xyouts,0.82,0.97,date,/norm,size=2
        xyouts,0.82,0.94,time,/norm,size=2
xyouts,0.95,0.94,'UT',/norm,size=2

;xyouts,0.03,0.96,date,/norm,size=3
;xyouts,0.03,0.95,time,/norm,size=3


;for 1200x1200
;xyouts,0.67,0.96,date,/norm,size=3
;xyouts,0.67,0.92,time,/norm,size=3
;xyouts,0.84,0.92,'UT',/norm,size=3

;xyouts,0.67,0.98,date,/norm,size=5
;xyouts,0.67,0.94,time,/norm,size=5
;xyouts,0.84,0.94,'UT',/norm,size=5

        xyouts,0.03,0.97,f3,/norm,size=2
        xyouts,0.72,0.02,'AIA NASA SAO LMSAL',/norm,size=2
        a2 = tvrd()
        filename = f1+f2+f3+'.gif'
        write_gif,outpath+filename,a2,r,g,b

dummy = temporary(in00)
dummy = temporary(in01)
dummy = temporary(in02)
dummy = temporary(in03)
dummy = temporary(in04)
dummy = temporary(in05)
;dummy = temporary(in06)
;dummy = temporary(in07)
;dummy = temporary(in08)
;dummy = temporary(in09)

;stop
     endfor
;================================================


set_plot,'x'

end
