pro make_aia_diff_coro_0613,wav=wav

set_plot,'z'

loadct,3,/silent


if not keyword_set(wav) then wav='193'
inpath='/data/SDO/AIA/level1/'
outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06132010/results/'+wav+'/'
date=['2010','06','13']
hours=['H0005']
hmins=['053','054']
xrange=[3072,4095]
yrange=[824,1847]


dim=1024
dirpath=inpath+date[0]+'/'+date[1]+'/'+date[2]+'/'

;Currently, this only works for a single hour
for h=0,n_elements(hours)-1 do begin
   for m=0,n_elements(hmins)-1 do begin
      if m eq 0 then begin files=find_file(dirpath+hours[h]+'/*_'+hmins[m]+'*_0'+wav+'.fits')
      endif else begin
         files=[files,find_file(dirpath+hours[h]+'/*_'+hmins[m]+'*_0'+wav+'.fits')]
      endelse
   endfor
endfor

nfiles=n_elements(files)

;new_files = '/Users/Shared/Engell/coro/'
new_files = '/tmp/'
;outpath = '/Users/Shared/aia/coronagraph/2010/09/' ;proposal_figs/';0813/';
outpath = '/home/kkozarev/Desktop/AIA/limbCMEs/'+date[1]+date[2]+date[0]+'/results/'+wav+'/'

wdef,1,1024,1024

restore,filename = 'vars.sav'

nrad = arad
nmin = amin
nmax = amax
newgrad = xIavgrad

sub =1

g =ulong(0)

p = 38

   rring = 1.00005       ; inner radius of "occulting disk"
   nr    = 250        ; number of radial "bins" between Sun & outer corners

;=============================================





;nfiles=1
for k=g , nfiles-1, sub DO BEGIN
loadct,3,/silent
tvlct,r,g,b,/get
file = files[k]


;===========================================================
;2. Load the files
;===========================================================
   mreadfits,file,in00,da00
  ; mreadfits,files[k+1],in01,da01
   print,'Read frame #'+strtrim(string(k+1),2)+' out of '+strtrim(string(nfiles),2)
;===========================================================
;da00=da01-da00


f1 = strmid(in00.date_obs,0,10) ;YYYY-MM-DD
f2 = strmid(in00.date_obs,11,8) ;hh:mm:ss
f3 = wav

;print,file

dataSUM0 = da00;+da01+da02+da03+da04+da05 + da06
;dataSUM1 = da04+da05+da06+da07;+da08+da09

dataSUM = dataSUM0; - dataSUM0




;================define=====================
XCpix = double(in00.crpix1) 
YCpix = double(in00.crpix2)
RSpix = double(in00.r_sun)

dataCOR = dataSUM
datamax = 20000;15000;for 4 imgs50000;100000;max(dataSUM)
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
        xInew = xInew;[3072:4095,768:1791]
        disk = alog(disk)
        disk = disk;[3072:4095,768:1791]
; if it is a diff image then these should be for example -1 as a min
; and +1 for a max

;For 171 (corona):
if wav eq '171' then begin
cmini = -0.25
cmaxi = 0.25
dmini = 4.0
dmaxi = 8.5
endif

;For 193 (corona):
if wav eq '193' then begin
cmini = -0.25
cmaxi = 0.25
dmini = 4.0
dmaxi = 8.5
endif

;For 211 (corona):
if wav eq '211' then begin
cmini = -0.2
cmaxi = 0.05
dmini = 2.5
dmaxi = 7.0
endif

if wav eq '335' then begin
cmini = -0.4
cmaxi = 0.1
dmini = 1.0
dmaxi = 3.5
endif

        ;cmini = -0.3;-.4; for 4 img: -.15
        ;cmaxi = 0.2;1.0;1.25; for 4 img: 1.35;.85
        xInew = bytscl(xInew,min=cmini,max=cmaxi)

        ;dmini = 3.0
        ;dmaxi = 7.5
        disk = bytscl(disk,min=dmini,max=dmaxi)

dist_circle,im,[4096,4096]

new_image = (disk)
inner = where(im le RSpix)
new_image[inner] = disk[inner]

outer = where(im gt RSpix)
new_image[outer] = xInew[outer]



        ;combo = xInew + disk
        ;tv, combo
        tv,new_image[xrange[0]:xrange[1],yrange[0]:yrange[1]]
        ;tv,disk
        ;tv,xInew



        ;xyouts,0.03,0.97,wav,/norm,size=2
        ;xyouts,0.72,0.02,'AIA NASA SAO LMSAL',/norm,size=2
        ;a2 = tvrd()
        filename = 'testAIArad_'+wav+'_'+strtrim(string(k+1000),2)+'.png'
        write_png,outpath+filename,tvrd(),r,g,b
        ;filename = 'testAIArad_'+wav+'_'+strtrim(string(k+1000),2)+'.gif'
        ;write_gif,outpath+filename,a2,r,g,b



dummy = temporary(in00)
;stop
     endfor
;================================================


set_plot,'x'

end
