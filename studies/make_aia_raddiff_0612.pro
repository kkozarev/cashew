pro make_aia_raddiff_0612,radData,indices,wav=wav,outpath=outpath
;This program makes radially-filtered images.
;Original code from Alec Engell.
;Kamen Kozarev Oct-Nov, 2010


set_plot,'z'

loadct,3,/silent

if not keyword_set(wav) then wav='193'

wdef,1,1024,1024
restore,filename = 'vars.sav'


inpath='/home/kkozarev/Desktop/AIA/limbCMEs/20100612/'+wav+'A/'
outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06122010/results/'+wav+'/'
date=['2010','06','12']
hours=['00','01']
hmins=[['5'],['0']]
xrange=[3072,4095]
yrange=[2548,3571]



;======SPECIAL IMPLEMENTATION FOR 06122010 EVENT=======================
dirpath=inpath
for h=0,n_elements(hours)-1 do begin
   for m=0,n_elements(hmins[*,0])-1 do begin
      if m eq 0 and h eq 0 then begin
    ;print,dirpath+'aia_test.lev1.'+wav+'A_'+date[0]+'-'+date[1]+'-'+date[2]+'T'+hours[h]+'_'+hmins[m,h]+'*.fits'
	files=find_file(dirpath+'aia_test.lev1.'+wav+'A_'+date[0]+'-'+date[1]+'-'+date[2]+'T'+hours[h]+'_'+hmins[m,h]+'*.fits')
      endif else begin
	files=[files,find_file(dirpath+'aia_test.lev1.'+wav+'A_'+date[0]+'-'+date[1]+'-'+date[2]+'T'+hours[h]+'_'+hmins[m,h]+'*.fits')]
      endelse
   endfor
endfor
;======SPECIAL IMPLEMENTATION FOR 06122010 EVENT=======================


nfiles=n_elements(files)

radData=dblarr(nfiles,1024,1024)

mreadfits,files[0],index,tmp
indices=replicate(index,nfiles)

nrad = arad
nmin = amin
nmax = amax
newgrad = xIavgrad

sub =1

g =ulong(0)


   rring = 1.00005       ; inner radius of "occulting disk"
   nr    = 250        ; number of radial "bins" between Sun & outer corners

;=============================================


;mreadfits,files[0],in00,da00
;index2map,in00,da00,map
;sub_map,map,smap,xrange=xrange,yrange=yrange
;maps=replicate(smap,nfiles)
;maps[0]=smap

for k=g , nfiles-1, sub DO BEGIN
   loadct,3,/silent
   tvlct,r,g,b,/get
   file = files[k]
   
;===========================================================
;2. Load the files
;===========================================================
   mreadfits,file,in00,da00
   copy_struct_inx,in00,indices,index_to=k

;   index2map,in00,da00,map
;   sub_map,map,smap,xrange=xrange,yrange=yrange
;   maps[k]=smap
   print,'Read frame #'+strtrim(string(k+1),2)+' out of '+strtrim(string(nfiles),2)
;===========================================================
   
   
   dataSUM0 = da00
   dataSUM = dataSUM0           ; - dataSUM0
   
   
   
   
;================define=====================
   XCpix = double(in00.crpix1) 
   YCpix = double(in00.crpix2)
   RSpix = double(in00.r_sun)
   
   dataCOR = dataSUM
   datamax = 15000              ;60000;for 4 imgs50000;100000;max(dataSUM)
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
         rnow[i,j] = sqrt(xsun[i]^2 + ysun[j]^2)
         if (rnow[i,j] le rring) then dataCOR[i,j] = 0
      endfor
      if ((i mod 50) eq 0) then print,' coronagraph masking ',i,' / ',(nx-1)
   endfor
   
   
;=============radial filter====================
   
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
   xImaxfit  = xIavgrad * 3.5   ;2.0;1.4;for 4 images:2.0
   
   coefdummy = poly_fit(arad,amin,5,aminfit)
   xIminfit  = 10.^aminfit
   
; renormalize image to bring out the details
   
   xInew  = dblarr(nx,ny)
   
   for i=0,nx-1 do begin
      for j=0,ny-1 do begin
         if (rnow[i,j] ge rmin) then begin
            finow = coefR[0] + coefR[1]*rnow[i,j]
            inow  = fix(finow+0.5)
            if (inow lt 0) then inow = 0
            if (inow gt (nr-1)) then inow = nr-1
            xInew[i,j] = (dataCOR[i,j] - xIminfit[inow]) / $
                         (xImaxfit[inow] - xIminfit[inow])
         endif
      endfor
      if ((i mod 50) eq 0) then print,' making renormalized img ',i,' / ',(nx-1)
   endfor
   
;==========add back disk===============
   disk = xInew*0
   
   
   for i=0,nx-1 do begin
      for j=0,ny-1 do begin
         rnow[i,j] = sqrt(xsun[i]^2 + ysun[j]^2)
         if (rnow[i,j] le rring) then begin
            disk[i,j] = (da00[i,j]) 
         endif
      endfor
      if ((i mod 50) eq 0) then print,' adding disk center ',i,' / ',(nx-1)
   endfor
   
;stop
   
   xIcutLO = 0.02
   xIcutHI = 0.95
   
;=============naming image====================
   
   
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
   
   
   xInew = xInew                ;[3072:4095,768:1791]
   disk = alog(disk)
   disk = disk                  ;[3072:4095,768:1791]
; if it is a diff image then these should be for example -1 as a min
; and +1 for a max
   
   
;For 171 (corona):
   if wav eq '171' then begin
      cmini = -0.2
      cmaxi = 0.01
      dmini = 4.0
      dmaxi = 8.5
   endif
   
;For 193 (corona):
   if wav eq '193' then begin
      cmini = -0.1
      cmaxi = 0.05
      dmini = 4.0
      dmaxi = 8.5
   endif
   
;For 211 (corona):
   if wav eq '211' then begin
      cmini = -0.1
      cmaxi = 0.01
      dmini = 3.0
      dmaxi = 7.5
   endif
   
   if wav eq '335' then begin
      cmini = -0.3
      cmaxi = 0.2
      dmini = 3.0
      dmaxi = 7.5
   endif
   
   
                                ;mini = -.1
                                ;maxi = 0.3
   xInew = bytscl(xInew,min=cmini,max=cmaxi)
   
                                ;mini = 4.0
                                ;maxi = 8.5
   disk = bytscl(disk,min=dmini,max=dmaxi)
   
   dist_circle,im,[4096,4096]
   
   new_image = (disk)
   inner = where(im le RSpix)
   new_image[inner] = disk[inner]
   
   outer = where(im gt RSpix)
   new_image[outer] = xInew[outer]
   
;new_image = rebin(new_image,1024,1024)
   
   ;tv,new_image[xrange[0]:xrange[1],yrange[0]:yrange[1]]
   
   
   radData[k,*,*]=new_image[xrange[0]:xrange[1],yrange[0]:yrange[1]]
   
                                ; xyouts,0.03,0.97,f3,/norm,size=2
                                ; xyouts,0.72,0.02,'AIA NASA SAO LMSAL',/norm,size=2
                                ; a2 = tvrd()
   ;filename = 'AIArad_'+wav+'_'+strtrim(string(k+1000),2)+'.png'
   ;write_png,outpath+filename,tvrd(),r,g,b
   
   dummy = temporary(in00)

  ; maps[k].data=reform(new_image[xrange[0]:xrange[1],yrange[0]:yrange[1]])
   

endfor
;================================================


;===========================================================
;3. Save all the data so I can go back and change the profiles
if keyword_set(outpath) then begin
   save,radData,indices,filename=outpath+'radData_'+date[1]+date[2]+'_'+wav+'.sav'
   endif
;===========================================================


set_plot,'x'
end
