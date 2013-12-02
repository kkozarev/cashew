pro aia_cfa_teem_map,files,arcoords,wave_,npix,teem_table,teem_fname
;+
; Project     : AIA/SDO
;
; Name        : AIA_TEMPMAP 
;
; Category    : Data analysis   
;
; Explanation : calculates EM and Te temperature maps
;		based on single-Gaussian fits in each macropixel
;
; Syntax      : IDL>aia_teem_map,files,arcoords,wave_,npix,teem_table,teem_fname
;
; Inputs      : files  = filenames of the 6 wavelength FITS images
;		wave_	 = strarr(6) with wavelengths in Angstroem
;		arcoords[2] =  an array specifying the AR X-Y location in
;                              arcseconds, like [-200,350]. Added by Kamen
;                              A. Kozarev, 03/15/2012
;		npix	 = size of macropixel (spatial resolution)
;		teem_table = filename of DEM lookup table
;                            (calculated previously with AIA_TEEM_TABLE.PRO)
;               teem_fname = the filename in which to save the output information
;
; Outputs     : postscript file <plotname>_col.ps (if io=2)
;
; History     :  3-Mar-2011, Version 1 written by Markus J. Aschwanden
;               12-May-2011, adding MAP_MAKE and PLOT_MAP (Steven Christe)
;               12-May-2011, check level nr, exposure normalization (Ding Yuan)
;		18-May-2011, change units of FOV from pixels to solar radii
;                2-Nov-2011, units of level 1.5 data are DN 
;               19-Mar-2012, Changed to work with the CfA archive of
;               data (Kamen A. Kozarev)
;
; Contact     : aschwanden@lmsal.com
;-

;_________________________________________________________________________
;if (max(abs(fov)) gt 1.3) then STOP,'Specify FOV in units of solar radii'
t1	=systime(0,/seconds)
nwave	=n_elements(wave_)
;files	=strarr(6)
;for iw=0,nwave-1 do begin
; file_iw=file_search(searchstring,count=nfiles)
; files[iw]=file_iw[0]
; print,files[iw]
;endfor

;_____________________REBINNING IMAGES___________________________________
texp_	=fltarr(nwave)
for iw=0,nwave-1 do begin
 read_sdo,files[iw],index0,data0,/uncomp_delete
 aia_prep,index0,data0,index,data
 cdelt1  =index.cdelt1
 crpix1  =index.crpix1
 crpix2  =index.crpix2
 rsun    =index.rsun_obs
 rpix    =rsun/cdelt1

;ADDED BY Kamen A. Kozarev, 03/19/2012
 newcoords=aia_autoselect_subroi(index,arcoords)
 i1=newcoords[0]
 i2=newcoords[2]
 j1=newcoords[1]
 j2=newcoords[3]
; i1      =long(fov[0]*rpix+crpix1+0.5)
; i2      =long(fov[2]*rpix+crpix1+0.5)
; j1      =long(fov[1]*rpix+crpix2+0.5)
; j2      =long(fov[3]*rpix+crpix2+0.5)
 print,'FOV in pixels [i1,j1,i2,j1]=',i1,j1,i2,j2
 dim0	=size(data)
 nx0	=dim0[1]
 ny0	=dim0[2]
 if (i1 ge nx0) or (i2 ge nx0) or (j1 ge ny0) or (j2 ge ny0) then begin
  print,'FOV=',i1,i2,j1,j2
  print,'image size=',nx0,ny0
;  stop,'ERROR in subimage range i1,i2,j1,j2 for image with size nx0,ny0'
 endif
 image	=data[i1:i2,j1:j2]
 dateobs=index.date_obs
 texp   =index.exptime
 level  =index.lvl_num			;check level 1.0 or 1.5 number
 if (iw eq 0) then begin
  dim	=size(image)
  nx	=dim[1]
  ny	=dim[2]
  nxx	=(nx/npix)
  nyy	=(ny/npix)
  i3	=nxx*npix-1
  j3	=nyy*npix-1
  x	=i1+(npix*findgen(nxx)+0.5)
  y	=j1+(npix*findgen(nyy)+0.5)
  images=fltarr(nxx,nyy,nwave)
 endif
 
 if (npix eq 1) then images[*,*,iw]=float(image(0:nxx-1,0:nyy-1))/texp
 if (npix gt 1) then images[*,*,iw]=rebin(float(image[0:i3,0:j3]),nxx,nyy)/texp
 texp_[iw]=texp
endfor

;___________________PLOT the image as a MAP (Steven Christe)____________
id 	=strmid(index.TELESCOP,0,3) + '/' + strmid(index.instrume,0,3)
aia_lct, rr, gg, bb, wavelnth=index.WAVELNTH, /load
a 	=index.CROTA2
xcen   	=index.CRVAL1 + index.CDELT1*cos(a)*(0.5*(index.NAXIS1+1)-$
	 index.CRPIX1)-index.CDELT2*sin(a)*((index.NAXIS2+1)/2-index.CRPIX2)
ycen 	=index.CRVAL2 + index.CDELT1*sin(a)*((index.NAXIS1+1)*0.5-$
	 index.CRPIX1) + index.CDELT2*cos(a)*((index.NAXIS2+1)*0.5-index.CRPIX2)
map 	=make_map(data, time = index.t_obs, id = id, dur = index.exptime,$
	 xc = xcen, yc = ycen, dx = index.cdelt1, dy = index.cdelt2)
;window,0,xsize=768,ysize=768
;plot_map, map, /limb, /log_scale

;________________________TEMPERATURE MAP_________________________________ 
restore,teem_table	;-->wave_,q94,area,resp_corr,telog,dte,tsig,flux
dim	=size(flux)
nte	=dim[1]
nsig	=dim[2]
nwave	=dim[3]
ntot	=nte*nsig
te_map	=fltarr(nxx,nyy)
em_map	=fltarr(nxx,nyy)
sig_map	=fltarr(nxx,nyy)
chi_map	=fltarr(nxx,nyy)
chi_map6=fltarr(nxx,nyy,6)
te_best	=0.
em_best	=0.
sig_best=0.
r0	=0.95*(4096/2)
x0	=4096/2
y0	=4096/2
nfree	=3

for j=0,nyy-1 do begin
 if (nx eq 4096) then ind=where(sqrt((x-x0)^2+(y(j)-y0)^2) le r0,nind)
 if (nx lt 4096) then ind=findgen(nxx)
 i1	=min(ind) > 0
 i2	=max(ind) < (nxx-1)
 for i=i1,i2 do begin
  flux_obs=reform(images[i,j,*])
  counts=flux_obs*texp_
  noise=sqrt(counts)/texp_
  chimin=9999.
  chi6min=9999.
  for k=0,nte-1 do begin
   for l=0,nsig-1 do begin
    flux_dem1=reform(flux(k,l,*))
    em1	=total(flux_obs)/total(flux_dem1)
    flux_dem=flux_dem1*em1
    chi	=sqrt(total((flux_obs-flux_dem)^2/noise^2)/(nwave-nfree))
    chi6=abs(flux_obs-flux_dem)/noise
    if (chi le chimin) then begin
     chimin	=chi
     chi6min	=chi6
     em_best	=alog10(em1)
     te_best	=telog(k)
     sig_best	=tsig(l)
    endif
   endfor 
  endfor
  em_map(i,j)=em_best
  te_map(i,j)=te_best
  sig_map(i,j)=sig_best
  chi_map(i,j)=chimin
  chi_map6(i,j,*)=chi6min
 endfor
 if (j mod 10) eq 0 then print,j,nyy
endfor

;________________________Making maps (Stephen Christe)____________
aia_map_cube = make_map( images[*,*,0], xc = xcen, yc = ycen, dx = index.cdelt1*npix, dy = index.cdelt2*npix, id = '', time = dateobs)
aia_map_cube = replicate(aia_map_cube, 6)

FOR i = 0, n_elements(wave_)-1 DO BEGIN
        aia_map_cube[i].data = images[*,*,i]
        aia_map_cube[i].id = 'SDO/AIA ' + num2str(wave_[i]) + ' A'
ENDFOR

temperature_map = make_map(te_map,xc = xcen,yc = ycen,dx = index.cdelt1*npix,$
	dy = index.cdelt2*npix, id = 'log(Temperature [MK])', time = dateobs)

emission_map = make_map(em_map, xc = xcen, yc = ycen, dx = index.cdelt1*npix,$
	dy=index.cdelt2*npix,id= 'log(Emission Measure [cm!U-3!N]', time = dateobs)

sigma_map = make_map( sig_map, xc = xcen, yc = ycen, dx = index.cdelt1*npix,$
	dy = index.cdelt2*npix, id = 'Sigma', time = dateobs)

chisq_map = make_map( chi_map, xc = xcen, yc = ycen, dx = index.cdelt1*npix,$
	dy = index.cdelt2*npix, id = 'Chi-square', time = dateobs)

;________________________STATISTICS OF CHI-2 FITS_________________
;print,'Statistics of chi2='
;statistic,chi_map
;print,'Statistics of chi2 (94 A)'
;statistic,chi_map6(*,*,5)
;print,'log(EM)-range = ',minmax(em_map)

;________________________SAVE MAPS________________________________

save,filename=teem_fname+'.sav',te_map,em_map,sig_map,chi_map,temperature_map,$
	emission_map,sigma_map,chisq_map,aia_map_cube, dateobs
print,'TE+EM maps saved in file : ',teem_fname
t2	=systime(0,/seconds)
cpu	=t2-t1
print,'Computation time = ',cpu,' s'
end
