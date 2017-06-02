pro aia_cfa_teem_total,files,arcoords,wave_,npix,q94,teem_table,teem_map,teem_tot,event=event,mask=mask
;+
; Project     : AIA/SDO
;
; Name        : AIA_TEMPMAP 
;
; Category    : Data analysis   
;
; Explanation : calculates temperature map
;		based on single-Gaussian fit
;
; Syntax      : IDL>aia_teem_total,fileset,fov,wave_,teem_map
;
; Inputs      : fileset  = strarr(6) with filenames of 6 wavelength FITS images
;		fov[4]   = [x1,y1,x2,y2] field-of-view in solar radii
;               npix     = macropixel size 
;		wave_	 = strarr(6) with wavelengths in Angstroem
;               q94      = empirical correction factor of 94 A response
;               vers     = lavel in filename for each FOV (e.g., vers='a')
;		mask 	= given a mask, return the dem only from within it.
;
; Outputs     : teem_tot = savefile containing total DEM and fluxes
;
; History     :  9-Mar-2011, Version 1 written by Markus J. Aschwanden
; 	      : 10-May-2011, Version 2 added mask keyword by Steven Christe
;               12-May-2011, check level nr, exposure normalization (Ding Yuan)
;               18-May-2011, change units of FOV from pixels to solar radii
;		11-Jun-2011, replace TEEM_MAP and TEEM_TOT by VERS 
;               14-Aug-2014, Changed to work with the CfA archive of
;               data (Kamen A. Kozarev)
;
; Contact     : aschwanden@lmsal.com
;-

;_____________________TOTAL FLUX_________________________________________

nwave	=n_elements(wave_)
flux_	=fltarr(nwave)

for iw=0,nwave-1 do begin
 ;searchstring=fileset+'*'+wave_(iw)+'*'
 ;file_iw=file_search(searchstring,count=nfiles)
 
 read_sdo,files[iw],index0,data0,/uncomp_delete
 aia_prep,index0,data0,index,data

 texp    =index.exptime
 level   =index.lvl_num                  ;check level 1.0 or 1.5 number
 cdelt1  =index.cdelt1
 crpix1  =index.crpix1
 crpix2  =index.crpix2
 rsun    =index.rsun_obs
 rpix    =rsun/cdelt1
;ADDED BY Kamen A. Kozarev, 03/19/2012
 newcoords=aia_autoselect_subroi(index,arcoords,event=event)
 i1=newcoords[0]
 i2=newcoords[2]
 j1=newcoords[1]
 j2=newcoords[3]
; i1      =long(fov[0]*rpix+crpix1+0.5)
; i2      =long(fov[2]*rpix+crpix1+0.5)
; j1      =long(fov[1]*rpix+crpix2+0.5)
; j2      =long(fov[3]*rpix+crpix2+0.5)
 print,'FOV in pixels [i1,j1,i2,j1]=',i1,j1,i2,j2
 if (level ge 1.5) then texp=1.0        ;already normalized
 flux_[iw]=total(data[i1:i2,j1:j2])/texp
 print,wave_[iw],flux_[iw]
endfor

;_____________________AIA RESPONSE FUNCTION________________________
restore,teem_table      ;-->wave_,q94,area,resp_corr,telog,dte,tsig,flux
nte	=n_elements(telog)

;_____________________READ DEM PER PIXEL__________________________ 
restore,teem_map	;-->te_map,em_map,sig_map,chi_map,dateobs 
dim	=size(em_map)
nx	=dim[1]
ny	=dim[2]
IF NOT keyword_set(mask) THEN mask = replicate(1,nx,ny)
em_tot	=fltarr(nx,ny)
em	=fltarr(nte)
te	=fltarr(nte)
sig	=fltarr(nte)
;nmacro	=float(nx)*float(ny)
;em_tot = 
for j=0,ny-1 do begin
 for i=0,nx-1 do begin
    em(*)  =mask[i,j]*10.^em_map[i,j] ;log(EM)-->EM
    te(*)  =te_map[i,j]*mask[i,j]     ;log(te)
    sig(*) =sig_map[i,j]*mask[i,j]
    em_tot[i,j] =int_tabulated(10^telog,em*exp(-(telog-te)^2/(2.*sig^2)),/double) ;/nmacro
 endfor
endfor
emlog	=alog10(em_tot)
;clearplot
;window,0,xsize=512,ysize=256
;plot,telog,emlog,yrange=minmax(emlog),xtitle='Temperature  log(T)',$
;   ytitle='Emission measure  log(EM [cm!U-5!N K!U-1!N])'

;______________________TOTAL FLUX FROM DEM________________________
;flux_dem=fltarr(nwave)
;qflux	=fltarr(nwave)
;for iw=0,nwave-1 do begin
; flux_dem[iw]=total(resp_corr[*,iw]*em_tot*dte)*npix^2;*nmacro
; qflux[iw]   =flux_dem[iw]/flux_[iw]
; print,flux_[iw],flux_dem[iw],qflux[iw]
;endfor

;______________________SAVE RESULTS_______________________________
save,filename=teem_tot,telog,emlog;,flux_,flux_dem,qflux
end
