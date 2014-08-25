pro aia_cfa_teem_table,files,wave_,tsig,te_range,q94,teem_table
;+
; Project     : AIA/SDO
;
; Name        : AIA_DEM_LOOKUP  
;
; Category    : Data analysis   
;
; Explanation : calculates AIA fluxes for 6 wavelengths (WAVE_)
;		for single-Gaussian DEM distributions with
;		TEMIN < TE < TEMAX, DTE1 < DTE < DTE2
;
; Syntax      : IDL>aia_dem_table,wave_,tsig,te_range,q94,fileset,tem_table
;
; Inputs      : wave_	 = strarr(6) with wavelengths in Angstroem
;
; Outputs     : postscript file <plotname>_col.ps (if io=2)
;
; History     :  3-Mar-2011, Version 1 written by Markus J. Aschwanden
;
; Dependencies: aia_get_response, read_sdo, aia_prep
;
; Contact     : aschwanden@lmsal.com
;-

;_____________________AIA RESPONSE FUNCTION________________________
tresp   =aia_get_response(/temp,/full,/phot)
telog_  =tresp.logte
telog1	=alog10(te_range(0))
telog2	=alog10(te_range(1))
ind_te	=where((telog_ ge telog1) and (telog_ le telog2),nte)
telog	=telog_(ind_te)
nwave	=n_elements(wave_)
ichan_  =fltarr(nwave)
resp    =fltarr(nte,nwave)
for iw=0,nwave-1 do begin
 filter ='A'+wave_(iw)
 if (wave_(iw) eq '094') then filter='A94'
 ichan  =where(tresp.channels eq filter)
 resp_  =tresp.tresp(*,ichan)
 resp(*,iw)=resp_(ind_te)
endfor
 
;_____________________EMPIRICAL CORRECTION 94 A____________________
ind1    =where(telog le 6.3)
resp_corr=resp
resp_corr(ind1,5)=resp(ind1,5)*q94

;_____________________PIXEL SIZE___________________________________
;searchstring=fileset+'*'+wave_(0)+'.fits'
;file_iw=file_search(searchstring,count=nfiles)
print,files[0]
read_sdo,files[0],index0,data0,/uncomp_delete
aia_prep,index0,data0,index,data


cdelt1 =index.cdelt1
dsun   =index.dsun_obs
arcsec=2.*!pi*dsun/(1.e3*360.*60.*60.)         ;1 arcsec in km
pix    =cdelt1*arcsec*1.e5                     ;pixel in cm
area   =pix^2                                  ;pixel area in cm^2

;_____________________CALCULATES LOOPUP TABLES_____________________
dte1	=10.^telog(1:nte-1)-10.^telog(0:nte-2)
dte	=[dte1(0),dte1]
em1	=1.
nsig	=n_elements(tsig)
flux	=fltarr(nte,nsig,nwave)
for i=0,nte-1 do begin
 for j=0,nsig-1 do begin
  em_te =em1*exp(-(telog-telog(i))^2/(2.*tsig(j)^2))
  for iw=0,nwave-1 do flux(i,j,iw)=total(resp_corr(*,iw)*em_te*dte)
 endfor
endfor
save,filename=teem_table,wave_,q94,area,resp_corr,telog,dte,tsig,flux
print,'Lookup table created : ',teem_table
end
