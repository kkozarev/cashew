;+
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;
;  pfss_rad_field_crossing.pro - This procedure determines the latitude and
;                                longitude at which a set of field lines
;                                crosses a specified surface (the shock)
;
;  usage:  pfss_rad_field_crossing,rcut,data,interpindex=interpindex
;  where rcut = radial level to evaluate line crossings
;        data = [3,n] array of data where each of the n crossings has
;               [lineno,bcross,lcross] with (bcross,lcross)=(lat,lon)
;               coordinate in degrees of planar crossing and lineno=line
;               number of input field line arrays
;        interpindex = on output, an n-element array of interpolation
;                      coordinates giving the fractional gridpoint along each
;                      fieldline at which the nth crossing occurs
;
;             11/04/2011 - Kamen Kozarev - based on Marc DeRosa's pfss_rad_field_crossing.pro
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;-

pro pfss_shock_field_crossing,rcut,data,interpindex=interpindex

;; ;  common block
;; @pfss_data_block

;; ;  preliminaries
;; nlines=n_elements(nstep)
;; rmin=min(rix)
;; rmax=max(rix)

;; ;  check rcut
;; if n_elements(rcut) eq 0 then begin
;;   print,'  pfss_rad_field_crossing: rcut being set to ',rix(0)
;;   rcut=rix(0) 
;; endif else rcut=rmin>rcut(0)<rmax

;; ;  initialize data array
;; data=[-1.,-1.,-1.]
;; interpindex=[-1.0]

;; ;  loop through the individual field lines
;; for i=0,nlines-1 do begin

;;   ;  extract coordinates of current field line
;;   ns=nstep(i)
;;   lr=ptr(0:ns-1,i)
;;   lth=ptth(0:ns-1,i)
;;   lph=ptph(0:ns-1,i)
  
;;   ;  look for crossings only if rcut is in range
;;   lmin=min(lr)
;;   lmax=max(lr)

;;   ;  first find out if any points exactly match, and set them
;;   ixm=where(lr eq rcut,nmatch)
;;   if nmatch gt 0 then begin
;;     for j=0,nmatch-1 do begin
;;       data=[[data],[i,lth(ixm(j)),lph(ixm(j))]]
;;       interpindex=[interpindex,ixm(j)]
;;     endfor
;;   endif

;;   ;  now find crossings
;;   ixpt=where(((lr(0:ns-2)-rcut)*(lr(1:ns-1)-rcut)) lt 0,ncr)
;;   if ncr gt 0 then begin

;;     ;  linearly interpolate to get locus of crossing
;;     for j=0,ncr-1 do begin
;;       if total(ixm eq ixpt(j)) eq 0 then begin  ;  avoids double-counting
;;         coeff=(rcut-lr(ixpt(j)))/(lr(ixpt(j)+1)-lr(ixpt(j)))
;;         ptcrth=90-((1-coeff)*lth(ixpt(j))+coeff*lth(ixpt(j)+1))*!radeg
;;         ptcrph=(((1-coeff)*lph(ixpt(j))+coeff*lph(ixpt(j)+1))*!radeg+360) $
;;           mod 360
;;         data=[[data],[i,ptcrth,ptcrph]]      
;;         interpindex=[interpindex,ixpt(j)+coeff]
;;       endif
;;     endfor

;;   endif

;; endfor

;; ;  remove first row of data
;; if n_elements(data) eq 3 then begin
;;   print,'  pfss_rad_field_crossing:  no crossings detected'
;;   data=-1
;; endif else begin
;;   data=data(*,1:*)
;;   interpindex=interpindex(1:*)
;; endelse

end
