function get_coronal_density, rad, sg99=sg99, ma08=ma08
;PURPOSE:
;This procedure calculates the coronal electron number density in cm^-3
;based on input radial distance, according to different models.
;
;CATEGORY:
; AIA/Ionization
;
;INPUTS:
;     RAD - radial distance from Sun center, in solar radii.
;KEYWORDS:
;      SG99 - density model from Sittler and Guhathakurta (1999)
;      MA08 - density model from Mancuso and Avetta (2008)
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 10/31/2011
;
  RSUN=6.955e5                  ;Solar radius in km
  if n_params() eq 0 then begin
     print,''
     print,'You need to supply the radial distance(s) from Sun center, in Rs.'
     print,'For example: IDL> result = get_coronal_density(1.3)'
     print,''
     return,-1
  endif
  
  if keyword_set(SG99) then begin
     n0=1.0e9
     y=rad-1.
     z=1./(1.+y)
     a1=0.001272
     a2=4.8039
     a3=0.29696
     a4=-7.1743
     a5=12.321
     dens=n0*a1*z^2*exp(a2*z)*(1+a3*z+a4*z^2+a5*z^3)
  endif else begin
     if keyword_set(MA08) then begin
        dens=1.0e8*(2.99/rad^16 + 1.55/rad^6 + 2.1/rad^4 + 0.08/rad^2.5)
     endif else begin
        ;Use the 1-fold Newkirk (1961) model
        a=1.0
        n0=4.2e4
        rs=6.958e5
        dens=a*n0*10^(4.32*rs/(rad*RSUN))
     endelse
  endelse
  
  return,dens
end
