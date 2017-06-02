function aia_theoretical_ionization_times,temperature,density,hotband,rates=rates
;PURPOSE:
;This procedure allows a user to determine ionization timescales
;from theoretical ionization rates based on CHIANTI.
;
;CATEGORY:
; AIA/Ionization
;
;INPUTS:
;     TEMPERATURE - coronal temperature, in Kelvin
;     HOTBAND - the hottest AIA band to which we ionize
;     DENSITY - the local coronal density, in cm^-3
;KEYWORDS:
;     RATES - returns the rates
;
;OUTPUTS:
;
;
;DEPENDENCIES:
;            ioniz_rate (CHIANTI)
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 12/02/2013
;
  if n_params() lt 3 then begin
     print,''
     print,'You need to supply a Temperature, Wavelength, Density:'
     print,' IDL> result = aia_theoretical_ionization_times(2.0e6,"211",1.0e8)'
     print,''
     return, -1
  endif
  
  if not keyword_set(hotband) then hotband='211'

;Determin which ionization rates to use here:
  case hotband of
     '211': ions=['fe_11','fe_12']
     '335': ions=['fe_11','fe_12','fe_13','fe_14']
     else: ions=['fe_11','fe_12']
  endcase
  
  res=0.0
  rates=dblarr(n_elements(ions))
  for io=0,n_elements(ions)-1 do begin
     rates[io]=ioniz_rate(ions[io],temperature)
     res+=(1./rates[io])
  endfor
  res/=(density*1.0D)
  
return,res
end
