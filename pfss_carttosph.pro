pro pfss_carttosph,x,y,z,l,b,r,theta,phi,degrees=degrees
;PURPOSE:
;Convert between cartesian and spherical positions. 
;Angles by default in radians. l/b are the lon/lat of the subsolar point.
;
;CATEGORY:
; PFSS
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev,  
;

;Kamen Kozarev, 11/04/2011

  r = sqrt(x^2+y^2+z^2)
  tmp=(sqrt(x^2+(y*cos(B)+z*sin(B))^2))/(-y*sin(B)+z*cos(B))
  theta = acos(tmp)
  phi   = L + asin(tmp)

  ;Fix weird angles
  ind=where(theta lt 0.0)
  if ind[0] ne -1 then theta[ind]+=2*!PI
  ind=where(theta ge 2*!PI)
  if ind[0] ne -1 then theta[ind]-=2*!PI
  
  ind=where(phi lt 0.0)
  if ind[0] ne -1 then phi[ind]+=2*!PI
  ind=where(phi ge 2*!PI)
  if ind[0] ne -1 then phi[ind]-=2*!PI
  

  if keyword_set(degrees) then begin
     theta*=180.0/!PI
     phi*=180.0/!PI
  endif
end
