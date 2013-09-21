pro pfss_sphtocart,r,theta,phi,l,b,x,y,z,degrees=degrees
;Convert between spherical and normalized cartesian positions. 
;Angles by default in radians. l/b are the lon/lat of the subsolar point.
;Kamen Kozarev, 11/04/2011

  if keyword_set(degrees) then begin
     theta*=!PI/180.0
     phi*=!PI/180.0
     l*=!PI/180.0
     b*=!PI/180.0
  endif

;Fix weird angles:
  ind=where(theta lt 0.0)
  if ind[0] ne -1 then theta[ind]+=2*!PI
  ind=where(theta ge 2*!PI)
  if ind[0] ne -1 then theta[ind]-=2*!PI
  
  ind=where(phi lt 0.0)
  if ind[0] ne -1 then phi[ind]+=2*!PI
  ind=where(phi ge 2*!PI)
  if ind[0] ne -1 then phi[ind]-=2*!PI
  
  
  x = r*sin(theta)*sin(phi-L)
  y =   r*sin(theta)*cos(phi-L)*cos(B) + r*cos(theta)*sin(B)
  z = - r*sin(theta)*cos(phi-L)*sin(B) + r*cos(theta)*cos(B)
end
