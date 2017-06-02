function aia_rad_height_limits,index,angle=angle,degarray=degarray,rsun=rsun
;PURPOSE:
;Calculate the limits to which the radial heights are physical in the
;images. This is useful when creating j-maps
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      
;
;KEYWORDS:
;      ANGLE: If the keyword angle is supplied (in degrees from solar north),
;             returns the height for that angle.
;      DEGARRAY: You can supply the degrees array (again in degrees)
;      rather than do the whole 360 degrees
;             
;
;OUTPUTS: Returns the height limit of the AIA field of view in arcsec
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 05/13/2014
;

;Angle array at 1 arcmin resolution
  degarr=findgen(360.*60.)/60.*!PI/180.
  ndeg=n_elements(degarr)
;Corresponding height array
  htarr=fltarr(ndeg)
  
;Pick some radius of the circle and the size of the square
  n=1.295*index[0].rsun_obs                       ;n is half the side of the square, in arcsec 1170 1220
  n=1.267*index[0].rsun_obs
  r=1.195*n                       ;r is the radius of the outer circle of good data 1370 1468
  ;r=1.01*n

  n=1229.
  r=1468.

  ;print,'rsun_obs: '+string(index[0].rsun_obs)
  ;print,'n: '+string(n)
  ;print,'r: '+string(r)

;calculate special degrees
  degs=fltarr(8)
  anr=acos(n/r)
  degs[0]=anr
  degs[1]=!pi/2.-anr
  degs[2]=!pi/2.+anr
  degs[3]=!pi-anr
  degs[4]=!pi+anr
  degs[5]=(3./2.)*!pi-anr
  degs[6]=(3./2.)*!pi+anr
  degs[7]=(2.)*!pi-anr
  
;Find their corresponding indices in the angle array
  dginds=intarr(8)
  for i=0,7 do dginds[i]=min(where(degarr ge degs[i]))
  
;Calculate the heights in the 12 different regions
  htarr[0:dginds[0]-1]=n/abs(cos(degarr[0:dginds[0]-1])) ; region I
  htarr[dginds[0]:dginds[1]-1]=r
  htarr[dginds[1]:dginds[2]-1]=n/abs(sin(degarr[dginds[1]:dginds[2]-1])) ; regions II and III
  htarr[dginds[2]:dginds[3]-1]=r
  htarr[dginds[3]:dginds[4]-1]=n/abs(cos(degarr[dginds[3]:dginds[4]-1])) ; regions IV and V
  htarr[dginds[4]:dginds[5]-1]=r
  htarr[dginds[5]:dginds[6]-1]=n/abs(sin(degarr[dginds[5]:dginds[6]-1])) ; regions VI and VII
  htarr[dginds[6]:dginds[7]-1]=r
  htarr[dginds[7]:ndeg-1]=n/abs(cos(degarr[dginds[7]:ndeg-1])) ; region VIII
  
  if keyword_set(angle) then begin
     ang=angle
     while ang lt 0. do ang+=360.
     while ang gt 360. do ang-=360.
     ang=ang*!pi/180.     
     angind=min(where(degarr ge ang))
     tmp=htarr[angind]
     htarr=tmp[0]
  endif

  
  if keyword_set(degarray) then begin
     numinds=n_elements(degarray)
     beg=min(where(degarr ge degarray[0]*!pi/180.))
     fin=min(where(degarr ge degarray[numinds-1]*!pi/180.))
     htarr=congrid(htarr[beg:fin],numinds)
  endif
  
  if keyword_set(rsun) then htarr/=index[0].rsun_obs
  return, htarr
  
end
