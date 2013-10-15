function cart2sph,pt
;returns the spherical position vector, given a cartesian one.

sph=dblarr(3)
sph[0]=sqrt(pt[0]^2+pt[1]^2+pt[2]^2)
sph[1]=acos(pt[2]/sph[0])
sph[2]=atan(pt[1]/pt[0])
while sph[2] lt 0.0 do sph[2] +=2*!PI
return,sph
end
