function sph2cart,sph
;returns the cartesian position vector, given a spherical one.

pt=dblarr(3)
pt[0]=sph[0]*sin(sph[1])*cos(sph[2])
pt[1]=sph[0]*sin(sph[1])*sin(sph[2])
pt[2]=sph[0]*cos(sph[1])

return,pt
end
