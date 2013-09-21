;------------------------------------------------------------------
;  Trace field line from (x0,y0,z0) in heliocentric coordinates:
;------------------------------------------------------------------
pro subr_trace,x2,y2,z2,rbase=rbase
@cms2.blk
@cms2.trc
;
;  Copy variables so that input (x2,y2,z2) don't change:
;
x0=x2
y0=y2
z0=z2
;
;  Check that starting point lies within computational domain:
;
rad=sqrt(x0^2+y0^2+z0^2)
if n_elements(rbase) eq 0 then rbase=1.0
if rad lt rbase or rad gt radmax then begin
  if rad lt rbase then fact=rbase/rad else fact=radmax/rad
  x0=fact*x0
  y0=fact*y0
  z0=fact*z0
endif
eps=1.e-6
;
;  Initialize arrays:
;
kdim=500
xtrc=fltarr(kdim)
ytrc=fltarr(kdim)
ztrc=fltarr(kdim)
strc=fltarr(kdim)
rtrc=fltarr(kdim)
ds2=fltarr(kdim)
bx2=fltarr(kdim)
by2=fltarr(kdim)
bz2=fltarr(kdim)
bm2=fltarr(kdim)
br2=fltarr(kdim)
kadd=500
dum=fltarr(kadd)
;
;  Trace field line starting from point (x0,y0,z0).
;  On exit, kp is the index of the starting point.
;
kp=kdim-1
k=kp
xtrc(k)=x0
ytrc(k)=y0
ztrc(k)=z0
strc(k)=0.0
rtrc(k)=sqrt(xtrc(k)^2+ytrc(k)^2+ztrc(k)^2)
subr_magn,xtrc(k),ytrc(k),ztrc(k),ds2a,bx2a,by2a,bz2a,/cart
ds2(k)=ds2a
bx2(k)=bx2a
by2(k)=by2a
bz2(k)=bz2a
bm2(k)=sqrt(bx2(k)^2+by2(k)^2+bz2(k)^2)
br2(k)=(bx2(k)*xtrc(k)+by2(k)*ytrc(k)+bz2(k)*ztrc(k))/rtrc(k)
;
;  Trace backward:
;
cond1=(rtrc(k) gt rbase+eps  and rtrc(k) lt radmax-eps)
cond2=(rtrc(k) le rbase+eps  and br2(k) lt 0.0)
cond3=(rtrc(k) ge radmax-eps and br2(k) gt 0.0)
if cond1 or cond2 or cond3 then begin
  ds2(k-1)=ds2(k)
  mode=0
  n=0
  repeat begin
    n=n+1
    k=k-1
;
;  If necessary, increase array size (backward):
;
    if k eq 0 then begin
      xtrc=[dum,xtrc]
      ytrc=[dum,ytrc]
      ztrc=[dum,ztrc]
      strc=[dum,strc]
      rtrc=[dum,rtrc]
      ds2=[dum,ds2]
      bx2=[dum,bx2]
      by2=[dum,by2]
      bz2=[dum,bz2]
      bm2=[dum,bm2]
      br2=[dum,br2]
      kdim=kdim+kadd
      kp=kp+kadd
      k=k+kadd
    endif
    k1=k+1
;
;  Compute magnetic field at half-step (backward):
;
    fact=0.5*ds2(k)/bm2(k1)
    x2h=xtrc(k1)-fact*bx2(k1)
    y2h=ytrc(k1)-fact*by2(k1)
    z2h=ztrc(k1)-fact*bz2(k1)
    r2h=sqrt(x2h^2+y2h^2+z2h^2)
    subr_magn,x2h,y2h,z2h,ds2h,bx2h,by2h,bz2h,/cart
    bm2h=sqrt(bx2h^2+by2h^2+bz2h^2)
    br2h=(bx2h*x2h+by2h*y2h+bz2h*z2h)/r2h
;
;  On the first backward step, if the field line is curved downward,
;  make sure that the step size is smaller than about one fifth
;  of the (estimated) loop length, otherwise reduce step size and
;  recompute the magnetic field at the half-step:
;
    if n eq 1 then begin
      e1=br2(k1)/bm2(k1)
      eh=br2h   /bm2h
      deds=(e1-eh)/(0.5*ds2(k))
      if deds lt 0.0 then begin
        length=-2.0/deds*sqrt(e1^2-2.*rtrc(k1)*deds)
        if ds2(k) gt 0.21*length then begin
          ds2(k)=0.21*length
          mode=1
          fact=0.5*ds2(k)/bm2(k1)
          x2h=xtrc(k1)-fact*bx2(k1)
          y2h=ytrc(k1)-fact*by2(k1)
          z2h=ztrc(k1)-fact*bz2(k1)
          r2h=sqrt(x2h^2+y2h^2+z2h^2)
          subr_magn,x2h,y2h,z2h,ds2h,bx2h,by2h,bz2h,/cart
          bm2h=sqrt(bx2h^2+by2h^2+bz2h^2)
          br2h=(bx2h*x2h+by2h*y2h+bz2h*z2h)/r2h
        endif
      endif
    endif
;
;  Compute magnetic field at full-step (backward):
;
    fact=ds2(k)/bm2h
    xtrc(k)=xtrc(k1)-fact*bx2h
    ytrc(k)=ytrc(k1)-fact*by2h
    ztrc(k)=ztrc(k1)-fact*bz2h
    rtrc(k)=sqrt(xtrc(k)^2+ytrc(k)^2+ztrc(k)^2)
    subr_magn,xtrc(k),ytrc(k),ztrc(k),ds2a,bx2a,by2a,bz2a,/cart
    ds2(k-1)=ds2a
    bx2(k)=bx2a
    by2(k)=by2a
    bz2(k)=bz2a
    bm2(k)=sqrt(bx2(k)^2+by2(k)^2+bz2(k)^2)
    br2(k)=(bx2(k)*xtrc(k)+by2(k)*ytrc(k)+bz2(k)*ztrc(k))/rtrc(k)
    strc(k)=strc(k1)-ds2(k)
    if mode then ds2(k-1)=ds2(k)
  endrep until ((rtrc(k) le rbase+eps) or (rtrc(k) ge radmax-eps))
;
;  Correct last point of backward trace (lower or upper boundary):
;
  if rtrc(k) le rbase+eps then begin
    frac=(rbase-rtrc(k1))/(rtrc(k)-rtrc(k1))
    frac1=1.0-frac
    xtrc(k)=xtrc(k1)*frac1+xtrc(k)*frac
    ytrc(k)=ytrc(k1)*frac1+ytrc(k)*frac
    ztrc(k)=ztrc(k1)*frac1+ztrc(k)*frac
    strc(k)=strc(k1)*frac1+strc(k)*frac
    rtrc(k)=sqrt(xtrc(k)^2+ytrc(k)^2+ztrc(k)^2)
    ds2(k)=frac*ds2(k)
    bx2(k)=bx2(k1)*frac1+bx2(k)*frac
    by2(k)=by2(k1)*frac1+by2(k)*frac
    bz2(k)=bz2(k1)*frac1+bz2(k)*frac
    bm2(k)=sqrt(bx2(k)^2+by2(k)^2+bz2(k)^2)
    br2(k)=(bx2(k)*xtrc(k)+by2(k)*ytrc(k)+bz2(k)*ztrc(k))/rtrc(k)
  endif
  if rtrc(k) ge radmax-eps then begin
    frac=(radmax-rtrc(k1))/(rtrc(k)-rtrc(k1))
    frac1=1.0-frac
    xtrc(k)=xtrc(k1)*frac1+xtrc(k)*frac
    ytrc(k)=ytrc(k1)*frac1+ytrc(k)*frac
    ztrc(k)=ztrc(k1)*frac1+ztrc(k)*frac
    strc(k)=strc(k1)*frac1+strc(k)*frac
    rtrc(k)=sqrt(xtrc(k)^2+ytrc(k)^2+ztrc(k)^2)
    ds2(k)=frac*ds2(k)
    bx2(k)=bx2(k1)*frac1+bx2(k)*frac
    by2(k)=by2(k1)*frac1+by2(k)*frac
    bz2(k)=bz2(k1)*frac1+bz2(k)*frac
    bm2(k)=sqrt(bx2(k)^2+by2(k)^2+bz2(k)^2)
    br2(k)=(bx2(k)*xtrc(k)+by2(k)*ytrc(k)+bz2(k)*ztrc(k))/rtrc(k)
  endif
endif
;
;  Reorganize data arrays:
;
k0=k
xtrc=[xtrc(k0:kdim-1),dum]
ytrc=[ytrc(k0:kdim-1),dum]
ztrc=[ztrc(k0:kdim-1),dum]
strc=[strc(k0:kdim-1),dum]
rtrc=[rtrc(k0:kdim-1),dum]
ds2=[ds2(k0:kdim-1),dum]
bx2=[bx2(k0:kdim-1),dum]
by2=[by2(k0:kdim-1),dum]
bz2=[bz2(k0:kdim-1),dum]
bm2=[bm2(k0:kdim-1),dum]
br2=[br2(k0:kdim-1),dum]
kdim=kdim-k0+kadd
kp=kp-k0
k=kp
;
;  Trace forward:
;
cond1=(rtrc(k) gt rbase+eps  and rtrc(k) lt radmax-eps)
cond2=(rtrc(k) le rbase+eps  and br2(k) gt 0.0)
cond3=(rtrc(k) ge radmax-eps and br2(k) lt 0.0)
if cond1 or cond2 or cond3 then begin
  mode=0
  n=0
  repeat begin
    n=n+1
    k=k+1
;
;  If necessary, increase array size (forward):
;
    if k eq kdim-1 then begin
      xtrc=[xtrc,dum]
      ytrc=[ytrc,dum]
      ztrc=[ztrc,dum]
      strc=[strc,dum]
      rtrc=[rtrc,dum]
      ds2=[ds2,dum]
      bx2=[bx2,dum]
      by2=[by2,dum]
      bz2=[bz2,dum]
      bm2=[bm2,dum]
      br2=[br2,dum]
      kdim=kdim+kadd
    endif
    k1=k-1
;
;  Compute magnetic field at half-step (forward):
;
    fact=0.5*ds2(k1)/bm2(k1)
    x2h=xtrc(k1)+fact*bx2(k1)
    y2h=ytrc(k1)+fact*by2(k1)
    z2h=ztrc(k1)+fact*bz2(k1)
    r2h=sqrt(x2h^2+y2h^2+z2h^2)
    subr_magn,x2h,y2h,z2h,ds2h,bx2h,by2h,bz2h,/cart
    bm2h=sqrt(bx2h^2+by2h^2+bz2h^2)
    br2h=(bx2h*x2h+by2h*y2h+bz2h*z2h)/r2h
;
;  On the first forward step, if the field line is curved downward,
;  make sure that the step size is smaller than about one fifth
;  of the (estimated) loop length, otherwise reduce step size and
;  recompute the magnetic field at the half-step:
;
    if n eq 1 then begin
      e1=br2(k1)/bm2(k1)
      eh=br2h   /bm2h
      deds=(eh-e1)/(0.5*ds2(k1))
      if deds lt 0.0 then begin
        length=-2.0/deds*sqrt(e1^2-2.*rtrc(k1)*deds)
        if ds2(k1) gt 0.21*length then begin
          ds2(k1)=0.21*length
          mode=1
          fact=0.5*ds2(k1)/bm2(k1)
          x2h=xtrc(k1)+fact*bx2(k1)
          y2h=ytrc(k1)+fact*by2(k1)
          z2h=ztrc(k1)+fact*bz2(k1)
          r2h=sqrt(x2h^2+y2h^2+z2h^2)
          subr_magn,x2h,y2h,z2h,ds2h,bx2h,by2h,bz2h,/cart
          bm2h=sqrt(bx2h^2+by2h^2+bz2h^2)
          br2h=(bx2h*x2h+by2h*y2h+bz2h*z2h)/r2h
        endif
      endif
    endif
;
;  Compute magnetic field at full-step (backward):
;
    fact=ds2(k1)/bm2h
    xtrc(k)=xtrc(k1)+fact*bx2h
    ytrc(k)=ytrc(k1)+fact*by2h
    ztrc(k)=ztrc(k1)+fact*bz2h
    rtrc(k)=sqrt(xtrc(k)^2+ytrc(k)^2+ztrc(k)^2)
    subr_magn,xtrc(k),ytrc(k),ztrc(k),ds2a,bx2a,by2a,bz2a,/cart
    ds2(k)=ds2a
    bx2(k)=bx2a
    by2(k)=by2a
    bz2(k)=bz2a
    bm2(k)=sqrt(bx2(k)^2+by2(k)^2+bz2(k)^2)
    br2(k)=(bx2(k)*xtrc(k)+by2(k)*ytrc(k)+bz2(k)*ztrc(k))/rtrc(k)
    strc(k)=strc(k1)+ds2(k1)
    if mode then ds2(k)=ds2(k1)
  endrep until ((rtrc(k) le rbase+eps) or (rtrc(k) ge radmax-eps))
;
;  Correct last point of backward trace:
;
  if rtrc(k) le rbase+eps then begin
    frac=(rbase-rtrc(k1))/(rtrc(k)-rtrc(k1))
    frac1=1.0-frac
    xtrc(k)=xtrc(k1)*frac1+xtrc(k)*frac
    ytrc(k)=ytrc(k1)*frac1+ytrc(k)*frac
    ztrc(k)=ztrc(k1)*frac1+ztrc(k)*frac
    strc(k)=strc(k1)*frac1+strc(k)*frac
    rtrc(k)=sqrt(xtrc(k)^2+ytrc(k)^2+ztrc(k)^2)
    ds2(k)=frac*ds2(k)
    bx2(k)=bx2(k1)*frac1+bx2(k)*frac
    by2(k)=by2(k1)*frac1+by2(k)*frac
    bz2(k)=bz2(k1)*frac1+bz2(k)*frac
    bm2(k)=sqrt(bx2(k)^2+by2(k)^2+bz2(k)^2)
    br2(k)=(bx2(k)*xtrc(k)+by2(k)*ytrc(k)+bz2(k)*ztrc(k))/rtrc(k)
  endif
  if rtrc(k) ge radmax-eps then begin
    frac=(radmax-rtrc(k1))/(rtrc(k)-rtrc(k1))
    frac1=1.0-frac
    xtrc(k)=xtrc(k1)*frac1+xtrc(k)*frac
    ytrc(k)=ytrc(k1)*frac1+ytrc(k)*frac
    ztrc(k)=ztrc(k1)*frac1+ztrc(k)*frac
    strc(k)=strc(k1)*frac1+strc(k)*frac
    rtrc(k)=sqrt(xtrc(k)^2+ytrc(k)^2+ztrc(k)^2)
    ds2(k)=frac*ds2(k)
    bx2(k)=bx2(k1)*frac1+bx2(k)*frac
    by2(k)=by2(k1)*frac1+by2(k)*frac
    bz2(k)=bz2(k1)*frac1+bz2(k)*frac
    bm2(k)=sqrt(bx2(k)^2+by2(k)^2+bz2(k)^2)
    br2(k)=(bx2(k)*xtrc(k)+by2(k)*ytrc(k)+bz2(k)*ztrc(k))/rtrc(k)
  endif
endif
;
;  Reorganize data arrays:
;
xtrc=xtrc(0:k)
ytrc=ytrc(0:k)
ztrc=ztrc(0:k)
strc=strc(0:k)
rtrc=rtrc(0:k)
ds2=ds2(0:k)
bx2=bx2(0:k)
by2=by2(0:k)
bz2=bz2(0:k)
bm2=bm2(0:k)
br2=br2(0:k)
;
ntrc=k+1
end

;----------------------------------------------------------------
;  Interpolate in 3D magnetic model.
;----------------------------------------------------------------
pro subr_magn,x2,y2,z2,ds2,fx2,fy2,fz2,hx2,hy2,hz2,  $
              cart=cart,par=par,verbose=verbose,test=test
@cms2.blk
;
;  The CART keyword indicates the coordinate system used for input
;  position (xx,yy,zz):
;  - If the CART keyword is set, the input is assumed to be in
;    "cartesian model coordinates" with the y-axis along the solar
;    rotation axis and the z-axis along intersection of equator and
;    central meridian. The position can be anywhere on the Sun (i.e.,
;    GLOBAL or HIRES regions). The output vector is rotated to the
;    same cartesian reference frame.
;  - If the CART keyword is NOT set, the input is assumed to be in
;    cell coordinates, and must be in range xmin < xx < xmax, etc.
;    If coordinates are not in range, values are modified and a
;    warning is issued. The output vector is (Qlon,Qlat,Qrad),
;    not rotated to cartesian frame.
;
if n_elements(cart) eq 0 then cart=0
cart=fix(cart)>0<1
if cart then str='Cartesian' else str='box'
;
;  Parameter to be determined:
;  0=MAG, 1=CUR, 2=VEL, 3=FOR, 4=GRAD(alpha), 5=alpha
;
if n_elements(par) eq 0 then par=0
par=fix(par)>0<5
var=['MAG','CUR','VEL','FOR','GRAD(alpha)','alpha']
;
;  Copy into (xx,yy,zz) so that (x2,y2,z2) don't change:
;
xx=x2
yy=y2
zz=z2
if keyword_set(verbose) then begin
  print,string(str,var(par),  $
    format='("SUBR_MAGN: Input in ",a," coordinates, var=",a)')
  if cart then print,string(xx,yy,zz,  $
    format='("  Cartesian: xs=",f9.6,", ys=",f9.6,", zs=",f9.6," Rsun")')
endif
;
;  If CART keyword is set, make sure point lies in spherical shell,
;  then compute spherical coordinates:
;
if cart then begin
  rad=sqrt(xx^2+yy^2+zz^2)
  if rad lt 0.8 or rad gt 1.2*radmax then begin
    xx=0.
    yy=0.
    zz=1.
    rad=1.
    if keyword_set(verbose) then begin
      print,string(xx,yy,zz,  $
      format='("  Modified:  xs=",f9.6,", ys=",f9.6,", zs=",f9.6," Rsun")')
    endif
    print,'  Warning: xs, ys or zs modified in SUBR_MAGN'
  endif
  if rad lt 1.0 or rad gt radmax then begin
    if rad lt 1.0 then fact=1.0/rad else fact=radmax/rad
    xx=fact*xx
    yy=fact*yy
    zz=fact*zz
    rad=sqrt(xx^2+yy^2+zz^2)
  endif
  rho=sqrt(xx^2+zz^2)
  if rho lt 1.e-6 then begin                  ; avoid rotation axis
    xx=0.
    zz=1.e-6
    rho=1.e-6
  endif
  cth=yy/rad
  sth=rho/rad
  if sth lt 0.3 then begin
    theta=asin(sth)
    if cth lt 0. then theta=!pi-theta
  endif else theta=acos(cth)
  phi=atan(xx,zz)
  cph=cos(phi)
  sph=sin(phi)
  rad=rad>1.0<radmax
  lat=90.0-!radeg*theta        ; longitude/latitude in degrees
  lon=!radeg*phi
  if keyword_set(verbose) then begin
    print,string(lon,lat,rad,  $
    format='("  Spherical: lon=",f8.3," deg, lat=",f8.3," deg, rad=",f7.4)')
  endif
;
;  When outside high-resolution region, interpolate in global model
;  and return:
;
  if lon lt lonmin or lon gt lonmax or  $
     lat lt latmin or lat gt latmax then begin
    if par eq 0 then begin                              ; magnetic field
      if lon lt glob_lons(        0) then lon=lon+360.
      if lon gt glob_lons(glob_nlon) then lon=lon-360.
      i=fix((lon-glob_lons(0))/glob_dlon)<(glob_nlon-1) ; uniform in longitude
      ip=i+1
      fx0=(lon-glob_lons(i))/glob_dlon
      fx1=1.0-fx0
      j=fix((lat+90.)/glob_dlat)<(glob_nlat-1)          ; uniform in latitude
      jp=j+1
      fy0=(lat-glob_lats(j))/glob_dlat
      fy1=1.0-fy0
      arg=alog(rad)/glob_del
      k=fix(arg)<(glob_nrad-1)                          ; exponential in radius
      kp=k+1
      fz0=arg-float(k)
      fz1=1.0-fz0
      blon=fz1*(fy1*(fx1*glob_blon(i,j ,k )+fx0*glob_blon(ip,j ,k ))    $
               +fy0*(fx1*glob_blon(i,jp,k )+fx0*glob_blon(ip,jp,k )))   $
          +fz0*(fy1*(fx1*glob_blon(i,j ,kp)+fx0*glob_blon(ip,j ,kp))    $
               +fy0*(fx1*glob_blon(i,jp,kp)+fx0*glob_blon(ip,jp,kp)))
      blat=fz1*(fy1*(fx1*glob_blat(i,j ,k )+fx0*glob_blat(ip,j ,k ))    $
               +fy0*(fx1*glob_blat(i,jp,k )+fx0*glob_blat(ip,jp,k )))   $
          +fz0*(fy1*(fx1*glob_blat(i,j ,kp)+fx0*glob_blat(ip,j ,kp))    $
               +fy0*(fx1*glob_blat(i,jp,kp)+fx0*glob_blat(ip,jp,kp)))
      brad=fz1*(fy1*(fx1*glob_brad(i,j ,k )+fx0*glob_brad(ip,j ,k ))    $
               +fy0*(fx1*glob_brad(i,jp,k )+fx0*glob_brad(ip,jp,k )))   $
          +fz0*(fy1*(fx1*glob_brad(i,j ,kp)+fx0*glob_brad(ip,j ,kp))    $
               +fy0*(fx1*glob_brad(i,jp,kp)+fx0*glob_brad(ip,jp,kp)))
      fy2=brad*cth+blat*sth
      dum=brad*sth-blat*cth            ; rotate to model cartesian cordinates
      fx2=dum*sph+blon*cph
      fz2=dum*cph-blon*sph
    endif else begin                   ; no current, velocity, force or alpha
      fx2=0.
      fy2=0.
      fx2=0.
    endelse
    ds2=0.3*glob_del
    if keyword_set(verbose) then begin
      print,'  In GLOBAL region:'
      if par eq 0 then begin
        print,string(i,j,k,   $
              format='("  Cell: i=",i4,", j=",i4,", k=",i4)')
        print,string(fx0,fy0,fz0,  $
              format='("  Interpol: fx0=",f7.3,", fy0=",f7.3,", fz0=",f7.3)')
        print,string(blon,blat,brad,  $
              format='("  Vector: blon=",f9.3,", blat=",f9.3,", brad=",f9.3)')
      endif
      print,string(fx2,fy2,fz2,  $
              format='("  Output: fx2=",e10.3,", fy2=",e10.3,", fz2=",e10.3)')
      print,'-----'
    endif
    return
;
;  When inside HIRES region, convert (xx,yy,zz) to cell coordinates
;  and continue:
;
  endif else begin
    xx=phi/delt0
    yy=-alog(tan(0.5*theta))/delt0
    zz=alog(rad)/delt0
  endelse
endif
;
;  The coordinates (xx,yy,zz) are now in cell coordinates, and must be
;  in the range xmin < xx < xmax, etc.:
;
if keyword_set(verbose) then begin
  print,'  In HIRES region:'
  print,string(xx,yy,zz,  $
    format='("  Box coord: xx=",f9.4,", yy=",f9.4,", zz=",f9.4," cells")')
endif
if xx lt xmin or xx gt xmax or yy lt ymin or yy gt ymax then begin
  print,'  Warning: xx or yy modified in SUBR_MAGN'
  print,string(xx,yy,format='("  Original:  xx=",f10.5,", yy=",f10.5," cells")')
  xx=xx>xmin<xmax
  yy=yy>ymin<ymax
  print,string(xx,yy,format='("  Modified:  xx=",f10.5,", yy=",f10.5," cells")')
endif
zz=zz>zmin<zmax
;
;  Find the associated cell in HIRES region:
;
if zz lt zarr(nzsum-2) then begin         ; determine kz index
  kz=0
  while zz gt zarr(kz+1) do kz=kz+1
endif else kz=nzsum-2
fz0=(zz-zarr(kz))/(zarr(kz+1)-zarr(kz))
fz1=1.0-fz0
;
n=0
while kz gt kz2(n) do n=n+1               ; determine box index
fact=2^n
nx=nx1base/fact+1
ny=ny1base/fact+1
;
dxx=(xx-xmin)/fact                        ; determine longitude index
i=fix(dxx)<(nx-2)
fx0=dxx-float(i)
fx1=1.0-fx0
;
dyy=(yy-ymin)/fact                        ; determine latitude index
j=fix(dyy)<(ny-2)
fy0=dyy-float(j)
fy1=1.0-fy0
;
if cart eq 0 then begin
  rad=exp(delt0*zz)
  theta=2.*atan(exp(-delt0*yy))
  sth=sin(theta)
  lat=90.0-theta*!radeg
  lon=delt0*xx*!radeg
endif
;
hx2=delt0*rad*sth      ; these are lengths (Rsun) for DX = 1.0, etc.,
hy2=delt0*rad*sth      ; not grid spacings DX = FACT >= 1.
hz2=delt0*rad
ds2=0.3*hx2*fact         ; step size (Rsun)
;
;  Compute indices in linear arrays for high-resolution model:
;
k000=ka1(kz  )+nx*(j  )+i
k100=ka1(kz  )+nx*(j  )+i+1
k010=ka1(kz  )+nx*(j+1)+i
k110=ka1(kz  )+nx*(j+1)+i+1
k001=ka1(kz+1)+nx*(j  )+i
k101=ka1(kz+1)+nx*(j  )+i+1
k011=ka1(kz+1)+nx*(j+1)+i
k111=ka1(kz+1)+nx*(j+1)+i+1
;
;  Interpolate in HIRES model:
;
case par of
0: begin
   qlon=fz1*(fy1*(fx1*bx1(k000)+fx0*bx1(k100))    $
            +fy0*(fx1*bx1(k010)+fx0*bx1(k110)))   $
       +fz0*(fy1*(fx1*bx1(k001)+fx0*bx1(k101))    $
            +fy0*(fx1*bx1(k011)+fx0*bx1(k111)))
   qlat=fz1*(fy1*(fx1*by1(k000)+fx0*by1(k100))    $
            +fy0*(fx1*by1(k010)+fx0*by1(k110)))   $
       +fz0*(fy1*(fx1*by1(k001)+fx0*by1(k101))    $
            +fy0*(fx1*by1(k011)+fx0*by1(k111)))
   qrad=fz1*(fy1*(fx1*bz1(k000)+fx0*bz1(k100))    $
            +fy0*(fx1*bz1(k010)+fx0*bz1(k110)))   $
       +fz0*(fy1*(fx1*bz1(k001)+fx0*bz1(k101))    $
            +fy0*(fx1*bz1(k011)+fx0*bz1(k111)))
   end
1: begin
   qlon=fz1*(fy1*(fx1*cx1(k000)+fx0*cx1(k100))    $
            +fy0*(fx1*cx1(k010)+fx0*cx1(k110)))   $
       +fz0*(fy1*(fx1*cx1(k001)+fx0*cx1(k101))    $
            +fy0*(fx1*cx1(k011)+fx0*cx1(k111)))
   qlat=fz1*(fy1*(fx1*cy1(k000)+fx0*cy1(k100))    $
            +fy0*(fx1*cy1(k010)+fx0*cy1(k110)))   $
       +fz0*(fy1*(fx1*cy1(k001)+fx0*cy1(k101))    $
            +fy0*(fx1*cy1(k011)+fx0*cy1(k111)))
   qrad=fz1*(fy1*(fx1*cz1(k000)+fx0*cz1(k100))    $
            +fy0*(fx1*cz1(k010)+fx0*cz1(k110)))   $
       +fz0*(fy1*(fx1*cz1(k001)+fx0*cz1(k101))    $
            +fy0*(fx1*cz1(k011)+fx0*cz1(k111)))
   end
2: begin
   qlon=fz1*(fy1*(fx1*vx1(k000)+fx0*vx1(k100))    $
            +fy0*(fx1*vx1(k010)+fx0*vx1(k110)))   $
       +fz0*(fy1*(fx1*vx1(k001)+fx0*vx1(k101))    $
            +fy0*(fx1*vx1(k011)+fx0*vx1(k111)))
   qlat=fz1*(fy1*(fx1*vy1(k000)+fx0*vy1(k100))    $
            +fy0*(fx1*vy1(k010)+fx0*vy1(k110)))   $
       +fz0*(fy1*(fx1*vy1(k001)+fx0*vy1(k101))    $
            +fy0*(fx1*vy1(k011)+fx0*vy1(k111)))
   qrad=fz1*(fy1*(fx1*vz1(k000)+fx0*vz1(k100))    $
            +fy0*(fx1*vz1(k010)+fx0*vz1(k110)))   $
       +fz0*(fy1*(fx1*vz1(k001)+fx0*vz1(k101))    $
            +fy0*(fx1*vz1(k011)+fx0*vz1(k111)))
   end
3: begin
   qlon=fz1*(fy1*(fx1*ffx1(k000)+fx0*ffx1(k100))    $
            +fy0*(fx1*ffx1(k010)+fx0*ffx1(k110)))   $
       +fz0*(fy1*(fx1*ffx1(k001)+fx0*ffx1(k101))    $
            +fy0*(fx1*ffx1(k011)+fx0*ffx1(k111)))
   qlat=fz1*(fy1*(fx1*ffy1(k000)+fx0*ffy1(k100))    $
            +fy0*(fx1*ffy1(k010)+fx0*ffy1(k110)))   $
       +fz0*(fy1*(fx1*ffy1(k001)+fx0*ffy1(k101))    $
            +fy0*(fx1*ffy1(k011)+fx0*ffy1(k111)))
   qrad=fz1*(fy1*(fx1*ffz1(k000)+fx0*ffz1(k100))    $
            +fy0*(fx1*ffz1(k010)+fx0*ffz1(k110)))   $
       +fz0*(fy1*(fx1*ffz1(k001)+fx0*ffz1(k101))    $
            +fy0*(fx1*ffz1(k011)+fx0*ffz1(k111)))
   end
4: begin
   qlon=(fz1*(fy1*(alpha1(k100)-alpha1(k000))    $
             +fy0*(alpha1(k110)-alpha1(k010)))   $
        +fz0*(fy1*(alpha1(k101)-alpha1(k001))    $
             +fy0*(alpha1(k111)-alpha1(k011))))/(fact*hx2)
   qlat=(fx1*(fz1*(alpha1(k010)-alpha1(k000))    $
             +fz0*(alpha1(k011)-alpha1(k001)))   $
        +fx0*(fz1*(alpha1(k110)-alpha1(k100))    $
             +fz0*(alpha1(k111)-alpha1(k101))))/(fact*hy2)
   qrad=(fy1*(fx1*(alpha1(k001)-alpha1(k000))    $
             +fx0*(alpha1(k101)-alpha1(k100)))   $
        +fy0*(fx1*(alpha1(k011)-alpha1(k010))    $
             +fx0*(alpha1(k111)-alpha1(k110))))/(fact*hz2)
   if keyword_set(test) then begin
     alp=fz1*(fy1*(fx1*alpha1(k000)+fx0*alpha1(k100))    $
             +fy0*(fx1*alpha1(k010)+fx0*alpha1(k110)))   $
        +fz0*(fy1*(fx1*alpha1(k001)+fx0*alpha1(k101))    $
             +fy0*(fx1*alpha1(k011)+fx0*alpha1(k111)))
     bx3=fz1*(fy1*(fx1*bx1(k000)+fx0*bx1(k100))    $
             +fy0*(fx1*bx1(k010)+fx0*bx1(k110)))   $
        +fz0*(fy1*(fx1*bx1(k001)+fx0*bx1(k101))    $
             +fy0*(fx1*bx1(k011)+fx0*bx1(k111)))
     by3=fz1*(fy1*(fx1*by1(k000)+fx0*by1(k100))    $
             +fy0*(fx1*by1(k010)+fx0*by1(k110)))   $
        +fz0*(fy1*(fx1*by1(k001)+fx0*by1(k101))    $
             +fy0*(fx1*by1(k011)+fx0*by1(k111)))
     bz3=fz1*(fy1*(fx1*bz1(k000)+fx0*bz1(k100))    $
             +fy0*(fx1*bz1(k010)+fx0*bz1(k110)))   $
        +fz0*(fy1*(fx1*bz1(k001)+fx0*bz1(k101))    $
             +fy0*(fx1*bz1(k011)+fx0*bz1(k111)))
     dum=bx3*fx2+by3*fy2+bz3*fz2
     cosa=dum/sqrt(bx3^2+by3^2+bz3^2)/sqrt(fx2^2+fy2^2+fz2^2)
     cx3=fz1*(fy1*(fx1*cx1(k000)+fx0*cx1(k100))    $
             +fy0*(fx1*cx1(k010)+fx0*cx1(k110)))   $
        +fz0*(fy1*(fx1*cx1(k001)+fx0*cx1(k101))    $
             +fy0*(fx1*cx1(k011)+fx0*cx1(k111)))
     cy3=fz1*(fy1*(fx1*cy1(k000)+fx0*cy1(k100))    $
             +fy0*(fx1*cy1(k010)+fx0*cy1(k110)))   $
        +fz0*(fy1*(fx1*cy1(k001)+fx0*cy1(k101))    $
             +fy0*(fx1*cy1(k011)+fx0*cy1(k111)))
     cz3=fz1*(fy1*(fx1*cz1(k000)+fx0*cz1(k100))    $
             +fy0*(fx1*cz1(k010)+fx0*cz1(k110)))   $
        +fz0*(fy1*(fx1*cz1(k001)+fx0*cz1(k101))    $
             +fy0*(fx1*cz1(k011)+fx0*cz1(k111)))
     fx3=cy3*bz3-cz3*by3
     fy3=cz3*bx3-cx3*bz3
     fz3=cx3*by3-cy3*bx3
     eps=sqrt(fx3^2+fy3^2+fz3^2)/sqrt(bx3^2+by3^2+bz3^2)/sqrt(cx3^2+cy3^2+cz3^2)
     print,'-----'
     print,'alpha=',alp,', B.grad(alpha)=',dum,', cos(angle)=',cosa,', eps=',eps
   endif
   end
5: begin
   alp=fz1*(fy1*(fx1*alpha1(k000)+fx0*alpha1(k100))    $
           +fy0*(fx1*alpha1(k010)+fx0*alpha1(k110)))   $
      +fz0*(fy1*(fx1*alpha1(k001)+fx0*alpha1(k101))    $
           +fy0*(fx1*alpha1(k011)+fx0*alpha1(k111)))
   end
endcase
;
;  If CART keyword is set, rotate vector to "model cartesian"
;  reference frame (for vectors only):
;
if par le 4 then begin
  if cart then begin
    fy2=qrad*cth+qlat*sth
    dum=qrad*sth-qlat*cth
    fx2=dum*sph+qlon*cph
    fz2=dum*cph-qlon*sph
  endif else begin
    fx2=qlon
    fy2=qlat
    fz2=qrad
  endelse
endif else begin
  fx2=alp
  fy2=0.
  fz2=0.
endelse
;
;  Diagnostic output:
;
if keyword_set(verbose) then begin
  if cart eq 0 then print,string(lon,lat,rad,  $
    format='("  Spherical: lon=",f8.3," deg, lat=",f8.3," deg, rad=",f7.4)')
  print,string(kz,n,nx,ny,i,j,  $
    format='("  Cell: kz=",i3,", n=",i1,", nx=",i4,", ny=",i4,'+  $
           '", i=",i4,", j=",i4)')
  print,string(fx0,fy0,fz0,  $
        format='("  Interpol: fx0=",f7.3,", fy0=",f7.3,", fz0=",f7.3)')
  if cart and (par le 4) then  $
  print,string(qlon,qlat,qrad,  $
        format='("  Vector: qlon=",e10.3,", qlat=",e10.3,", qrad=",e10.3)')
  print,string(fx2,fy2,fz2,  $
        format='("  Output: fx2=",e10.3,",  fy2=",e10.3,",  fz2=",e10.3)')
  print,string(hx2,hy2,hz2,  $
        format='("  Scale:  hx2=",f7.5,", hy2=",f7.5,", hz2=",f7.5)')
  print,'-----'
endif
end
