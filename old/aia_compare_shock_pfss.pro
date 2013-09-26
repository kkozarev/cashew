
pro aia_compare_shock_pfss,test=test ;subindex,subdata
;This procedure loads AIA data, calculates the corresponding PFSS model,
;then compares/overplots the PFSS to a 3D spherical geometric model.


;------------------------------
;Coordinate conversions notes |
;------------------------------
;We have to combine the graphical coordinate system of IDL with the coordinate
;system of PFSS.

;1. The IDL 3D graphics coordinate system is right-handed so that when
;looking from a positive axis to the origin, a positive rotation is
;counterclockwise. As usual, the x-axis runs across the display, the
;y-axis is vertical, and the positive z-axis extends out from the
;display to the viewer. For example, a 90-degree positive rotation 
;about the z-axis transforms the x-axis to the y-axis.
;             ^ y
;             |
;             |
;             |
;             .------------> x
;            /
;           / 
;          / 
;         z

;2. The PFSS coordinate system is a heliographic r,theta,phi
; coordinate system.  Once the lat/lon (B,L) of the subsolar point are known, it
;      is (reasonably) straightforward to convert from heliographic
;      coordinates (r,theta,phi) to normalized cartesian
;      coordinates (x,y,z):
;         x = r*sin(theta)*sin(phi-L)
;         y =   r*sin(theta)*cos(phi-L)*cos(B) + r*cos(theta)*sin(B)
;         z = - r*sin(theta)*cos(phi-L)*sin(B) + r*cos(theta)*cos(B)
;      Here,the (x,z)-plane defines the plane perpendicular to the
;      viewer (i.e. the image plane) with the x-axis positive
;      westward and the z-axis positive northward.  The y-axis is
;      along the line-of-sight and is positive inward (toward the
;      observer).  Note that (x,y,z) forms a *left*-handed
;      coordinate system.
;             ^ z
;             |
;             |
;             |
;             .------------> x
;            /
;           / 
;          / 
;         y
;The inverse transform is:
;         r     = sqrt(x^2+y^2+z^2)
;                         sqrt(x^2+(y*cos(B)+z*sin(B))^2)
;         theta = arccos( ------------------------------- )
;                               -y*sin(B)+z*cos(B)
;                           sqrt(x^2+(y*cos(B)+z*sin(B))^2)
;         phi   = L+arcsin( -------------------------------- )
;                                 -y*sin(B)+z*cos(B)

;3. To convert between the left-handed PFSS x,y,z system and the
;right-handed IDL x,y,z system, we have to make the following
;adjustments: Switch the y and the z axes.
;--------------------------------------------------------------




;--------------------------------------------------------------
;Load AIA data

;test=1
  if keyword_set(test) then begin
;This is a datacube used for testing.
     print,'Loading SDO/AIA data...'
     pcname=hostname()
     
;for sloncho-2
     if pcname eq 'sloncho-2' then begin
        datapath='/Users/kkozarev/AIA/'
        restore,datapath+'algoTests/yaftawave/normalized_AIA_20110125_05_193_subdata.sav'
        restore,datapath+'pfss_results_20110125_1.1Rs.sav'
     endif else begin
;for arien
        if pcname eq 'arien' then begin
           datapath='/Volumes/PLUME/AIA_data/studies/2011events/e05/'
           restore,datapath+'normalized_AIA_20110125_05_193_subdata.sav'
           restore,datapath+'pfss_results_20110125_1.1Rs.sav'
        endif else begin
           datapath='/Users/kkozarev/AIA/'
           restore,datapath+'algoTests/yaftawave/normalized_AIA_20110125_05_193_subdata.sav'
           restore,datapath+'pfss_results_20110125_1.1Rs.sav'
        endelse
     endelse
;The number of time steps in the AIA data
     ntimes=n_elements(subdata[0,0,*])
  endif
;--------------------------------------------------------------  



;--------------------------------------------------------------
;Calculate and return the PFSS model
  if not keyword_set(test) then begin
     datapath='/Volumes/PLUME/AIA_data/studies/2011events/e37/'
     print,'restoring '+datapath+'normalized_AIA_20110511_37_193_subdata.sav'
     restore,datapath+'normalized_AIA_20110511_37_193_subdata.sav'
     subdata=0.0
     pfss_return_field,subindex[0].date_obs,rstart=1.1,invdens=1
     @pfss_data_block
     ind=where(ptph lt 0.0)
     if ind[0] gt -1 then ptph[ind]+=2*!PI
     ind=where(ptph ge 2*!PI)
     if ind[0] gt -1 then ptph[ind]-=2*!PI
     ind=where(ptth lt 0.0)
     if ind[0] gt -1 then ptth[ind]+=2*!PI
     ind=where(ptth ge 2*!PI)
     if ind[0] gt -1 then ptth[ind]-=2*!PI
     l=subindex[0].crln_obs*!PI/180.0
     b=subindex[0].crlt_obs*!PI/180.0
     print,'saving '+datapath+'pfss_results_20110511_1.1Rs_dens_1.sav'
     save,filename=datapath+'pfss_results_20110511_1.1Rs_dens_1.sav',l,b,ptr,ptth,ptph,nstep
     stop
     return
  endif
;--------------------------------------------------------------



;==============================================================================
;LOOP OVER ALL AIA TIME STEPS HERE
  ntimes=1
  for t=0,ntimes-1 do begin

;Get the Carrington longitude and latitude of SDO
  l=subindex[t].crln_obs*!PI/180.0
  b=subindex[t].crlt_obs*!PI/180.0

;To convert the PFSS coordinates for IDL plotting, switch the x and
;y-axes. See coordinate notes above.
;Also, rescale the PFSS to the solar radius in pixels
  pfss_sphtocart,ptr,ptth,ptph,l,b,px,pz,py
  px*=subindex[t].r_sun
  py*=subindex[t].r_sun
  pz*=subindex[t].r_sun

  nlines=n_elements(px[0,*])
  maxnpts=n_elements(px[*,0])
  
;prepare the translation matrix for the pfss points to the subdata
;field of view:
  T3d, /Reset
  T3d, Translate=[subindex[t].x0_mp, $
                  subindex[t].y0_mp, $
                0.0]
  ;px+=subindex[t].x0_mp
  ;py+=subindex[t].y0_mp



;####################### 1. PLOTTING OF SHOCK AND PFSS ###########################

;-----------------------------------------
;1.1. Plot the AIA data and spherical shock
     wdef,0,1024
     aia_lct,rr,gg,bb,wavelnth=subindex[t].wavelnth,/load
     tvscl,subdata[*,*,t]


;-----------------------------------------
;1.2. Overplot the pfss points, in the device coordinate system.
;create rotation and translation matrix
     pfss_cartpos=fltarr(nlines,3,maxnpts)
     for ff=0,nlines-1 do begin
        npt=nstep[ff]           ;the number of points in this particular line.
        pos = transpose([[reform(px[0:npt-1,ff])],$
                                  [reform(py[0:npt-1,ff])],$
                                  [reform(pz[0:npt-1,ff])]])
        pos = Vert_T3d(pos)
        plots,pos,color=250,/device

;The cartesian position of the PFSS
;points in the IDL coordinate system
        pfss_cartpos[ff,*,0:npt-1]=pos
     endfor
;for ff=0,nlines-1 do plots,reform(pfss_cartpos[ff,*,*]),color=150,/device
   
     
;-----------------------------------------
;1.3. Calculate and plot the spherical surface:

;The radius SHOULD be determined automatically by a combination of 
;yaftawave and least squares circle fit.
;Alternatively, I will have to substitute yafta by clicking on points...
     radius=500
     
;This will have been previously determined and added to the index.
     arcoords=[-20,-70]
     
     aia_plot_hemisphere,subindex,radius,arcoords=arcoords,vertex_list=vertex_list
     nverts=n_elements(vertex_list[0,*])
     
;#################################################################################



     
     
;####################### 2. SHOCK TO FIELD ORIENTATION ##########################
     
;-----------------------------------------
;2.1. Convert the spherical surface points to spherical coordinates for
;easier searching.
;Before converting the spherical surface to heliographic coordinates,
;switch its x- ad y-axis, and convert length to solar radii. 
;NOT SURE IF THIS IS CORRECT!!! NEEDS TO BE CHECKED VISUALLY!
;     sphere_x=(reform(vertex_list[0,*])-subindex[0].x0_mp)
;     sphere_y=(reform(vertex_list[2,*])-subindex[0].y0_mp)
;     sphere_z=reform(vertex_list[1,*])
;     pfss_carttosph,sphere_x,sphere_y,sphere_z,l,b,sphere_r,sphere_th,sphere_ph
;     sphvertex_list=fltarr(3,nverts)
;     sphvertex_list[0,*]=sphere_r
;     sphvertex_list[1,*]=sphere_th
;     sphvertex_list[2,*]=sphere_ph

;Simply use the regular conversions between cartesian and spherical
;coordinates. Stay on the IDL coordinate system.
     sphvertex_list=fltarr(3,nverts)
     
     ;Convert X/Y/Z distances to Rs
     pos=vertex_list
     
     r=sqrt(pos[0,*]^2 + pos[1,*]^2 + pos[2,*]^2)
     sphvertex_list[0,*]=r/subindex[t].r_sun+1

     theta=reform(acos(pos[2,*]/r))
;Check THETA for weird angles
     ind=where(theta lt 0.0)
     if ind[0] ne -1 then theta[ind]+=2*!PI
     ind=where(theta ge 2*!PI)
     if ind[0] ne -1 then theta[ind]-=2*!PI
     sphvertex_list[1,*]=theta
     
     phi=reform(atan(pos[1,*]/pos[0,*]))
;Check PHI for weird angles
     ind=where(phi lt 0.0)
     if ind[0] ne -1 then phi[ind]+=2*!PI
     ind=where(phi ge 2*!PI)
     if ind[0] ne -1 then phi[ind]-=2*!PI
     sphvertex_list[2,*]=phi

;-----------------------------------------
;2.2. Convert the PFSS points to spherical coordinates for
;easier searching.
  pfss_sphpos=fltarr(nlines,3,maxnpts)
  for ff=0,nlines-1 do begin
     npt=nstep[ff]
     pos=reform(pfss_cartpos[ff,*,0:npt-1])

 
     r=sqrt(pos[0,*]^2 + pos[1,*]^2 + pos[2,*]^2)
     pfss_sphpos[ff,0,0:npt-1]=r/subindex[t].r_sun-1.0
     
     theta=reform(acos(pos[2,*]/r))
     ;Check THETA for weird angles
     ind=where(theta lt 0.0)
     if ind[0] ne -1 then theta[ind]+=2*!PI
     ind=where(theta ge 2*!PI)
     if ind[0] ne -1 then theta[ind]-=2*!PI
     pfss_sphpos[ff,1,0:npt-1]=theta
     
     phi=reform(atan(pos[1,*]/pos[0,*]))
;Check PHI for weird angles
     ind=where(phi lt 0.0)
     if ind[0] ne -1 then phi[ind]+=2*!PI
     ind=where(phi ge 2*!PI)
     if ind[0] ne -1 then phi[ind]-=2*!PI
     pfss_sphpos[ff,2,0:npt-1]=phi
  endfor


;-----------------------------------------
;2.3. Compute the locations where the field lines cross the spherical
;surface. Use algorithms from pfss_rad_field_crossing.pro and/or
;spherical_get_radial_crossing.pro - both in the PFSS package.
;Things to think about - will the surface be infinitely thin,
;or have some thickness?
;pfss_field_shock_crossing,pfss_cartpos,nstep,vertex_list,l,b
  ;save,filename='/home/kkozarev/Desktop/AIA/pro/test_sph_shock_crossing.sav',$
  ;     pfss_sphpos,nstep,sphvertex_list
  save,filename='/Users/kkozarev/AIA/test_sph_shock_crossing.sav',$
       pfss_sphpos,nstep,sphvertex_list


  ;stop
     pfss_field_shock_crossing_sph;,pfss_sphpos,nstep,sphvertex_list
     stop
     


     
;-----------------------------------------   
;2.4.     
;Once the crossing point has been found, find the relative orientation
;of the field to the local normal of the spherical surface (the angle
;theta_Bn). Should be easy.
     
;Store the theta_Bn information for the whole surface.
;Follow the feature with time and record the overall theta_Bn of the
;spherical surface.

;#################################################################################



  endfor
  
end
