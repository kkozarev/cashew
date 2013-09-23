pro test_pfss_shock_crossing,png=png,steps=steps,plot=plot
;PURPOSE:
;This procedure tests visually the geometrical overlap of the pfss
;results to the spherical shell model. It does so by plotting the two
;on the center of the solar disk.
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
;pfss_sphtocart,
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 11/08/2011
;

;--------------------------------------------------------------
;LOAD DATA
  print,''
  print,'Loading data...'
  
;Figure out the name of the local machine.
  pcname=hostname()

;for sloncho-2
  if pcname eq 'sloncho-2' then begin
;restore,'/Users/kkozarev/AIA/algoTests/yaftawave/normalized_AIA_20110125_05_193_subdata.sav'
     restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs_dens_2.sav'
     ;restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs.sav'
  endif else begin
     
;for arien
     if pcname eq 'arien' then begin
        datapath='/Volumes/PLUME/AIA_data/studies/2011events/e113/'
                                ;restore,datapath+'normalized_AIA_20110125_05_193_subdata.sav'
        ;restore,datapath+'pfss_results_20110125_1.0Rs.sav'
        ;restore,datapath+'pfss_results_20100613_1.1Rs_dens_2.sav'
     endif else begin
        restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.0Rs_dens_1.sav'
     endelse
  endelse

;--------------------------------------------------------------


;--------------------------------------------------------------
;Constants and definitions
  loadct,8,/silent
  winsize=800
  xcenter=winsize/2.0
  ycenter=winsize/2.0
  sunrad=0.26*winsize
  shockrad=0.15*winsize

  if not keyword_set(steps) then nsteps=10.0 $
  else nsteps=steps*1.0

  if keyword_set(png) then plot=1
  
  allcrossPoints=fltarr(nsteps,3,100000)
  allcrossAngles=fltarr(nsteps,100000)
  allcrosses=fltarr(nsteps)
  

  tvlct,rr,gg,bb,/get

;Loop over all the steps
for sstep=0,nsteps-1 do begin
   print,'Step #'+string(sstep)

   if keyword_set(steps) then $
      shockrad=sunrad*0.1+(sunrad*0.5*sstep)/(nsteps-1)
   
;Rotation angles for the entire plot
   xrot_gen=(sstep*0.0)/nsteps
   yrot_gen=(sstep*0.0)/nsteps
   zrot_gen=(sstep*0.0)/nsteps
   
;Rotation angles for the PFSS points
   xrot_pfss=0+xrot_gen
   yrot_pfss=0+yrot_gen
   zrot_pfss=0+zrot_gen
   
;Latitude and longitude for the shock location
   lat=-10.0
   lon=50.0
   
;Rotation angles for the shock surface points
   xrot_shock=-lat+xrot_gen
   yrot_shock=lon+yrot_gen
   zrot_shock=0+zrot_gen
   
;--------------------------------------------------------------

 if keyword_set(plot) then Window, 0, Xsize=winsize, Ysize=winsize

;+==============================================================================
;1. Plot the field lines on disk center.
  if sstep eq 0 then begin
     l=20*!PI/180.0
     b=25*!PI/180.0
;Convert the spherical to x,y,z coordinates. 
;Switch y and z axes to make the coordinate system right-handed.
     pfss_sphtocart,ptr,ptth,ptph,l,b,pfss_px,pfss_pz,pfss_py
     nlines=n_elements(pfss_px[0,*])
     maxnpts=n_elements(pfss_px[*,0])  
     
     ;Convert the pfss coordinates from Rs to pixels
     pfss_px*=sunrad
     pfss_py*=sunrad
     pfss_pz*=sunrad
     
;create rotation and translation matrix
     T3d, /Reset
     T3d, Rotate=[xrot_pfss, yrot_pfss, zrot_pfss]
     T3d, Translate=[xcenter, ycenter, 0.0]
     
;Apply the rotations and translations and plot
     pfss_cartpos=fltarr(nlines,3,maxnpts)
     for ff=0,nlines-1 do begin
        npt=nstep[ff]           ;the number of points in this particular line.
        pos = transpose([[reform(pfss_px[0:npt-1,ff])],$
                         [reform(pfss_py[0:npt-1,ff])],$
                         [reform(pfss_pz[0:npt-1,ff])]])
        pos = Vert_T3d(pos)
        pfss_cartpos[ff,*,0:npt-1]=pos
     endfor
     ;Free some memory
     pfss_px=0
     pfss_pz=0
     pfss_py=0
     pos=0
     ptr=0
     ptth=0
     ptph=0
  endif
  
  if keyword_set(plot) then begin
     for ff = 0,nlines-1,4 do begin
        npt = nstep[ff]         ;the number of points in this particular line.
        pos = reform(pfss_cartpos[ff,*,0:npt-1])
        plots,pos,color=250,/device,psym=3 ;,psym=sym(1),symsize=0.25
     endfor
  endif
;-==============================================================================
  
  
;+==============================================================================
;2. Draw a circle representing the solar disk.
  points = (2 * !PI / 399.0) * FINDGEN(400)
  x = xcenter + sunrad * cos(points)
  y = ycenter + sunrad * sin(points)
  if keyword_set(plot) then $
  plots,[x],[y],/device,psym=sym(1),color=150,symsize=1
;-==============================================================================


;+==============================================================================
;3. Calculate and plot the spherical surface:

;Create the shock surface
; Calculate the latitudinal extension of the shock surface below theta=0.
  ;print,-asin(shockrad/(2*sunrad))
  ;stop
  MESH_OBJ, $
     4, $
     Vertex_List, Polygon_List, $ ;lists of polygons and vertices
     Replicate(shockrad, 100, 100)  , $
     p3=-asin(shockrad/(2*sunrad))

  ;Coordinates of the center of the shock surface.
  sc=[xcenter+sunrad*SIN((lon+yrot_gen)*!PI/180.)*COS((lat+xrot_gen)*!PI/180.), $
      ycenter+sunrad*SIN((lon+yrot_gen)*!PI/180.)*SIN((lat+xrot_gen)*!PI/180.), $
      sunrad*COS(lat*!PI/180.)]

;create rotation and translation matrix
  T3d, /Reset
  T3d, Rotate=[xrot_shock, yrot_shock, zrot_shock]
  T3d, Translate=[sc[0],sc[1],sc[2]]
;apply rotation and translation to the surface
  Vertex_List = Vert_T3d(Vertex_List)
  nverts=n_elements(vertex_list[0,*])
;plot the shock surface
  if keyword_set(plot) then $
  plots,Vertex_list,psym=sym(1),color=150,symsize=0.4,/device
;-============================================================================== 



;+==============================================================================
;4. Determine the points where the field lines cross the shock
;surface.
  dr2=(reform(pfss_cartpos[*,0,*])-sc[0])^2 + (reform(pfss_cartpos[*,1,*])-sc[1])^2 + $
      (reform(pfss_cartpos[*,2,*])-sc[2])^2
  mind=1.0
  ptind=where(dr2 le (shockrad+mind)^2 and dr2 ge (shockrad-mind)^2)


  if ptind[0] gt -1 then begin
     pind=array_indices(reform(pfss_cartpos[*,0,*]),ptind)
     ncrosses=n_elements(pind[0,*])
  endif else begin
     ;Plot the image, then continue to the next frame.
     if keyword_set(png) then begin
        image=tvrd(true=1)
        in=strtrim(string(sstep),2)
        if sstep lt 10 then in='0'+in
        fname='hairyball_'+in
        write_png,fname+'.png',image,rr,gg,bb
     endif

     print,'No field/shock crossings. Continuing...'
     continue
  endelse

  cross_points=fltarr(3,ncrosses)
  for i=0,ncrosses-1 do cross_points[*,i]=reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
  allcrossPoints[sstep,*,0:ncrosses-1]=cross_points
  
  ;find the normals at the crossing
  ;points and the local field direction.
  ;Compute ThetaBN.
  normals=fltarr(3,ncrosses)
  local_field=fltarr(3,ncrosses)


  ;find the normals
  for i=0,2 do normals[i,*] = cross_points[i,*] - sc[i]
  ;find the local field direction
  for i=0,ncrosses-1 do local_field[*,i] = reform(pfss_cartpos[pind[0,i],*,pind[1,i]+1]) - $
     reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
  
  ;compute the dot products
  dotp=normals[0,*]*local_field[0,*] + normals[1,*]*local_field[1,*] + normals[2,*]*local_field[2,*]
  dotp=reform(dotp)
  
  ;The angles are acos((a dot b)/(|a|*|b|))
  prod=total(abs(normals),1) * total(abs(local_field),1)
  thetabn=acos(dotp/prod)
  ind=where(thetabn gt !PI/2.0)
  if ind[0] gt -1 then thetabn[ind]=!PI-thetabn[ind]
  th=thetabn*180/!PI
  allcrossAngles[sstep,0:ncrosses-1]=th
  allcrosses[sstep]=ncrosses

  ;bins=5
  ;hist=histogram(th,binsize=bins)
  ;bins = FINDGEN(N_ELEMENTS(hist))*bins + MIN(th)
  ;PLOT, bins, hist, YRANGE = [MIN(hist)-3, MAX(hist)+3], PSYM = 10, $ 
  ;      XTITLE = 'Theta!DBn!N', YTITLE = 'Density per Bin',$
  ;      XRANGE= [min(th)-1,max(th)+1],xstyle=1,ystyle=1

  ;stop


;Plot the field lines that pass through the shock surface
  loadct,13,/silent
  colors=abs(randomn(10L,ncrosses))*255.
  unique_lines=pind[*,uniq(pind[0,*])]
  pind=unique_lines
  ncrosses=n_elements(pind[0,*])

  if keyword_set(plot) then begin
     for ff=0,ncrosses-1 do begin
        npt=nstep[pind[0,ff]]
        plots,reform(pfss_cartpos[pind[0,ff],*,0:npt-1]),$
              color=colors[ff],/device,psym=sym(1),symsize=0.3
     endfor
     
;plot the points of crossing in red.
     plots,cross_points,color=240,psym=sym(1),symsize=0.8,/device
  endif

  if keyword_set(png) then begin
     image=tvrd(true=1)
     in=strtrim(string(sstep),2)
     if sstep lt 10 then in='0'+in
     fname='hairyball_'+in
     write_png,fname+'.png',image,rr,gg,bb
  endif
;-==============================================================================



  wait,0.2
  loadct,8,/silent
endfor



;+==============================================================================
;5. Plot the histogram of all angles at all steps.
binsiz=5

;for sstep=0,nsteps-1 do begin
;   th=allcrossangles[sstep,0:allcrosses[sstep]-1]
;   hist=histogram(th,binsize=binsiz)
;   bins = FINDGEN(N_ELEMENTS(hist))*binsiz + MIN(th)
;   if sstep eq 0 then PLOT, bins, hist, YRANGE = [10, 500], PSYM = 10, $ 
;                        XTITLE = 'Theta!DBn!N', YTITLE = 'Density per Bin',$
;                        XRANGE= [min(th),max(th)+1],xstyle=1,ystyle=1 $
;   else oplot,bins,hist,psym=10
;endfor
;-==============================================================================

pfss_cartpos=0

;+==============================================================================
;6. Plot the crossing points on the polar projection of the shock
;surface with their color signifying the crossing angle.
;If I want to make a movie out of it, then plot this for every step
;with the same size dots. If it is to be presented as a single slide,
;make the size of the dots correspond to the number of points with
;that angle in that area of the projection.

window,0,xsize=winsize*1.1,ysize=winsize

;First need to derotate the points so they can be plotted as a polar
;projection.
;create rotation and translation matrix
  T3d, /Reset
  T3d, Rotate=[-xrot_shock, -yrot_shock, -zrot_shock]
  T3d, Translate=[-sc[0],-sc[1],-sc[2]]
  
  deproj_cross_points = allcrossPoints
  maxshockrad = 0.6*sunrad
  for sstep=0,nsteps-1 do begin
     deproj_cross_points[sstep,*,0:allcrosses[sstep]-1] = $
        Vert_T3d(reform(allcrossPoints[sstep,*,0:allcrosses[sstep]-1]))
     ;Need to rescale the point locations to a single shock size
     ;for plotting all on a single slide.
     if keyword_set(steps) then begin
        shockrad = sunrad*0.1+(sunrad*0.5*sstep)/(nsteps-1)
        shockrad_ratio = maxshockrad/shockrad
     endif else begin
        shockrad_ratio = 1.0
     endelse
                                ;print,'shockrad_ratio: '+string(shockrad_ratio)
     deproj_cross_points[sstep,*,0:allcrosses[sstep]-1] *= shockrad_ratio
  endfor

;apply rotation and translation to the surface
  Vertex_List = Vert_T3d(Vertex_List)
  
  
;Then, do the actual plotting:
;Calculate the levels for the color plotting.
  nlev=36
  th=allcrossangles
  minn=min(th[where(th gt 0.0 and finite(th))])
  maxx=max(th[where(th gt 0.0 and finite(th))])
  levstep=abs(alog10(minn)-alog10(maxx))/nlev
  levels=minn*10^(findgen(nlev)*levstep)
  thlet='!4' + String("110B) + '!X'

  ;figure out the x and y range for plotting the points.
  tmp=allcrossPoints[*,0,*]
  xrng = minmax(reform(tmp[where(tmp gt 0.0)]))
  tmp=allcrossPoints[*,1,*]
  yrng = minmax(reform(tmp[where(tmp gt 0.0)]))
     
  for sstep=0,nsteps-1 do begin
     flx = reform(allcrossPoints[sstep,0,0:allcrosses[sstep]-1])
     fly = reform(allcrossPoints[sstep,1,0:allcrosses[sstep]-1])
     th=reform(allcrossangles[sstep,0:allcrosses[sstep]-1])
    ; print,minmax(flx)
    ; print,minmax(fly)
     loadct,0,/silent

     !P.position=[0.1,0.1,0.84,0.84]
     !P.charthick=2
;Plot Axes
    ;if sstep eq 0 then $
       PLOT, flx, fly, PSYM = 3, $ 
             TITLE = thlet+'!DBn!N, degrees', $
             XTITLE = 'X', $
             YTITLE = 'Y', $
             xrange=xrng,yrange=yrng,$
             xstyle=1,ystyle=1,color=0,background=255,$
             xthick=3,ythick=3,thick=3,charsize=2,/nodata
     
;plot the shock mesh
;     if sstep eq 0 then $
;        PLOTS,vertex_list,psym=3,color=0

     

     loadct,13,/silent
     ;Contour data on regular grid
     PLOTS,flx,fly,psym=sym(1),symsize=2,color=(th-minn)/(maxx-minn)*255.
     
     fcolorbar, MIN=minn,MAX=maxx,Divisions=4, $
                Color=0,VERTICAL=1,RIGHT=1, TITLE=thlet+'!DBn!N [deg]',$
                CHARSIZE=1.8, format='(i)',Position=[0.9, 0.4, 0.89, 0.7]

     image=tvrd(true=1)
     in=strtrim(string(sstep),2)
     if sstep lt 10 then in='0'+in
     fname='thetabn_'+in
     write_png,fname+'.png',image,rr,gg,bb

     wait,0.2
  endfor
  

;-==============================================================================

stop

loadct,0,/silent
end














;####################### 2. SHOCK TO FIELD ORIENTATION ##########################
     
;-----------------------------------------
;2.1. Convert the spherical surface points to spherical coordinates for
;easier searching.
;When converting the spherical surface to heliographic coordinates,
;switch its x- ad y-axis, and convert length to solar radii.

;Simply use the regular conversions between cartesian and spherical
;coordinates. Stay on the IDL coordinate system.
;     sphvertex_list=fltarr(3,nverts)
;     
;     pos=vertex_list/sunrad
;     
;     r=sqrt(pos[0,*]^2 + pos[1,*]^2 + pos[2,*]^2)
     ;Convert X/Y/Z distances to Rs
;     sphvertex_list[0,*]=r;/sunrad+1

;     theta=reform(acos(pos[2,*]/r))
;Check THETA for weird angles
;     ind=where(theta lt 0.0)
;     if ind[0] ne -1 then theta[ind]+=2*!PI
;     ind=where(theta ge 2*!PI)
;     if ind[0] ne -1 then theta[ind]-=2*!PI
;     sphvertex_list[1,*]=theta
     
;     phi=reform(atan(pos[1,*]/pos[0,*]))
;Check PHI for weird angles
;     ind=where(phi lt 0.0)
;     if ind[0] ne -1 then phi[ind]+=2*!PI
;     ind=where(phi ge 2*!PI)
;     if ind[0] ne -1 then phi[ind]-=2*!PI
;     sphvertex_list[2,*]=phi

;-----------------------------------------
;2.2. Convert the PFSS points to spherical coordinates for
;easier searching.
;  pfss_sphpos=fltarr(nlines,3,maxnpts)
;  for ff=0,nlines-1 do begin
;     npt=nstep[ff]
;     pos=reform(pfss_cartpos[ff,*,0:npt-1])/sunrad

 
;     r=sqrt(pos[0,*]^2 + pos[1,*]^2 + pos[2,*]^2)
;     pfss_sphpos[ff,0,0:npt-1]=r;/sunrad-1.0
     
;     theta=reform(acos(pos[2,*]/r))
     ;Check THETA for weird angles
;     ind=where(theta lt 0.0)
;     if ind[0] ne -1 then theta[ind]+=2*!PI
;     ind=where(theta ge 2*!PI)
;     if ind[0] ne -1 then theta[ind]-=2*!PI
;     pfss_sphpos[ff,1,0:npt-1]=theta
     
;     phi=reform(atan(pos[1,*]/pos[0,*]))
;Check PHI for weird angles
;     ind=where(phi lt 0.0)
;     if ind[0] ne -1 then phi[ind]+=2*!PI
;    ind=where(phi ge 2*!PI)
;     if ind[0] ne -1 then phi[ind]-=2*!PI
;     pfss_sphpos[ff,2,0:npt-1]=phi
;  endfor



;-----------------------------------------
;2.3. Search for overlaps between lines and the shock surface.
;pfss_field_shock_crossing_sph,pfss_sphpos,nstep,sphvertex_list
;nspts=n_elements(sphvertex_list[0,*])
;DMIN_R=1 ;minimum pixel distance for radial crossing candidates.
;DMIN_ANG=5 ;minimum pixel distance for lat/lon crossing candidates.

;dr=abs(sphvertex_list[0,1]-sphvertex_list[0,0])
;dth=abs(sphvertex_list[1,1]-sphvertex_list[1,0])
;dph=abs(sphvertex_list[2,1]-sphvertex_list[2,0])

;rrange=minmax(sphvertex_list[0,*])
;trange=minmax(sphvertex_list[1,*])
;prange=minmax(sphvertex_list[2,*])

;pfss_rrange=minmax(pfss_sphpos[*,0,*])
;pfss_trange=minmax(pfss_sphpos[*,1,*])
;pfss_prange=minmax(pfss_sphpos[*,2,*])

;loadct,13,/silent

;create colors for individual field lines.
;colors=abs(randomn(seed,nlines))*255.

;for ff=0,nlines-1 do begin
;npt=nstep[ff]
;seed=2

;line=reform(pfss_sphpos[ff,*,0:npt-1])


;Search for the intersections.

;   ptarr=fltarr(3,npt)
   ;print,'Field line '+string(ff)

 ;First check if any of the line's points cross the
 ;r-theta-phi ranges of the surface.
;   tprin=-1
;   tpin=-1
;   pin=where(line[2,*] ge prange[0] and line[2,*] le prange[1])
;   tin=where(line[1,*] ge trange[0] and line[1,*] le trange[1])
;   rin=where(line[0,*] ge rrange[0] and line[0,*] le rrange[1])
   
   
;   if pin[0] gt -1 and tin[0] gt -1 and rin[0] gt -1 then begin
;      tpin=where(pin eq tin)
;      print,minmax(line[0,*])
;      print,n_elements(pin),n_elements(tin),n_elements(rin)
;      print,n_elements(tpin)
      ;if tpin[0] lt 0 then continue else begin
                                ;Plot the individual field line in a different color
;         plots,reform(pfss_cartpos[ff,*,0:npt-1]),$
;               color=colors[ff],/device,psym=sym(1),symsize=0.3
         ;stop
     ;endelse
;   endif else continue

;endfor

;#################################################################################
