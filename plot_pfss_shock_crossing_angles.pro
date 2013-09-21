pro plot_pfss_shock_crossing_angles,allcrossPoints,nsteps,allcrosses,radiusfitlines,vertex_list,allcrossangles,subindex,suncenter,vert_rotmat,vert_transmat,sc,datapath=datapath
;Plot the crossing points on the polar projection of the shock
;surface with their color signifying the crossing angle.
;If I want to make a movie out of it, then plot this for every step
;with the same size dots. If it is to be presented as a single slide,
;make the size of the dots correspond to the number of points with
;that angle in that area of the projection.
  deproj_cross_points = allcrossPoints
  sunrad=subindex[0].r_sun+10
  kmpx=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)
  minshockrad = radiusfitlines[0]/kmpx
  maxshockrad = radiusfitlines[nsteps-1]/kmpx
  xcenter=suncenter[0]
  ycenter=suncenter[1]
  if not keyword_set(datapath) then datapath='./'
;First need to derotate the points so they can be plotted as a polar
;projection. Create rotation and translation matrices.

;---------------------------------------------------------------------------------------
;TRANSFORM THE SHOCK SURFACE POINTS
;---------------------------------------------------------------------------------------
  Vertex_List = transform_volume(vertex_list,t3dmat=invert(vert_rotmat))
  Vertex_List = transform_volume(vertex_list,t3dmat=invert(vert_transmat))
;---------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------
;TRANSFORM THE PFSS POINTS
;---------------------------------------------------------------------------------------
;  for sstep=0,nsteps-1 do begin
;     shockrad=radiusfitlines[sstep]/kmpx
;     shscale=maxshockrad/shockrad
;    pos=reform(allcrossPoints[sstep,*,0:allcrosses[sstep]-1])
;     pos=transform_volume(pos,t3dmat=invert(vert_rotmat))
;     pos=transform_volume(pos,t3dmat=invert(vert_transmat))
;     pos=transform_volume(pos,scale=[shscale,shscale,shscale])
;     deproj_cross_points[sstep,*,0:allcrosses[sstep]-1]=pos ; * maxshockrad/shockrad
;  endfor
;---------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------
;PLOTTING SECTION
;---------------------------------------------------------------------------------------
;Calculate the levels for the color plotting.
  nlev=36
  th=allcrossangles
  minn=min(th[where(th gt 0.0 and finite(th))])
  maxx=max(th[where(th gt 0.0 and finite(th))])
  levstep=abs(alog10(minn)-alog10(maxx))/nlev
  levels=minn*10^(findgen(nlev)*levstep)
  thlet='!4' + String("110B) + '!X'
  
  ;figure out the x and y range for plotting the points.
  ;tmp=deproj_cross_points[*,0,*]
  ;xrng = minmax(reform(tmp[where(tmp gt 0.0)]))
  xrng=[-maxshockrad*1.001,maxshockrad*1.001]
  ;tmp=deproj_cross_points[*,1,*]
 ; yrng = minmax(reform(tmp[where(tmp gt 0.0)]))
  yrng=[-maxshockrad*1.001,maxshockrad*1.001]

  !P.position=[0.18,0.14,0.8,0.92]
  !P.charthick=2
  wdef,0,1000,800

  for sstep=0,nsteps-1 do begin


     shockrad=radiusfitlines[sstep]/kmpx
     shscale=maxshockrad/shockrad
     pos=reform(allcrossPoints[sstep,*,0:allcrosses[sstep]-1])
     
     pos=transform_volume(pos,t3dmat=invert(vert_rotmat))
     pos=transform_volume(pos,t3dmat=invert(vert_transmat))
     pos=transform_volume(pos,scale=[shscale,shscale,shscale])
     
     flx=reform(pos[0,*]) ;reform(deproj_cross_points[sstep,0,0:allcrosses[sstep]-1])
     fly=reform(pos[1,*]) ;reform(deproj_cross_points[sstep,1,0:allcrosses[sstep]-1])
     
     th=reform(allcrossangles[sstep,0:allcrosses[sstep]-1])
 
     ;Plot Axes and the shock mesh
     if sstep eq 0 then begin
        loadct,0,/silent
        
        PLOT, flx, fly, PSYM = 3, $ 
              TITLE = thlet+'!DBN!N at B-Shock Crossings', $
              XTITLE = 'X', $
              YTITLE = 'Y', $
              xrange=xrng,yrange=yrng,$
              xstyle=1,ystyle=1,color=0,background=255,$
              xthick=3,ythick=3,thick=3,charsize=3,/nodata
        PLOTS,vertex_list[0,*],vertex_list[1,*],psym=3,color=0 ,/data
        loadct,13,/silent
     endif
     
     
     ;Contour data on regular grid
     PLOTS,flx,fly,psym=sym(1),symsize=1.8,color=(th-minn)/(maxx-minn)*254.,/data
     
 
     if sstep lt nsteps-1 then begin
        image=tvrd(true=1)
        in=strtrim(string(sstep),2)
        if sstep lt 100 then in='0'+in
        if sstep lt 10 then in='0'+in
        fname=datapath+'thetabn_orientation_'+in+'.png'
        write_png,fname,image,rr,gg,bb
     endif

  endfor
  
  
  fcolorbar, MIN=minn,MAX=maxx,Divisions=4, $
             Color=0,VERTICAL=1,RIGHT=1, TITLE=thlet+'!DBN!N [deg]',$
             CHARSIZE=3, format='(i)',Position=[0.9, 0.4, 0.93, 0.8]
  
  image=tvrd(true=1)
  in=strtrim(string(sstep),2)
  if sstep lt 100 then in='0'+in
  if sstep lt 10 then in='0'+in
  fname=datapath+'thetabn_orientation_'+in+'.png'
  write_png,fname,image,rr,gg,bb

;---------------------------------------------------------------------------------------


end
