;+=====================================================================
pro test_aia_test_3dtransforms
  path='/Volumes/Backscratch/Users/kkozarev/AIA_data/studies/2011events/'
  ev='e37'
  
  shockfile=path+ev+'/AIA_20110511_37_193_shocklocations.sav'
  restore,shockfile
  radius=reform(radius[0,*])
  lon=subindex[0].arlon
  lat=subindex[0].arlat

  for t=0,n_elements(time)-1 do begin
     rad=(radius[t]-1)/subindex[init+t].rsun_ref*1000.*subindex[init+t].r_sun
     print,rad,lon,lat
     
     aia_test_3dtransforms,rad,lon,lat,subindex[init+t]
  endfor
end
;-=====================================================================



;+=====================================================================
pro aia_test_3dtransforms,radius,lon,lat,ind
;PURPOSE:
;This procedure tests visually the 3D transformations of the wave
;surfaces.
;
;CATEGORY:
;
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
; transform_volume
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/14/2012
;



;--------------------------------------------------------------
;Constants and definitions
  loadct,8,/silent
  winsize=1024
  sunrad=ind.r_sun              ;0.26*winsize
  shockrad=radius               ;0.18*winsize
  xcenter=ind.x0_mp ;winsize/2.0
  ycenter=ind.y0_mp ;winsize/2.0
  zcenter=sunrad*cos(ind[0].arlon*!PI/180.0)
  


;Latitude and longitude for the shock location
   ;lat=-90.0
   ;lon=50.0

;Rotation angles for the entire plot
   xrot_gen=0.0
   yrot_gen=0.0
   zrot_gen=0.0

   
;Rotation angles for the shock surface points
   xrot_shock=-lat+xrot_gen
   yrot_shock=lon+yrot_gen
   zrot_shock=0+zrot_gen
   
;--------------------------------------------------------------
   tvlct,rr,gg,bb,/get
  
;+===========================================
;2. Draw a circle representing the solar disk.
  points = (2 * !PI / 399.0) * FINDGEN(400)
  x = xcenter + sunrad * cos(points)
  y = ycenter + sunrad * sin(points)
  plots,[x],[y],/device,color=150,thick=2
;-==========================================


;+==========================================
;3. Calculate and plot the spherical surface:

;Create the shock surface
  MESH_OBJ, 4, Vertex_List, Polygon_List, $ ;lists of polygons and vertices
     Replicate(shockrad, 100, 100)  , $
     p3=-asin(shockrad/(2*sunrad))
  
 ;vertex_list=[0.0,0.0,0.0]
  ;Plot the original surface
  plots,Vertex_list,psym=sym(1),color=100,symsize=0.2,/device
  ;print,vertex_list

;Coordinates of the center of the shock surface.
  sc=[xcenter+sunrad*SIN((lon+yrot_gen)*!PI/180.)*COS((lat+xrot_gen)*!PI/180.), $
      ycenter+sunrad*SIN((lon+yrot_gen)*!PI/180.)*SIN((lat+xrot_gen)*!PI/180.), $
      sunrad*COS(lat*!PI/180.)]
  
  
;Transform the point locations
  Vertex_List = transform_volume(vertex_list,translate=[xcenter,ycenter,zcenter+sunrad])
  transmat=!P.T
  Vertex_List = transform_volume(vertex_list,$
                                 rotation=[xrot_shock,yrot_shock,zrot_shock],$
                                 centre_rotation=[xcenter,ycenter,zcenter])
  rotmat=!P.T
  

;plot the rotated surface
  plots,Vertex_list,psym=sym(1),color=150,symsize=0.2,/device


;Invert the transform
  vertex_list = transform_volume(vertex_list,t3dmat=invert(rotmat))
  vertex_list = transform_volume(vertex_list,t3dmat=invert(transmat))
;  vertex_list = transform_volume(vertex_list,$
;                                 rotation=[-xrot_shock,-yrot_shock,-zrot_shock],$
;                                 centre_rotation=[xcenter,ycenter,zcenter])
;  vertex_list = transform_volume(vertex_list,translate=[-xcenter,-ycenter,-zcenter-sunrad]) 
;print,vertex_list
  
;plot the de-rotated surface to test the inversion
  plots,vertex_list,psym=sym(1),color=200,symsize=0.2,/device ;,/device
;-====================

wait,0.2

loadct,0,/silent
end
;-=====================================================================
