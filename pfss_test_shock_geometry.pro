pro pfss_test_shock_geometry,png=png,steps=steps
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
     restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs.sav'
  endif else begin
     
;for arien
     if pcname eq 'arien' then begin
        datapath='/Volumes/Backscratch/Users/kkozarev/AIA/events/05/'
                                ;restore,datapath+'normalized_AIA_20110125_05_193_subdata.sav'
        restore,datapath+'pfss_results_20110125_1.1Rs.sav'
     endif else begin
        restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs.sav'
     endelse
  endelse

;--------------------------------------------------------------


;--------------------------------------------------------------
;Constants and definitions
  loadct,8
  winsize=800
  xcenter=winsize/2.0
  ycenter=winsize/2.0
  sunrad=0.26*winsize
  shockrad=0.13*winsize

  if not keyword_set(steps) then nsteps=1.0 $
  else nsteps*=1.0

  tvlct,rr,gg,bb,/get

;Loop over all the steps
for i=0,steps-1 do begin

   if keyword_set(steps) then $
      shockrad=(winsize*0.14)*(1+i)/(nsteps+1)

;Rotation angles for the entire plot
  xrot_gen=(i*0.0)/nsteps
  yrot_gen=(i*0.0)/nsteps
  zrot_gen=(i*0.0)/nsteps
  
;Rotation angles for the PFSS points
  xrot_pfss=0+xrot_gen
  yrot_pfss=0+yrot_gen
  zrot_pfss=0+zrot_gen

;Latitude and longitude for the shock location
  lat=-20.0
  lon=-70.0

;Rotation angles for the shock surface points
  xrot_shock=-lat+xrot_gen
  yrot_shock=lon+yrot_gen
  zrot_shock=0+zrot_gen

;--------------------------------------------------------------




      Window, 0, Xsize=winsize, Ysize=winsize


;+==============================================================================
;1. Plot the field lines on disk center.

;Convert the spherical to x,y,z coordinates. 
;Switch y and z axes to make the coord. system right-handed.
  pfss_sphtocart,ptr,ptth,ptph,l,b,pfss_px,pfss_pz,pfss_py
  nlines=n_elements(pfss_px[0,*])
  
  ;Convert the pfss coordinates from Rs to pixels
  pfss_px*=sunrad
  pfss_py*=sunrad
  pfss_pz*=sunrad
  
;create rotation and translation matrix
  T3d, /Reset
  T3d, Rotate=[xrot_pfss, yrot_pfss, zrot_pfss]
  T3d, Translate=[xcenter, $
                  ycenter, $
                  0.0]

;Apply the rotations and translations and plot
  for ff=0,nlines-1,2 do begin
     npt=nstep[ff] ;the number of points in this particular line.
     pfss_cartpos = transpose([[reform(pfss_px[0:npt-1,ff])],$
                               [reform(pfss_py[0:npt-1,ff])],$
                               [reform(pfss_pz[0:npt-1,ff])]])
     pfss_cartpos = Vert_T3d(pfss_cartpos)
     plots,pfss_cartpos,color=250,/device
  endfor
  
;-==============================================================================
  

;+==============================================================================
;2. Draw a circle representing the solar disk.
  points = (2 * !PI / 399.0) * FINDGEN(400)
  x = xcenter + sunrad * cos(points)
  y = ycenter + sunrad * sin(points)
  plots,[x],[y],/device,psym=2,color=150,symsize=2
;-==============================================================================


;+==============================================================================
;3. Calculate and plot the spherical surface:

;Create the shock surface  
  MESH_OBJ, $
     4, $
     Vertex_List, Polygon_List, $ ;lists of polygons and vertices
     Replicate(shockrad, 100, 100)  , $
     p3=0,/degrees
;create rotation and translation matrix
  T3d, /Reset
  T3d, Rotate=[xrot_shock, yrot_shock, zrot_shock]
  T3d, Translate=[xcenter+sunrad*SIN((lon+yrot_gen)*!PI/180)*COS((lat+xrot_gen)*!PI/180), $
                  ycenter+sunrad*SIN((lat+xrot_gen)*!PI/180), $
                  0.0]
;apply rotation and translation to the surface
  Vertex_List = Vert_T3d(Vertex_List)
;plot the shock surface
  plots,Vertex_list,psym=5,color=150,symsize=0.2,/device

;-============================================================================== 

  
  if keyword_set(png) then begin
     image=tvrd(true=1)
     in=strtrim(string(i),2)
     if i lt 10 then in='0'+in
     fname='hairyball_'+in
     ;fname='hairyball'
     write_png,fname+'.png',image,rr,gg,bb
  endif

wait,0.2
endfor

end
