pro test_mesh_obj,testnum,lat,lon

;To test this procedure, uncomment the following
;restore,'/Users/kkozarev/AIA/algoTests/mesh_obj/'+'normalized_AIA_20110125_05_193_subdata.sav'
;tvscl,subdata[*,*,0]
;lat=
;lon=

;test the mesh_obj procedure of the IDL Direct Graphics

  if testnum eq 1 then begin
     loadct,8
     winsize=800
     xcenter=400
     ycenter=400
     radius=200
     
;Before rotating the hemisphere, need to figure out what the local
;normal orientation is
     xrot=-lat
     yrot=lon
     zrot=0

     print,'X rotation: '+strtrim(string(xrot),2)
     print,'Y rotation: '+strtrim(string(yrot),2)
     print,'Z rotation: '+strtrim(string(zrot),2)


     ;set_plot,'ps'
     ;fname='test_'+strtrim(string(testnum),2)
     ;device, file=fname+'.eps', /inches,xsize=10.0, ysize=10, /encaps, /color     
     type=4   ; cylinder
; Create a sphere.
     MESH_OBJ, $
        type, $
        Vertex_List, Polygon_List, $ ;lists of polygons and vertices
        Replicate(radius/2, 100, 100)  , $
        p3=0,/degrees
     
; T3d prepares a 4x4 matrix for transformation of coordinates.
; The resulting matrix is saved in the variable !P.T
     T3d, /Reset
     ;Rotation around the x-,y-, and z-axes.
     T3d, Rotate=[xrot, yrot, zrot]
     ;T3d, Rotate=[xrot, 0, 0]
     ;T3d, Rotate=[0, 0, zrot]
     ;Linear translation in x,y,z
     T3d, Translate=[xcenter+radius*SIN(lon*!PI/180)*COS(lat*!PI/180), $
                     ycenter+radius*SIN(lat*!PI/180), $
                     0.0]
     
; Apply the transformation to the vertex list of the mesh object.
     Vertex_List = Vert_T3d(Vertex_List)
     
; Create the window and view.
     Window, 0, Xsize=winsize, Ysize=winsize
     ;Create_View, Winx=winsize, Winy=winsize
  
; Render the mesh.
     ;Set_Shading, Light=[0.5, 0.3, 2.0], Reject=0
     ;Tvscl, Polyshade(Vertex_List, Polygon_List, /device)
     plots,Vertex_list,psym=3,color=150,symsize=3,/device

     ;Draw a circle
     points = (2 * !PI / 199.0) * FINDGEN(200)
     x = xcenter + radius * cos(points)
     y = ycenter + radius * sin(points)
     plots,[x],[y],/device,psym=3
     stop

;device,/close
;exec='convert -flatten '+fname+'.eps '+fname+'.png; rm '+fname+'.eps'
;spawn,exec
;set_plot,'x'

  endif

  if testnum eq 2 then begin
;Here, test the concept of creating a surface of revolution from the
;front of the YAFTA-tracked points.

; Create a cone (surface of revolution).
     MESH_OBJ, 6, Vertex_List, Polygon_List, $
               [[0.75, 0.0, 0.25], [0.5, 0.0, 0.75]], P1=30, P2=[0.5, 0.0, 0.0]
     
; Create the window and view.
     Window, 0, Xsize=512, Ysize=512
     Create_View, Winx=512, Winy=512, Ax=30.0, Ay=(140.0), Zoom=0.5
     
; Render the mesh.
     Set_Shading, Light=[-0.5, 0.5, 2.0], Reject=0
     Tvscl, Polyshade(Vertex_List, Polygon_List, /Data, /T3d)
     
  endif


end
