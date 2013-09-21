pro test_aia_plot_hemisphere
;;TEST CASE
restore,'/Volumes/Backscratch/Users/kkozarev/AIA_data/studies/2011events/e05/normalized_AIA_20110125_05_193_subdata.sav'
index=subindex
wdef,0,1024
tvscl,subdata[*,*,0]
arcoords=[-20,-70]
radius=300

aia_plot_hemisphere,index,radius,arcoords=arcoords,vertex_list=vertex_list

stop
end

pro aia_plot_hemisphere,index,radius,arcoords=arcoords,vertex_list=vertex_list
;This program will overplot a hemispherical surface on an AIA image,
;given the image index, which should contain the AR coordinates and
;the solar radius.
;It will optionally return the vertex list of the hemisphere.
;Kamen Kozarev, 10/20/2011

loadct,8,/silent

if n_elements(index) gt 1 then index=index[0]

if not keyword_set(arcoords) then begin
   if not tag_exist(index,'ARLON') then begin
      print,''
      print,'AR coordinates not present in index.'
      print,'Please specify them with the arcoords keyword. Quitting...'
      print,''
      return
   endif else begin
      arcoords= [index.ARLON,index.ARLAT];[index.ARX0,index.ARY0]
   endelse
endif

sunrad=index.R_SUN
xcenter=index.X0_MP
ycenter=index.Y0_MP
;radius=200 ;radius of the hemisphere in pixels
lon=arcoords[0]
lat=arcoords[1]

circ=aia_circle(xcenter,ycenter,sunrad,/plot)

;Rotation angles corresponding to the AR location:
xrot=-lat
yrot=lon
zrot=0

    ; print,'X rotation: '+strtrim(string(xrot),2)
    ; print,'Y rotation: '+strtrim(string(yrot),2)
    ; print,'Z rotation: '+strtrim(string(zrot),2)
   
; Create a spherical mesh, cut in half.
type=4  ; sphere
MESH_OBJ, $
   4, $
   Vertex_List, Polygon_List, $ ;lists of polygons and vertices
   Replicate(radius, 200, 200)  , $
   p3=-asin(radius/(2*sunrad)),/degrees

; T3d prepares a 4x4 matrix for transformation of coordinates.
; The resulting matrix is saved in the variable !P.T
T3d, /Reset
                                ;Rotation around the x-,y-, and z-axes.
T3d, Rotate=[xrot, yrot, zrot]
                                ;Translation
T3d, Translate=[xcenter+sunrad*SIN(lon*!PI/180)*COS(lat*!PI/180), $
                ycenter+sunrad*SIN(lat*!PI/180), $
                0.0]

; Apply the transformation to the vertex list of the mesh object.
vertex_list = Vert_T3d(vertex_list)


; Render the mesh.
plots,vertex_list,psym=3,color=150,symsize=3,/device


loadct,0



end
