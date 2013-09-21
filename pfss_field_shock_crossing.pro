;+
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;
;  pfss_field_shock_crossing.pro - This procedure finds the
;                                  intersection points of a set of field lines
;                                with a specified 3D surface (the shock)
;
;INPUT:
;      vertex_list - a 3D, [3,nspts] array of x-y-z shock surface
;                    coordinates, where nspts is the number of
;                    surface points
;      pfss_cartpos - [nlines,3,nlinpts] array for the PFSS lines' cartesian
;                   coordinates. nlinpts are number of points on a line,
;                   nlines is the number of lines.
;
;             11/04/2011 - Kamen Kozarev
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;-
pro pfss_field_shock_crossing,pfss_cartpos,nstep,vertex_list

;Constants and definitions
nlines=n_elements(pfss_cartpos[*,0,0])
nspts=n_elements(vertex_list[0,*])
DMIN=2 ;minimum pixel distance for crossing candidates.


;Look for the crossings line by line
for ff=0,nlines-1 do begin
   line=reform(pfss_cartpos[ff,*,*])

;To find crossings, look for the minimal distances between each coordinate of
;each point and the surface points.
   for p=0,nstep[ff]-1 do begin
      out
      
   endfor
   
   
   
   
endfor

;  linearly interpolate to get locus of crossing
      for j=0,ncr-1 do begin
         if total(ixm eq ixpt[j]) eq 0 then begin ;  avoids double-counting
            coeff=(rcut-lr[ixpt[j]])/(lr[ixpt[j]+1]-lr[ixpt[j]])
            ptcrth=90-((1-coeff)*lth[ixpt[j]]+coeff*lth[ixpt[j]+1])*!radeg
            ptcrph=(((1-coeff)*lph[ixpt[j]]+coeff*lph[ixpt[j]+1])*!radeg+360) $
                   mod 360
            data=[[data],[i,ptcrth,ptcrph]]      
            interpindex=[interpindex,ixpt[j]+coeff]
         endif
      endfor

end
