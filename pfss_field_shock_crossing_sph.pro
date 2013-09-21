;+
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;
;  pfss_field_shock_crossing_sph.pro - This procedure finds the
;                                  intersection points of a set of field lines
;                                with a specified 3D surface (the shock)
;
;INPUT:
;      sphvertex_list - a 3D, [3,nspts] array of r-theta-phi shock surface
;                    coordinates, where nspts is the number of
;                    surface points
;      pfss_sphpos - [nlines,3,nlinpts] array for the PFSS lines' spherical
;                   coordinates. nlinpts are number of points on a line,
;                   nlines is the number of lines.
;
;             11/04/2011 - Kamen Kozarev
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;-
pro pfss_field_shock_crossing_sph;,pfss_sphpos,nstep,sphvertex_list
;This procedure assumes that all the coordinate conversions have been
;performed correctly, and that both position arrays are in
;heliographic coordinates.
  
;TEST CASE
;To test this procedure, comment out the arguments above and uncomment
;the lines below
;--------------------------------------------------------------
  pcname=hostname()
  case pcname of
     'sloncho-2': $
     restore,'/Users/kkozarev/AIA/test_sph_shock_crossing.sav'
     'arien': $
     restore,'/home/kkozarev/Desktop/AIA/pro/test_sph_shock_crossing.sav'
     else: $
        restore,'/Users/kkozarev/AIA/test_sph_shock_crossing.sav'
endcase
;--------------------------------------------------------------
;TEST CASE

;Constants and definitions
nlines=n_elements(pfss_sphpos[*,0,0])
nspts=n_elements(sphvertex_list[0,*])
DMIN_R=1 ;minimum pixel distance for radial crossing candidates.
DMIN_ANG=5 ;minimum pixel distance for lat/lon crossing candidates.

dr=abs(sphvertex_list[0,1]-sphvertex_list[0,0])
dth=abs(sphvertex_list[1,1]-sphvertex_list[1,0])
dph=abs(sphvertex_list[2,1]-sphvertex_list[2,0])

rrange=minmax(sphvertex_list[0,*])
trange=minmax(sphvertex_list[1,*])
prange=minmax(sphvertex_list[2,*])

pfss_rrange=minmax(pfss_sphpos[*,0,*])
pfss_trange=minmax(pfss_sphpos[*,1,*])
pfss_prange=minmax(pfss_sphpos[*,2,*])


;Look for the crossings line by line
for ff=0,nlines-1 do begin
   npt=nstep[ff]
   line=reform(pfss_sphpos[ff,*,0:npt-1])
   ptarr=fltarr(3,npt)
   print,'Field line '+string(ff)

 ;First check if any of the line's points cross the
 ;r-theta-phi ranges of the surface.
   tprin=-1
   tpin=-1
   pin=where(line[2,*] ge prange[0] and line[2,*] le prange[1])
   tin=where(line[1,*] ge trange[0] and line[1,*] le trange[1])
   rin=where(line[0,*] ge rrange[0] and line[0,*] le rrange[1])
   
   print,minmax(line[0,*])
   print,n_elements(pin),n_elements(tin),n_elements(rin)
   
   
   if pin[0] gt -1 and tin[0] gt -1 then tpin=where(pin eq tin)
   print,n_elements(tpin)
   stop
   if tpin[0] gt -1 then tprin=where(pin[tpin] eq rin)
   if tprin[0] gt -1 then begin
;These are the common elements
   ;line[2,pin[tpin]]
   ;line[1,tin[tpin]]
  ; if n_elements(pin) gt 1 and n_elements(tin) gt 1 and n_elements(rin) gt 1 then begin


;To find crossings, look for the minimal distances between each coordinate of
;each point and the surface points.
      for p=1,nstep[ff]-1 do begin
         dist_r=min(abs(line[0,p]-sphvertex_list[0,*]),rind)
         if dist_r le dr then begin
                                ;print,'R:',string(ff),string(p),string(rind)
            dist_th=min(abs(line[1,p]-sphvertex_list[1,*]),tind)
            if dist_th le dth then begin
                                ;print,'TH:',string(ff),string(p),string(tind)
               dist_ph=min(abs(line[2,p]-sphvertex_list[2,*]),pind)
               if dist_ph le dph then begin
                  print,''
                  print,dist_r,dist_th,dist_ph
                  print,ff,p
                  print,rind,tind,pind
                  print,''
               endif
            endif
         endif
      endfor
      
   endif
   
endfor


   stop






;  linearly interpolate to get locus of crossing
;      for j=0,ncr-1 do begin
;         if total(ixm eq ixpt[j]) eq 0 then begin ;  avoids double-counting
;            coeff=(rcut-lr[ixpt[j]])/(lr[ixpt[j]+1]-lr[ixpt[j]])
;            ptcrth=90-((1-coeff)*lth[ixpt[j]]+coeff*lth[ixpt[j]+1])*!radeg
;            ptcrph=(((1-coeff)*lph[ixpt[j]]+coeff*lph[ixpt[j]+1])*!radeg+360) $
;                   mod 360
;            data=[[data],[i,ptcrth,ptcrph]]      
;            interpindex=[interpindex,ixpt[j]+coeff]
;         endif
;      endfor

end
