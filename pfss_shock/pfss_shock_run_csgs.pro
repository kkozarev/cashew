function bfield_pfss,ptc,sph_data
;Use interpolation on the PFSS model
  r2d=180./!PI
  d2r=!PI/180.
  irc=get_interpolation_index(*sph_data.rix,ptc[0])
  ithc=get_interpolation_index(*sph_data.lat,90-ptc[1]*r2d)
  iphc=get_interpolation_index(*sph_data.lon,(ptc[2]*r2d+360) mod 360)
  brc=interpolate(*sph_data.br,iphc,ithc,irc)
  bthc=interpolate(*sph_data.bth,iphc,ithc,irc)/ptc[0]
  bphc=interpolate(*sph_data.bph,iphc,ithc,irc)/(ptc[0]*sin(ptc[1]))
  bmag=sqrt(brc^2+bthc^2+bphc^2)
  return,bmag
end


function fieldline_isopen,ptc,sph_data
  ;Find out if the field line is open or not.
  r2d=180./!PI
  d2r=!PI/180.
  isopen=0
  irc=get_interpolation_index(*sph_data.rix,ptc[0])
  ithc=get_interpolation_index(*sph_data.lat,90-ptc[1]*r2d)
  iphc=get_interpolation_index(*sph_data.lon,(ptc[2]*r2d+360) mod 360)
  brc=interpolate(*sph_data.br,iphc,ithc,irc)
  if brc gt 0 then isopen=1 else isopen=-1
  return,isopen
end


;+--------------------------------------------------------------------
pro test_pfss_shock_run_csgs
; A small procedure to run several instances of the coronal shock
; model.
  
  event=load_events_info(label='110511_01')
  pfss_shock_run_csgs,event;,/plot,/png

end
;---------------------------------------------------------------------



;+--------------------------------------------------------------------
pro pfss_shock_run_csgs,event,plot=plot,png=png
;This procedure runs the pfss/shock model for estimating shock
;orientation to magnetic fields, and 
;PURPOSE:
;Run time-dependent Coronal Shock Geometric Surface model, find
;crossing points and local B-field and thetaBN.
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;       event - an event structure
;
;KEYWORDS:
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;transform_volume, sym
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/21/2014
;
  resolve_routine,'sym',/either,/compile_full_file

;LOAD DATA
  print,''
  print,'Loading data...'
  
  wav='193'
  evnum=event.label
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  eventname='AIA_'+date+'_'+evnum+'_'+wav

;Figure out the name of the local machine.
  pcname=hostname()
  
  savepath=event.savepath
  datapath=savepath
  pfsspath=event.pfsspath
  
  pfssfile=pfsspath+'pfss_results_'+date+'_'+label+'_1.05Rs_dens_1.0.sav'
  ;pfssfile=pfsspath+'pfss_results_'+date+'_'+label+'_1.05Rs_dens_4.0.sav'
  aiafile=datapath+'normalized_'+eventname+'_subdata.sav'
  shockfile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed.sav'
  
  print,'Loading AIA File '+aiafile
  restore,aiafile
  subdata=0
  
  ;Load the measured shock wave radius.
  ;This was created with measure_wave_sphere.pro
  ;print, 'Loading shock info file '+datapath+eventname+'_shocklocations.sav'
  ;restore,datapath+eventname+'_shocklocations.sav'
  print, 'Loading shock info file '+shockfile
  restore,shockfile

  ;Load the PFSS model results
  print,'Loading PFSS File '+pfssfile
  restore,pfssfile
;--------------------------------------------------------------



;--------------------------------------------------------------
;Constants and definitions
  loadct,8,/silent
  sp=rad_data.xfitrange[0]
  ep=rad_data.xfitrange[1]
  time=(rad_data.time[sp:ep]-rad_data.time[sp])*3600.
  nsteps=n_elements(time)                                
  RSUN=subindex[0].rsun_ref/1000. ;Solar radius in km.  
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  fit=reform(rad_data.fitparams[0,*].front)
  radiusfitlines=(fit[0]+fit[1]*time+0.5*fit[2]*time^2)/RSUN
  radiusfitlines-=1.
  radiusfitlines*=RSUN*event.geomcorfactor
  radius=radiusfitlines/kmpx
    
  lon=event.arlon
  lat=event.arlat
  
  winsize=1024
  xcenter=subindex[0].x0_mp
  ycenter=subindex[0].y0_mp
  zcenter=0.0
  suncenter=[xcenter,ycenter,zcenter]
  
  sunrad=subindex[0].r_sun+10;For some reason the R_SUN variable is 10 px short...
  minshockrad=radius[0]/kmpx
  maxshockrad=radius[nsteps-1]/kmpx
  subindex=ind_arr[sp:ep]

  rsun_m=subindex[0].rsun_ref ;Solar radius in m.
  minAU=1.49598e11 ;m in AU
  mpix=rsun_m/sunrad ;conversion between pixels and m.
  
  shockthick=3.0                ;shock thickness, in pixels
    
;Calculate the number of steps and their size.
  dt= time[1]-time[0]           ;The cadence (maxshockrad-minshockrad)*mpix/(nsteps*1.0)/vshock ;timestep in seconds
  
;Variables for the crossing points information
  allcrossPoints=fltarr(nsteps,3,1000000)
  allcrossAngles=fltarr(nsteps,1000000)
  allcrossBmag=fltarr(nsteps,1000000)
  allcrossLineIndices=intarr(nsteps,1000000)
  allcrosses=fltarr(nsteps)
;--------------------------------------------------------------
  
  

;--------------------------------------------------------------

;THIS IS THE STEPS LOOP!
     for sstep=0,nsteps-1 do begin             
        print,'Step #'+string(sstep)

        shockrad=radius[sstep] ;Get this from the measurements
        
;Rotation angles for the entire plot
        xrot_gen=(sstep*0.0)/nsteps
        yrot_gen=(sstep*0.0)/nsteps
        zrot_gen=(sstep*0.0)/nsteps
        genrot=[xrot_gen,yrot_gen,zrot_gen]
        
;Rotation angles for the PFSS points
        xrot_pfss=0+xrot_gen
        yrot_pfss=0+yrot_gen
        zrot_pfss=0+zrot_gen
        pfssrot=[xrot_pfss,yrot_pfss,zrot_pfss]
        
;Rotation angles for the shock surface points
        xrot_shock=-lat+xrot_gen
        yrot_shock=lon+yrot_gen
        zrot_shock=0+zrot_gen
        csgsrot=[xrot_shock,yrot_shock,zrot_shock]
        
;Save the rotation angles
        rotationAngles={genrot:genrot,pfssrot:pfssrot,csgsrot:csgsrot}
        
;+==============================================================================
;1. Plot the field lines on disk center.
        if sstep eq 0 then begin
;Convert the spherical to x,y,z coordinates.
;Switch y and z axes to make the coordinate system right-handed.
           carrlon=subindex[sstep].crln_obs*!PI/180.0 ;l
           carrlat=subindex[sstep].crlt_obs*!PI/180.0  ;b
           
           pfss_sphtocart,ptr,ptth,ptph,carrlon,carrlat,pfss_px,pfss_pz,pfss_py
           
           nlines=n_elements(pfss_px[0,*])*1.0D
           maxnpts=n_elements(pfss_px[*,0])
           
;Apply the rotations and translations and plot
           pfss_cartpos=fltarr(nlines,3,maxnpts)
           linecols=fix(abs(randomu(10L,nlines,/uniform))*255.)
           for ff=0.0D,nlines-1 do begin
              npt=nstep[ff]     ;the number of points on this particular line.
              
              ;SAVE THE FIELD LINE INFORMATION TO A STRUCTURE ARRAY
              if ff eq 0 then begin
                 pfssLine={npts:0L,ptr:dblarr(max(nstep)),ptth:dblarr(max(nstep)),ptph:dblarr(max(nstep)),$
                           open:0,linid:0L,color:0.}
                 pfssLines=replicate(pfssLine,nlines)
              endif
              pfssLines[ff].npts=npt
              pfssLines[ff].ptr=ptr[0:npt-1,ff]
              pfssLines[ff].ptth=ptth[0:npt-1,ff]
              pfssLines[ff].ptph=ptph[0:npt-1,ff]
              pfssLines[ff].open=0
              pfssLines[ff].linid=ff
              pfssLines[ff].color=linecols[ff]
              
              ;Transform to the current view
              pos = transpose([[reform(pfss_px[0:npt-1,ff])],$
                               [reform(pfss_py[0:npt-1,ff])],$
                               [reform(pfss_pz[0:npt-1,ff])]])
              pos = transform_volume(pos,rotation=[xrot_pfss,yrot_pfss,zrot_pfss],$
                                     scale=[sunrad,sunrad,sunrad])
              pos = transform_volume(pos,translate=[xcenter,ycenter,zcenter])
              pfss_cartpos[ff,*,0:npt-1]=pos
  
           endfor
           
           ;Free some memory
           linecols=0
           pfss_px=0
           pfss_pz=0
           pfss_py=0
           pos=0
           ptr=0
           ptth=0
           ptph=0
           
        endif
;-==============================================================================
        
        
;PLOT THE AIA IMAGE!
           if keyword_set(plot) or keyword_set(png) then begin
              aia_lct,rr,gg,bb,wavelnth=subindex[sstep].wavelnth,/load
              
              if sstep eq 0 then begin
                 wdef,0,1024
                                ;tvlct,rrr,ggg,bbb,/get
              endif
              tv,bytscl(sqrt(subdata[*,*,sp+sstep]),min=1,max=50)
              
;OVERPLOT THE LIMB LOCATION
              circ=aia_circle(xcenter,ycenter,sunrad,/plot)

              
;PLOT THE PFSS INFORMATION
              for ff = 0.0D,nlines-1,4 do begin
                 npt = nstep[ff] ;the number of points in this particular line.
                 pos = reform(pfss_cartpos[ff,*,0:npt-1])
                 plots,pos,color=250,/device,psym=3 ;,psym=sym(1),symsize=0.25
              endfor
           endif
        
;+==============================================================================
;3. Calculate and plot the spherical surface:
        
;Create the shock surface
; Calculate the latitudinal extension of the shock surface below theta=0.
;It is equal to -asin(shockrad/(2*sunrad)) or similar
        
        MESH_OBJ, $
           4, $
           Vertex_List, Polygon_List, $ ;lists of polygons and vertices
           Replicate(shockrad, 100, 100)  , $
           p3=-asin(shockrad/(2*sunrad))
        
;apply rotation and translation to the surface
        Vertex_List = transform_volume(vertex_list,translate=[xcenter,ycenter,zcenter+sunrad])
        vert_transmat=!P.T
        Vertex_List = transform_volume(vertex_list,rotation=[xrot_shock,yrot_shock,zrot_shock],$
                                       centre_rotation=[xcenter,ycenter,zcenter])
        vert_rotmat=!P.T
        
        ;Initial coordinates of the center of the shock surface.
        sc = transform_volume([0,0,0],translate=[xcenter,ycenter,zcenter+sunrad])
        sc = transform_volume(sc,rotation=[xrot_shock,yrot_shock,zrot_shock],$
                              centre_rotation=[xcenter,ycenter,zcenter])
        
        nverts=n_elements(vertex_list[0,*])
        index=subindex[sstep]
        if keyword_set(plot) or keyword_set(png) then begin
                                ;aia_plot_hemisphere,index,shockrad,vertex_list=vertex_list
           loadct,9,/silent
           plots,sc[0],sc[1],psym=2,color=0,symsize=2,/device
           plots,vertex_list,color=0,thick=0.05,/device
           plots,vertex_list,color=180,thick=0.1,symsize=0.6,psym=sym(1),/device
        endif
;-==============================================================================

        
;+==============================================================================
;4. Determine the points where the field lines cross the shock
;surface of thickness shockthick
        dr2=(reform(pfss_cartpos[*,0,*])-sc[0])^2 + $
            (reform(pfss_cartpos[*,1,*])-sc[1])^2 + $
            (reform(pfss_cartpos[*,2,*])-sc[2])^2
        ptind=where(dr2 le (shockrad+shockthick)^2 and dr2 ge shockrad^2)
        
        if ptind[0] gt -1 then begin
           ;pind[0,*] are the crossing line indices
           ;pind[1,*] are the crossing point indices
           pind=array_indices(reform(pfss_cartpos[*,0,*]),ptind)
           
           ;Leave only the unique crossing lines.
           srt=sort(pind[0,*])
           pind=pind[*,srt]
           tmp=reform(pind[0,*])
           unique_lines=pind[*,uniq(tmp)]
           pind=unique_lines
           ncrosses=n_elements(pind[0,*])*1.0D
        endif else begin
           print,'No field/shock crossings. Continuing...'
           continue
        endelse
        
        cross_points=fltarr(3,ncrosses)
        cross_bmag=fltarr(ncrosses)
        for i=0,ncrosses-1 do begin
           cross_points[*,i]=reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
           sphpt=cart2sph([cross_points[0,i]-xcenter,cross_points[1,i]-ycenter,cross_points[2,i]-zcenter]/sunrad)
           cross_bmag[i]=bfield_pfss(sphpt,sph_data)
        endfor
        
        allcrossPoints[sstep,*,0:ncrosses-1]=cross_points*1.0D
        allcrossBmag[sstep,0:ncrosses-1]=cross_bmag*1.0D
        allcrossLineIndices[sstep,0:ncrosses-1]=reform(pind[0,*])
        allcrosses[sstep]=ncrosses
;Plot the field lines that pass through the shock surface
        if keyword_set(plot) or keyword_set(png) then begin
           colors=abs(randomn(10L,ncrosses))*255.
           loadct,13,/silent
           for ff=0,ncrosses-1 do begin
              npt=nstep[pind[0,ff]]
              plots,reform(pfss_cartpos[pind[0,ff],*,0:npt-1]),$
                    color=colors[ff],/device,psym=sym(1),symsize=0.3
           endfor
;plot the points of crossing in red.
           plots,cross_points,color=240,psym=sym(1),symsize=1.4,/device
        endif
;-==============================================================================

       
;+==============================================================================
;6. Find the crossing angles. Find the local field direction and the
;normal to the shock first, then the angle ThetaBN at every point.
        normals=fltarr(3,ncrosses)
        local_field=fltarr(3,ncrosses)
        mag=fltarr(ncrosses)

;find the shock normals
        for i=0,ncrosses-1 do mag[i]=sqrt((cross_points[0,i] - sc[0])^2 + $
                                          (cross_points[1,i] - sc[1])^2 + $
                                          (cross_points[2,i] - sc[2])^2)
        for i=0,2 do begin
           normals[i,*] = (cross_points[i,*] - sc[i])/mag
        endfor

;find the local field direction
        for i=0,ncrosses-1 do begin
           ;Take the crossing point, and subtract
           ;its position from that of the one two points farther on the line to get the direction
           p1= reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
           p2= reform(pfss_cartpos[pind[0,i],*,pind[1,i]+2])
           mag=sqrt((p2[0]-p1[0])^2+(p2[1]-p1[1])^2+(p2[2]-p1[2])^2)
           local_field[*,i] = (p2-p1)/mag
           ;print,p1
           ;print,p2
           ;print,local_field[*,i]
        endfor
        ;compute the dot products
        dotp=normals[0,*]*local_field[0,*] + $
             normals[1,*]*local_field[1,*] + $
             normals[2,*]*local_field[2,*]
        dotp=reform(dotp)
        ind=where(dotp eq 0.0)
        if ind[0] gt -1 then dotp[ind]=1.0e-33
        
        ;The angles are acos((a dot b)/(|a|*|b|))
        ;prod=total(abs(normals),1) * total(abs(local_field),1)
        ;thetabn=acos(dotp/prod) ;I've already divided by the magnitudes
        thetabn=acos(dotp)
        
        ind=where(thetabn gt !PI/2.0)
        if ind[0] gt -1 then thetabn[ind]=!PI-thetabn[ind]
        th=thetabn*180.0/!PI
        allcrossAngles[sstep,0:ncrosses-1]=th
;-==============================================================================
        
        
        ;Save the PFSS/CSGS/AIA image
        if keyword_set(png) then begin
           tvlct,rr,gg,bb,/get
           image=tvrd(true=1)
           stp=strtrim(string(sstep),2)
           if stp lt 100 then stp='0'+stp
           if stp lt 10 then stp='0'+stp
           write_png,pfsspath+'aia_pfss_shock_'+event.date+'_'+event.label+'_'+stp+'.png',image,rr,gg,bb
        endif
        
     endfor  ;END TIMESTEP LOOP
     
     
     
;CREATE A STRUCTURE TO HOLD THE CROSS-POINT RESULTS FOR EASY PROCESSING LATER
     nmaxcrosses=max(allcrosses)
     crossPoint={px:0.0D,py:0.0D,pz:0.0D,thbn:0.0D,linid:0L,bmag:0.0D}
     crossPoints=replicate(crossPoint,nsteps,nmaxcrosses)
     
        ;This array will hold the closest and farthest point coordinates for every interacting
        ;field line
           endPointCoords=replicate({ptr:0.0,ptth:0.0,ptph:0.0,linid:0L},nsteps,nmaxcrosses,2)
           lineSpread=dblarr(nsteps,max(allcrosses),3,2)
           maxLonExtent=dblarr(nsteps)
           
     for sstep=0,nsteps-1 do begin
        ncrosses=allcrosses[sstep]
        for cross=0,ncrosses-1 do begin
           crossPoints[sstep,cross].px=allcrosspoints[sstep,0,cross]
           crossPoints[sstep,cross].py=allcrosspoints[sstep,1,cross]
           crossPoints[sstep,cross].pz=allcrosspoints[sstep,2,cross]
           crossPoints[sstep,cross].linid=allcrossLineIndices[sstep,cross]
           crossPoints[sstep,cross].thbn=allcrossAngles[sstep,cross]
           crossPoints[sstep,cross].bmag=allcrossBmag[sstep,cross]

           
;Get the spherical coordinates for the starting and ending point for
;every interacting field line.
        linid=allcrossLineIndices[sstep,cross]
        endPointCoords[sstep,cross,0].ptr=pfssLines[linid].ptr[0]
        endPointCoords[sstep,cross,0].ptth=pfssLines[linid].ptth[0]
        endPointCoords[sstep,cross,0].ptph=pfssLines[linid].ptph[0]
        endPointCoords[sstep,cross,0].linid=pfssLines[linid].linid
        endPointCoords[sstep,cross,1].ptr=pfssLines[linid].ptr[pfssLines[linid].npts-1]
        endPointCoords[sstep,cross,1].ptth=pfssLines[linid].ptth[pfssLines[linid].npts-1]
        endPointCoords[sstep,cross,1].ptph=pfssLines[linid].ptph[pfssLines[linid].npts-1]
        endPointCoords[sstep,cross,1].linid=pfssLines[linid].linid
;Get the largest and smallest coordinates for each field line, for
;each time step.
        lineSpread[sstep,cross,0,*]=minmax(pfssLines[linid].ptr[0:pfssLines[linid].npts-1])
        lineSpread[sstep,cross,1,*]=minmax(pfssLines[linid].ptth[0:pfssLines[linid].npts-1])
        lineSpread[sstep,cross,2,*]=minmax(pfssLines[linid].ptph[0:pfssLines[linid].npts-1])
     endfor
     
     ;Find the largest longitudinal extent for this particular time step.
        tmp=abs(reform(lineSpread[sstep,0:ncrosses-1,2,*])-lon*!PI/180.)
        maxLonExtent[sstep]=max(tmp)*180./!PI
     endfor
     


;Save the results to a file
     fname='csgs_results_'+event.date+'_'+event.label+'.sav'
     print,'Saving file '+pfsspath+fname
     save,filename=pfsspath+fname,pfssLines,endPointCoords,lineSpread,maxLonExtent,$
          allcrosses,dt,subindex,radius,time,rotationAngles,crossPoints,carrlon,carrlat,$
          vertex_list,vert_rotmat,vert_transmat,suncenter,nsteps,sc,radiusfitlines,ind_arr
;-==============================================================================     
end ; END AIA_MODEL_SHOCK_PFSS
