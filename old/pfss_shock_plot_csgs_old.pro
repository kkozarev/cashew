pro test_pfss_shock_plot_csgs
;Testing the CSGS model plotting procedure
event=load_events_info(label='test')
pfss_shock_plot_csgs,event,png=png
end


pro pfss_shock_plot_csgs,event,png=png
;PURPOSE:
;Plot the time-dependent Coronal Shock Geometrical Surface model,
;overlaying AIA images, PFSS model, and CSGS model with crossing points.
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
  wav='193'
  evnum=event.label
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  eventname='AIA_'+date+'_'+evnum+'_'+wav
  savepath=event.savepath
  datapath=savepath
  pfsspath=event.pfsspath
  
  pfssfile=event.pfsspath+'pfss_results_'+date+'_'+label+'_1.05Rs_dens_0.5.sav'
  wavefile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed.sav'
  aiafile=event.savepath+'normalized_'+eventname+'_subdata.sav'
  
  ;Find a file to load with the latest results of applying the CSGS model
  csgsfile=find_latest_file(event.pfsspath+'model_shock_vsh_*') 
    if csgsfile eq '' then begin
     print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif

;--------------------------------------------------------------
;LOAD THE DATA
  print,''
  print,'Loading data...'
  ;Load the CSGS model results
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
  
  ;Load the AIA observations
  print,'Loading AIA File '+aiafile
  restore,aiafile

  ;Load the PFSS model results
  print,'Loading PFSS File '+pfssfile
  restore,pfssfile
  
  ;Load the Annulusplot analysis results
  print, 'Loading shock info file '+wavefile
  restore,wavefile
  

;--------------------------------------------------------------


;--------------------------------------------------------------
;Constants and definitions
  sp=rad_data.xfitrange[0]
  ep=rad_data.xfitrange[1]
  time=(rad_data.time[sp:ep]- rad_data.time[sp])*3600.
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
  xcenter=suncenter[0]
  ycenter=suncenter[1]
  zcenter=suncenter[2]
  sunrad=subindex[0].r_sun+10;For some reason the R_SUN variable is 10 px short...
  

;THIS IS THE STEPS LOOP!
  for sstep=0,nsteps-1 do begin             
     print,'Step #'+string(sstep)
                                ;calculate the shock radius
                                ;shockrad=minshockrad+((maxshockrad-minshockrad)*sstep)/(nsteps-1)
     
     shockrad=radius[sstep]     ;Get this from the measurements
     ;print,shockrad
                                ;vshock=radiusmoments[sstep,1]*1000.0 ;Shock speed in m/s
     
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
     shockrot=[xrot_shock,yrot_shock,zrot_shock]
     
     
;+==============================================================================
;1. Plot the field lines on disk center.
     if sstep eq 0 then begin
;Convert the spherical to x,y,z coordinates.
;Switch y and z axes to make the coordinate system right-handed.
        l=subindex[sstep].crln_obs*!PI/180.0
        b=subindex[sstep].crlt_obs*!PI/180.0
        pfss_sphtocart,ptr,ptth,ptph,l,b,pfss_px,pfss_pz,pfss_py
        nlines=n_elements(pfss_px[0,*])*1.0D
        maxnpts=n_elements(pfss_px[*,0])  
        
;Convert the pfss coordinates from Rs to pixels
                                ;stop
        
;Apply the rotations and translations and plot
        pfss_cartpos=fltarr(nlines,3,maxnpts)
        for ff=0.0D,nlines-1 do begin
           
           npt=nstep[ff]        ;the number of points in this particular line.
           pos = transpose([[reform(pfss_px[0:npt-1,ff])],$
                            [reform(pfss_py[0:npt-1,ff])],$
                            [reform(pfss_pz[0:npt-1,ff])]])
           ;stop
           pos = transform_volume(pos,rotation=[xrot_pfss,yrot_pfss,zrot_pfss],$
                                  scale=[sunrad,sunrad,sunrad])
           pos = transform_volume(pos,translate=[xcenter,ycenter,zcenter])
                                ; stop
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
;-==============================================================================
     
     
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
        npt = nstep[ff]         ;the number of points in this particular line.
        pos = reform(pfss_cartpos[ff,*,0:npt-1])
        plots,pos,color=250,/device,psym=3 ;,psym=sym(1),symsize=0.25
     endfor
     
     
;+==============================================================================
;3. Calculate and plot the spherical surface:
        
;Create the shock surface
; Calculate the latitudinal extension of the shock surface below theta=0.
;print,-asin(shockrad/(2*sunrad))
        
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
     
;     nverts=n_elements(vertex_list[0,*])
;     index=subindex[sstep]
     
     loadct,9,/silent
     plots,sc[0],sc[1],psym=2,color=0,symsize=2,/device
     plots,vertex_list,color=0,thick=0.05,/device
     plots,vertex_list,color=180,thick=0.1,symsize=0.6,psym=sym(1),/device
;-==============================================================================
        
;DEBUG

;+==============================================================================
;4. Determine the points where the field lines cross the shock
;surface of thickness shockthick
shockthick=3.0
        dr2=(reform(pfss_cartpos[*,0,*])-sc[0])^2 + $
            (reform(pfss_cartpos[*,1,*])-sc[1])^2 + $
            (reform(pfss_cartpos[*,2,*])-sc[2])^2
        ptind=where(dr2 le (shockrad+shockthick)^2 and dr2 ge shockrad^2)
        
        if ptind[0] gt -1 then begin
           ;pind[0,*] are the crossing line indices
           ;pind[1,*] are the crossing point indices
           pind=array_indices(reform(pfss_cartpos[*,0,*]),ptind)
           
           ;Record the uniquely crossing lines.
           srt=sort(pind[0,*])
           pind=pind[*,srt] ;indices ordered by crossing line index
           tmp=reform(pind[0,*])
           unid=uniq(tmp)
           unique_lines=pind[*,unid] ;The uniquely crossing line indices
           pind2=unique_lines
           ncrosses=n_elements(pind2[0,*])*1.0D
        endif else begin
           print,'No field/shock crossings. Continuing...'
           continue
        endelse



;DEBUG





;+==============================================================================
;Plot the field lines that pass through the shock surface
     ncrosses=allcrosses[sstep]
     cross_points=reform(allcrossPoints[sstep,*,0:ncrosses-1])
     pind=reform(allcrossLineIndices[sstep,0:ncrosses-1])
     colors=abs(randomn(10L,ncrosses))*255.
     loadct,13,/silent
     for ff=0,ncrosses-1 do begin
        npt=nstep[pind[ff]]
        plots,reform(pfss_cartpos[pind[ff],*,0:npt-1]),$
              color=colors[ff],/device,psym=sym(1),symsize=0.3
     endfor
;plot the points of crossing in red.
     plots,cross_points,color=240,psym=sym(1),symsize=1.4,/device
;-==============================================================================
     
     if keyword_set(png) then begin
        tvlct,rr,gg,bb,/get
        image=tvrd(true=1)
        stp=strtrim(string(sstep),2)
        if stp lt 100 then stp='0'+stp
        if stp lt 10 then stp='0'+stp
        write_png,pfsspath+'aia_pfss_shock_'+event.date+'_'+event.label+'_'+stp+'.png',image,rr,gg,bb
     endif
     
  endfor ;END TIMESTEP LOOP
     
end
