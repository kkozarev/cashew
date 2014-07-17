pro test_pfss_shock_plot_csgs
;Testing the CSGS model plotting procedure
event=load_events_info(label='110511_01')
pfss_shock_plot_csgs,event,/png
end


pro pfss_shock_plot_csgs,event,png=png,hires=hires,lores=lores
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
  
  wavefile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed.sav'
  aiafile=event.savepath+'normalized_'+eventname+'_subdata.sav'
  
  ;Find a file to load with the latest results of applying the CSGS model
  csgsfile=find_latest_file(event.pfsspath+'csgs_results_*')
  if keyword_set(hires) then infile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'hires.sav')
  if keyword_set(hires) then infile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'lores.sav')
  if infile[0] eq '' then begin
     print,'The file to load is not properly set or does not exist. Quitting.'
     return
  endif  
if csgsfile eq '' then begin
   print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif

;+==============================================================================
;LOAD THE DATA
  print,''
  print,'Loading data...'
  ;Load the CSGS model results
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
  
  ;Load the AIA observations
  print,'Loading AIA File '+aiafile
  restore,aiafile
  
  ;Load the Annulusplot analysis results
  print, 'Loading shock info file '+wavefile
  restore,wavefile

;-==============================================================================


;+==============================================================================
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
  set_plot,'z'
;-==============================================================================


;--------------------------------------------------------------
;;THIS IS THE TIMESTEPS LOOP!
;--------------------------------------------------------------
  for sstep=0,nsteps-1 do begin             
     print,'Step #'+string(sstep)
     shockrad=radius[sstep]     ;Get this from the measurements


;+==============================================================================
;PLOT THE AIA IMAGE
     aia_lct,rr,gg,bb,wavelnth=subindex[sstep].wavelnth,/load     
     ;if sstep eq 0 then wdef,0,winsize
     if sstep eq 0 then $
        ;device,set_resolution=[winsize,winsize],SET_PIXEL_DEPTH=24, DECOMPOSED=0
        device,set_resolution=[event.aiafov[0],event.aiafov[1]],SET_PIXEL_DEPTH=24,DECOMPOSED=0
     tv,bytscl(sqrt(subdata[*,*,sp+sstep]),min=1,max=50)
     
;Overplot the limb location
     circ=aia_circle(xcenter,ycenter,sunrad,/plot)
;-============================================================================== 



;+==============================================================================
;1. Plot the field lines on disk center.
     if sstep eq 0 then begin
        nlines=n_elements(pfssLines)
        maxnpts=n_elements(pfssLines[0].ptr)  
        stride=(1.*nlines)/1000. ;assume that we want to see about 1000. field lines, for now.
;Apply the rotations and translations and plot
        pfss_cartpos=fltarr(nlines,3,maxnpts)
        for ff=0.0D,nlines-1,stride do begin
           ;the number of points in this particular line.
           npt=pfssLines[ff].npts      
           ;px=pfssLines[ff].px[0:npt-1]
           ;py=pfssLines[ff].py[0:npt-1]
           ;pz=pfssLines[ff].pz[0:npt-1]
           ;pos = transpose([[reform(px)],[reform(py)],[reform(pz)]])
           ;pos = transform_volume(pos,rotation=rotationAngles.pfssrot,$
           ;                       scale=[sunrad,sunrad,sunrad])
           ;pos = transform_volume(pos,translate=suncenter)
           pfss_sphtocart,pfssLines[ff].ptr,pfssLines[ff].ptth,pfssLines[ff].ptph,$
                          carrlon,carrlat,px,pz,py
           pos = transpose([[reform(px[0:npt-1])],[reform(py[0:npt-1])],[reform(pz[0:npt-1])]])
           ;TRANSFORM THE POINTS APPROPRIATELY
           pos = transform_volume(pos,scale=[sunrad,sunrad,sunrad])
           pos = transform_volume(pos,translate=suncenter)
           pfss_cartpos[ff,*,0:npt-1]=pos        
           ;Plot the field line 
           plots,pos,color=250,/device,psym=3
        endfor
        pos=0
     endif

     for ff=0.0D,nlines-1,stride do begin
        npt=pfssLines[ff].npts
        ;Plot the field lines
        if pfss_cartpos[ff,2,0] gt 0.0 and pfss_cartpos[ff,2,npt-1] gt 0.0 then $
           plots,reform(pfss_cartpos[ff,*,0:npt-1]),/device,color=250
     endfor

;-==============================================================================
     


;+==============================================================================
;3. CALCULATE AND PLOT THE CSGS MODEL
       
;Create the shock surface
     MESH_OBJ, $
        4, $
        Vertex_List, Polygon_List, $ ;lists of polygons and vertices
        Replicate(shockrad, 100, 100)  , $
        p3=-asin(shockrad/(2*sunrad))
     
;apply rotation and translation to the surface
     Vertex_List = transform_volume(vertex_list,translate=[suncenter[0],suncenter[1],suncenter[2]+sunrad])
     vert_transmat=!P.T
     Vertex_List = transform_volume(vertex_list,rotation=rotationAngles.csgsrot,$
                                    centre_rotation=suncenter)
     vert_rotmat=!P.T
     loadct,9,/silent
     plots,sc[0],sc[1],psym=2,color=0,symsize=2,/device
     plots,vertex_list,color=0,thick=0.05,/device
     plots,vertex_list,color=180,thick=0.1,symsize=0.6,psym=sym(1),/device
;-==============================================================================


;+==============================================================================
;Plot the field lines that pass through the shock surface
     ncrosses=allcrosses[sstep]
     cpsx=crossPoints[sstep,0:ncrosses-1].px
     cpsy=crossPoints[sstep,0:ncrosses-1].py
     cpsz=crossPoints[sstep,0:ncrosses-1].pz 
     pind=reform(crossPoints[sstep,0:ncrosses-1].linid)
     ;colors=abs(randomn(10L,ncrosses))*255.
     loadct,13,/silent
     
     for ff=0,ncrosses-1 do begin
        lind=pind[ff]
        npt=pfssLines[lind].npts
;        if pfss_cartpos[ff,2,0] gt 0.0 and pfss_cartpos[ff,2,npt-1] gt 0.0 then $
        plots,reform(pfss_cartpos[lind,*,0:npt-1]),$
              color=pfssLines[lind].color,/device,psym=sym(1),symsize=0.5
     endfor
;plot the points of crossing in red.
     plots,[cpsx,cpsy,cpsz],color=240,psym=sym(1),symsize=1.4,/device
;-==============================================================================
     
     if keyword_set(png) then begin
        tvlct,rr,gg,bb,/get
        image=tvrd(true=1)
        stp=strtrim(string(sstep),2)
        if stp lt 100 then stp='0'+stp
        if stp lt 10 then stp='0'+stp
        write_png,pfsspath+'aia_pfss_shock_'+event.date+'_'+event.label+'_'+stp+'.png',image,rr,gg,bb
     endif
     ;stop
  endfor ;END TIMESTEP LOOP
  set_plot,'x'
  loadct,0,/silent
end
