pro test_pfss_shock_plot_angular_influence
;Testing the CSGS angular influence plotting procedure
event=load_events_info(label='paper')
pfss_shock_plot_angular_influence,event,/lores,/newtimes
;pfss_shock_plot_angular_influence,event,/hires,/newtimes,/topview
end

pro pfss_shock_plot_angular_influence,event,topview=topview,hires=hires,lores=lores,pfssLines=pfssLines,newtimes=newtimes,force=force
;PURPOSE:
;Visualize the time-dependent CSGS model with interacting field lines.
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;       event - an event structure
; 
;KEYWORDS:
;       topview - plot the PFSS+CSGS as viewed from above the solar
;                 north pole.
;OUTPUTS:
;
; 
;DEPENDENCIES:
;transform_volume, sym
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/22/2014
;04/02/2014, Kamen Kozarev - added the top view functionality.
;04/06/2015, Kamen Kozarev - added the /force keyword.

  set_plot,'x'
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
  resolution='lores'
  if keyword_set(hires) then resolution='hires'
  wavefile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed_radial.sav'
  
  ;Find a file to load with the latest results of applying the CSGS model
  ;csgsfile=find_latest_file(event.pfsspath+'csgs_results_*')
  csgsfile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_lores.sav')
  if keyword_set(hires) then csgsfile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_hires.sav')
  if csgsfile eq '' then begin
     print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif

;+==============================================================================
;LOAD THE DATA
  print,''
  print,'Loading data...'
  
  ;Load the Annulusplot analysis results
  print, 'Loading shock info file '+wavefile
  restore,wavefile
  
  ;Load the CSGS model results
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
  
  ;Restore just the subindex from the AIA data
  aiafile=file_search(event.savepath+'normalized_'+eventname+'_subdata.sav')
  print,'Loading AIA File '+aiafile
  if aiafile[0] ne '' then begin
     restore,aiafile[0]
     subdata=0
     aiatime=anytim(subindex.date_obs)
     aiatime=aiatime-aiatime[0]
  endif else begin
     print,'No AIA data present. Quitting...'
     return
  endelse
;-==============================================================================



;+==============================================================================
;Constants and definitions
  lon=event.arlon
  lat=event.arlat
  
  winsize=1024
  xcenter=winsize/2.0
  ycenter=winsize/2.0
  zcenter=0
  suncenter=[xcenter,ycenter,zcenter]
  sunrad=winsize/(2.*2.62) ;Make sure the full 2.54Rsun of the PFSS lines are visible
  maxnumlines=5000         ;maximum number of PFSS field lines to plot for the context coronal image
  RSUN=subindex[0].rsun_ref/1000. ;Solar radius in km.  
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  sp=rad_data.xfitrange[0]
  ep=rad_data.xfitrange[1]
  time=(rad_data.time[sp:ep]- rad_data.time[sp])
  plot_times=ind_arr[sp:ep].date_obs
  
if keyword_set(newtimes) then begin
     newtime=aiatime
     pfss_shock_generate_csgs_radii,ind_arr,rad_data,radiusfitlines,newtime=newtime,tindrange=tindrange
     time=newtime
     sp=tindrange[0]
     ep=tindrange[1]
     plot_times=subindex[sp:ep].date_obs
  endif else begin
     pfss_shock_generate_csgs_radii,ind_arr,rad_data,radiusfitlines
  endelse
  
  radius=radiusfitlines*sunrad*event.geomcorfactor
  nsteps=n_elements(time)

  

  set_plot,'z'
  
;-==============================================================================


;--------------------------------------------------------------
;;THIS IS THE TIMESTEPS LOOP!
;--------------------------------------------------------------
  for sstep=0,nsteps-1 do begin
    
     stp=strtrim(string(sstep),2)
     if stp lt 100 then stp='0'+stp
     if stp lt 10 then stp='0'+stp
     print,'Step #'+stp
     savefname=pfsspath+'aia_pfss_shock_angular_influence_'+event.date+'_'+event.label+'_'+resolution+'_'+stp+'.png'
     if keyword_set(topview) then savefname=pfsspath+'aia_pfss_shock_angular_influence_'+$
                                        event.date+'_'+event.label+'_topview_'+resolution+'_'+stp+'.png'
     if file_test(savefname) and not keyword_set(force) then begin
        print,'Files '+'aia_pfss_shock_angular_influence_'+event.date+'_'+event.label+'_'+resolution+'_*.png'
        print,'already exist. To overwrite, rerun with /force.'
        print,'----'
        break
     endif else begin
     shockrad=radius[sstep]     ;Get this from the measurements
     
     if sstep eq 0 then begin
        sunrot=[-lon,-lat]
;Rotation angles for the entire plot
        xrot_gen=(sstep*0.0)/nsteps-sunrot[1]
        if keyword_set(topview) then xrot_gen+=90.
        yrot_gen=(sstep*0.0)/nsteps+sunrot[0]
        zrot_gen=(sstep*0.0)/nsteps
        genrot=[xrot_gen,yrot_gen,zrot_gen]
        
;Rotation angles for the PFSS points
        xrot_pfss=0           ;+xrot_gen
        yrot_pfss=0             ;+yrot_gen
        zrot_pfss=0             ;+zrot_gen
        pfssrot=[xrot_pfss,yrot_pfss,zrot_pfss]
        
;Rotation angles for the shock surface points
        xrot_shock=-lat+xrot_gen
        yrot_shock=lon+yrot_gen   
        zrot_shock=0+zrot_gen
        csgsrot=[xrot_shock,yrot_shock,zrot_shock]
        
;Save the rotation angles
        rotationAngles={genrot:genrot,pfssrot:pfssrot,csgsrot:csgsrot}   
     endif
     
     loadct,0,/silent

     device,set_resolution=[winsize,winsize],SET_PIXEL_DEPTH=24, DECOMPOSED=0
     !p.background=100
     !P.color=255
     tv,findgen(100,100);,/nodata,xstyle=
     erase
     ;wdef,0,winsize

     
     ;Plot the limb location
     circ=aia_circle(xcenter,ycenter,sunrad,/plot,color=0)
     ;Print the time of this step.
     xyouts,0.5*winsize,0.965*winsize,plot_times[sstep],/device,charsize=2.4,charthick=3
     
;+==============================================================================
;1. Plot the field lines on disk center.
     if sstep eq 0 then begin
        ;Get the field line info from the PFSS model results
          if not keyword_set(pfssLines) then $
             if keyword_set(hires) then pfss_get_field_line_info,event,pfssLines=pfssLines,/hires $
             else pfss_get_field_line_info,event,pfssLines=pfssLines,/lores
        nlines=n_elements(pfssLines)
        ;maxnpts=n_elements(pfssLines[0].px)  
        maxnpts=n_elements(pfssLines[0].ptr)  
        
        nplotLines=1000.
        if (keyword_set(lores)) then begin
           stride=1
           plotLinesIndex=lonarr(nlines)
        endif else begin
           plotLinesIndex=lonarr(nplotLines+1)
           stride=fix((1.*nlines)/nPlotLines) ;assume that we want to see about 1000. field lines, for now.
           if stride eq 0 then stride=1
        endelse
        cc=0
        for ll=0.D,nlines-1,stride do begin
           if cc gt nplotLines then break
           plotLinesIndex[cc]=ll
           cc++
        endfor

        if maxnumlines gt nlines then maxnumlines=nlines
        
;Apply the rotations and translations and plot
        pfss_cartpos=fltarr(nlines,3,maxnpts)
        for ff=0.0D,nlines-1 do begin
           ;the number of points in this particular line.
           npt=pfssLines[ff].npts
           
           pfss_sphtocart,pfssLines[ff].ptr,pfssLines[ff].ptth,pfssLines[ff].ptph,$
                          carrlon-sunrot[0]*!PI/180.,carrlat-sunrot[1]*!PI/180.,px,pz,py
           pos = transpose([[reform(px[0:npt-1])],[reform(py[0:npt-1])],[reform(pz[0:npt-1])]])
           
           ;The order of the operations is rotate, scale, translate
           ;pos = transform_volume(pos,scale=[sunrad,sunrad,sunrad])          
           pos = transform_volume(pos,scale=[sunrad,sunrad,sunrad]) ;,centre_rotation=suncenter,rotation=pfssrot)
           pos = transform_volume(pos,translate=suncenter)
           if keyword_set(topview) then pos = transform_volume(pos,rotation=[90.,0,0],centre_rotation=suncenter)
           
           pfss_cartpos[ff,*,0:npt-1]=pos
           ;Plot the field line
           ;plots,pos,color=250,/device
        endfor
        pos=0
     endif
     ;stop
     
     ;ii=0L
     ;Find the cadence, at which to plot lines
     ;for ff=0L,nlines-1,stride do begin
     for ll=0.0D,nplotLines-1 do begin
        ff=plotLinesIndex[ll]
        npt=pfssLines[ff].npts
        ;Plot the field lines
        ;if pfss_cartpos[ff,2,0] gt 0.0 and
        ;pfss_cartpos[ff,2,npt-1] gt 0.0 then
        ;$
;if not keyword_set(topview) then
           plots,reform(pfss_cartpos[ff,*,0:npt-1]),/device,color=255,thick=1.2
           ;ii++
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
     vertex_List = $
        transform_volume(vertex_list,translate=[xcenter,ycenter,zcenter+sunrad])
     vert_transmat=!P.T
     vertex_List = transform_volume(vertex_list,rotation=csgsrot,$
                                    centre_rotation=suncenter)
     vert_rotmat=!P.T
     loadct,9,/silent
     plots,vertex_list,color=0,thick=0.05,/device
     plots,vertex_list,color=180,thick=0.05,symsize=0.1,psym=sym(1),/device

;-==============================================================================



;+==============================================================================
;Plot the field lines that pass through the shock surface
     ncrosses=allcrosses[sstep]
     cpsx=crossPoints[sstep,0:ncrosses-1].px
     cpsy=crossPoints[sstep,0:ncrosses-1].py
     cpsz=crossPoints[sstep,0:ncrosses-1].pz 
     pind=reform(crossPoints[sstep,0:ncrosses-1].linid)
     
     loadct,13,/silent
     
;     if sstep eq 0 then begin
;        nPlotLines=50.
;        crossPlotLinesIndex=lonarr(nplotLines+1)
;        stride=fix((1.*max(ncrosses))/nPlotLines) ;assume that we want to see about 1000. field lines, for now.
;        if (stride eq 0) or (keyword_set(lores)) then stride=1
;        cc=0
;        for ll=0.D,ncrosses-1,stride do begin
;           crossPlotLinesIndex[cc]=ll
;           cc++
;        endfor     
;     endif
stride=1 

     for ff=0.D,ncrosses-1,stride do begin
        lind=pind[ff]
        npt=pfssLines[lind].npts
        if pfssLines[lind].open eq 1 then color=110 else color=230
        if pfss_cartpos[ff,2,0] gt 0.0 and pfss_cartpos[ff,2,npt-1] gt 0.0 then begin
           loadct,0,/silent
           plots,reform(pfss_cartpos[lind,*,0:npt-1]),$
                 color=0,/device,psym=sym(1),symsize=0.8
           loadct,13,/silent
           plots,reform(pfss_cartpos[lind,*,0:npt-1]),$
                 color=color,/device,psym=sym(1),symsize=0.5
        endif
           
;plot the points of crossing in red.
    ; plots,[cpsx,cpsy,cpsz],color=240,psym=sym(1),symsize=1.4,/device
     endfor
     loadct,0,/silent
;-==============================================================================
     
     
;+==============================================================================
;Plot the connection point to Earth at the time. This is only done for
;the top view plots.
     if keyword_set(topview) then begin
        omegaSun=2*!PI/(27.27*86400) ;radians/sec
        npos=100
        usw=findgen(npos)+400.         ;Take a constant nominal 400 km/s solar wind speed above the source surface
;The formula is (Phi0-Phi)=omegaSun*(Re-R0)/usw
;R0 is the source surface radius.
        R0=2.54*rsun
;Re is at Earth.
        Re=210*rsun
;Assuming Phi is zero (using the heliographic coordinate system), have
;Phi0 = omegaSun*(Re-R0)/usw.
        ;phi0=fltarr(npos)
        ;for pp=0,npos-1 do phi0[pp]=omegaSun*(Re-R0)/usw[pp]
        phi0=omegaSun*(Re-R0)/usw
;Need to derotate it by the longitude of the AR
        phi0-=lon*!PI/180.
;Finally, subtract !PI/2. to get the zero angle to the positive-Y axis
        phi0-=!PI/2.
;Plot the position of the connection point to Earth
        plots,xcenter+2.57*sunrad*cos(phi0),ycenter+2.57*sunrad*sin(phi0),psym=sym(1),symsize=2,color=255,/device
        plots,xcenter+2.57*sunrad*cos(phi0),ycenter+2.57*sunrad*sin(phi0),psym=sym(1),symsize=1,color=0,/device
     endif
;-==============================================================================

     tvlct,rr,gg,bb,/get
     image=tvrd(true=1)
     write_png,savfname,image,rr,gg,bb
     endelse
  endfor                        ;END TIMESTEP LOOP
  set_plot,'x'
  loadct,0,/silent

end
