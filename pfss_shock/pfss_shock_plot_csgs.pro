pro test_pfss_shock_plot_csgs
;Testing the CSGS model plotting procedure
event=load_events_info(label='paper')
pfss_shock_plot_csgs,event,/hires,/png;/newtimes,
end


pro pfss_shock_plot_csgs,event,png=png,hires=hires,lores=lores,pfssLines=pfssLines,newtimes=newtimes,force=force
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
;04/06/2015, Kamen Kozarev - added the /force keyword.
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
  resolution='lores'
  if keyword_set(hires) then resolution='hires'
  
  ;Find a file to load with the latest results of applying the CSGS model
  ;csgsfile=find_latest_file(event.pfsspath+'csgs_results_*')
  csgsfile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_lores.sav')
  if keyword_set(hires) then csgsfile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_hires.sav')
  if csgsfile[0] eq '' then begin
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
  aiafile=file_search(event.savepath+'normalized_'+eventname+'_subdata.sav')
  print,'Loading AIA File '+aiafile
  if aiafile[0] ne '' then begin
     restore,aiafile[0]
     aiatime=anytim(subindex.date_obs)
     aiatime=aiatime-aiatime[0]
  endif else begin
     print,'No AIA data present. Quitting...'
     return
  endelse

     
  ;Load the Annulusplot analysis results
  wavefile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed_radial.sav'
  print, 'Loading shock info file '+wavefile
  restore,wavefile

;-==============================================================================


;+==============================================================================
;Constants and definitions
  sp=rad_data.xfitrange[0]
  ep=rad_data.xfitrange[1]
  time=(rad_data.time[sp:ep]-rad_data.time[sp])
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.  
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
   if keyword_set(newtimes) then begin
     newtime=aiatime
     pfss_shock_generate_csgs_radii,ind_arr,rad_data,radiusfitlines,newtime=newtime,tindrange=tindrange
     time=newtime
     sp=tindrange[0]
     ep=tindrange[1]
  endif else begin
     pfss_shock_generate_csgs_radii,ind_arr,rad_data,radiusfitlines
  endelse  
  radiusfitlines*=RSUN*event.geomcorfactor
  radius=radiusfitlines/kmpx
  nsteps=n_elements(time)
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
     stp=strtrim(string(sstep),2)
     if stp lt 100 then stp='0'+stp
     if stp lt 10 then stp='0'+stp
     
     savefname=pfsspath+'aia_pfss_shock_'+event.date+'_'+event.label+'_'+resolution+'_'+stp+'.png'
     if file_test(savefname) and not keyword_set(force) then begin
        print,'Files '+'aia_pfss_shock_'+event.date+'_'+event.label+'_'+resolution+'_*.png'
        print,'already exist. To overwrite, rerun with /force.'
        print,'----'
        break
     endif else begin

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
; Plot the field lines on disk center.
     if sstep eq 0 then begin
        ;Get the field line info from the PFSS model results
          if not keyword_set(pfssLines) then $
             if keyword_set(hires) then pfss_get_field_line_info,event,pfssLines=pfssLines,/hires $
             else pfss_get_field_line_info,event,pfssLines=pfssLines,/lores
        nlines=n_elements(pfssLines)
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

;Apply the rotations and translations and plot
        pfss_cartpos=fltarr(nlines,3,maxnpts)
        for ff=0.0D,nlines-1 do begin
           ;the number of points in this particular line.
           npt=pfssLines[ff].npts
           pfss_sphtocart,pfssLines[ff].ptr,pfssLines[ff].ptth,pfssLines[ff].ptph,$
                          carrlon,carrlat,px,pz,py
           pos = transpose([[reform(px[0:npt-1])],[reform(py[0:npt-1])],[reform(pz[0:npt-1])]])
           ;TRANSFORM THE POINTS APPROPRIATELY
           pos = transform_volume(pos,scale=[sunrad,sunrad,sunrad])
           pos = transform_volume(pos,translate=suncenter)
           pfss_cartpos[ff,*,0:npt-1]=pos        
           ;Plot the field line 
           ;plots,pos,color=250,/device,psym=3
        endfor
        pos=0
     endif
     
     for ll=0.0D,nPlotLines-1 do begin
        ff=plotLinesIndex[ll]
        npt=pfssLines[ff].npts
        ;Plot the field lines
        if pfss_cartpos[ff,2,0] gt 0.0 and pfss_cartpos[ff,2,npt-1] gt 0.0 then $
           plots,reform(pfss_cartpos[ff,*,0:npt-1]),/device,color=255,thick=1
     endfor

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
;        plots,reform(pfss_cartpos[lind,*,0:npt-1]),$
;              color=color,/device,psym=sym(1),symsize=0.5
        loadct,0,/silent
        plots,reform(pfss_cartpos[lind,*,0:npt-1]),$
              color=0,/device,thick=4
        loadct,13,/silent
        plots,reform(pfss_cartpos[lind,*,0:npt-1]),$
              color=color,/device,thick=2.5
        ;plot the points of crossing.
        loadct,0,/silent
        plots,[cpsx[ff],cpsy[ff],cpsz[ff]],color=0,psym=sym(1),symsize=0.9,/device
        loadct,13,/silent
        plots,[cpsx[ff],cpsy[ff],cpsz[ff]],color=190,psym=sym(1),symsize=0.6,/device
     endfor

;-==============================================================================
     


;+==============================================================================
; CALCULATE AND PLOT THE CSGS MODEL
       
;Create the shock surface
     MESH_OBJ, $
        4, $
        Vertex_List, Polygon_List, $ ;lists of polygons and vertices
        Replicate(shockrad, 100, 100), $
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
     plots,vertex_list,color=0,thick=0.1,symsize=0.2,psym=sym(1),/device
;-==============================================================================
     
     
;+==============================================================================
;Plot the field lines that pass through the shock surface
     ncrosses=allcrosses[sstep]
     cpsx=crossPoints[sstep,0:ncrosses-1].px
     cpsy=crossPoints[sstep,0:ncrosses-1].py
     cpsz=crossPoints[sstep,0:ncrosses-1].pz 
     loadct,13,/silent
     stride=1
     
     for ff=0.D,ncrosses-1,stride do begin
        ;plot the points of crossing.
        loadct,0,/silent
        plots,[cpsx[ff],cpsy[ff],cpsz[ff]],color=0,psym=sym(1),symsize=0.9,/device
        loadct,13,/silent
        plots,[cpsx[ff],cpsy[ff],cpsz[ff]],color=190,psym=sym(1),symsize=0.6,/device
     endfor

;-==============================================================================
     
     
     ;Save the plot in a png file
     tvlct,rr,gg,bb,/get
     image=tvrd(true=1)
     write_png,savefname,image,rr,gg,bb
  endelse
     
  endfor ;END TIMESTEP LOOP
  set_plot,'x'
  loadct,0,/silent
end
