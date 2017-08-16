;+--------------------------------------------------------------------
pro test_pfss_shock_run_csgs
; A small procedure to run several instances of the coronal shock
; model.
  labels=['140708_01','131212_01','130517_01','130423_01','120915_01','120526_01',$
	  '120424_01','110607_01','110211_01','110125_01']
  labels=['paper','110511_01','130423_01','140708_01']
  labels=['131212_01','130517_01','120915_01','120526_01','120424_01','110607_01','110211_01','110125_01']

  labels=['100613_01','101103_02','101231_01','110125_01','110211_02','110327_01','110515_01','110607_01','110904_01','120307_01', '120424_01', '120526_01', '120728_01', '120915_01', '121007_01', '130423_01', '130501_01', '130517_01', '131107_01', '131207_01', '131212_01', '140217_01', '140708_01', '140901_01', '141105_02', '141205_01', '150303_01', '150421_01', '150920_01','151109_01']

  
  labels=['151104_01']
  
    for ev=0,n_elements(labels)-1 do begin
      label=labels[ev]
      event=load_events_info(label=label)
      pfss_shock_run_csgs,event,/lores ;,/force
      
      ;pfss_shock_plot_all,event,/lores,/force
      ;pfss_shock_run_csgs,event,/hires,/force
      ;pfss_shock_plot_all,event,/hires,/force
      ;aia_make_movies,event,movie_type='pfss_shock',/force
      ;pfss_shock_run_csgs,event,/lores,/newtimes
      ;pfss_shock_plot_all,event,/hires,/force
      
; pfss_shock_plot_csgs,event,/png,/newtimes
;      pfss_shock_plot_crossing_angles,event,/oplot
;      pfss_shock_plot_thetabn_stats,event,/lores
;      pfss_shock_plot_angular_influence,event
;      pfss_shock_plot_angular_influence,event,/topview
;new_pfss_shock_run_csgs,event,/hires,/newtimes;,/plot;,/png

   endfor
 end
;---------------------------------------------------------------------


;+--------------------------------------------------------------------
function get_rotation_angles,sstep,nsteps,lat,lon
;Get various rotation angles
  
;Rotation angles for the entire plot
  xrot_gen=(sstep*0.0)/nsteps
  yrot_gen=(sstep*0.0)/nsteps
  zrot_gen=(sstep*0.0)/nsteps
  genrot={xrot_gen:xrot_gen,yrot_gen:yrot_gen,zrot_gen:zrot_gen}
  
;Rotation angles for the PFSS points
  xrot_pfss=0+xrot_gen
  yrot_pfss=0+yrot_gen
  zrot_pfss=0+zrot_gen
  pfssrot={xrot_pfss:xrot_pfss,yrot_pfss:yrot_pfss,zrot_pfss:yrot_pfss}
  
;Rotation angles for the shock surface points
  xrot_shock=-lat+xrot_gen
  yrot_shock=lon+yrot_gen
  zrot_shock=0+zrot_gen
  csgsrot={xrot_shock:xrot_shock,yrot_shock:yrot_shock,zrot_shock:zrot_shock}

  rotang={genrot:genrot,pfssrot:pfssrot,csgsrot:csgsrot}
  return,rotang
end
;---------------------------------------------------------------------


;+--------------------------------------------------------------------
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
;---------------------------------------------------------------------


;+--------------------------------------------------------------------
pro get_crossing_points,sstep,nsteps,pfss_cartpos,csgs_init_coords,shockrad,shockthick,suncenter,sunrad,sph_data,pfsslines,ncrosses,crossPoints,cross_points,pind,exitstatus
  nmaxcrosses=5.0e4
  exitstatus=1
;A structure array that will hold the information about the
;field-shock crossings
  cashewtime=generate_struct('cashewtime')
  crossPoint={time:cashewtime,rpx:0.0D,rpy:0.0D,rpz:0.0D,px:0.0D,py:0.0D,pz:0.0D,thbn:0.0,linid:0L,$
              bmag:0.0D,density:0.0D,temperature:0.0,open:0,shockjump:0.0,vshock:0.0}
  crossPoints=replicate(crossPoint,nsteps,nmaxcrosses)
  
  dr2=(reform(pfss_cartpos[*,0,*])-csgs_init_coords[0])^2 + $
      (reform(pfss_cartpos[*,1,*])-csgs_init_coords[1])^2 + $
      (reform(pfss_cartpos[*,2,*])-csgs_init_coords[2])^2
  pfssrad=sqrt(dr2)
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
     exitstatus=-1
     return
  endelse
  
  cross_points=fltarr(3,ncrosses)
  for cross=0,ncrosses-1 do begin 
     cross_points[*,cross]=reform(pfss_cartpos[pind[0,cross],*,pind[1,cross]])
     crossPoints[sstep,cross].px=cross_points[0,cross]
     crossPoints[sstep,cross].py=cross_points[1,cross]
     crossPoints[sstep,cross].pz=cross_points[2,cross]
     
     pt=(reform(cross_points[*,cross])-[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter])/sunrad
     crossPoints[sstep,cross].rpx=pt[0] ;X-, Y-, and Z- positions of the point in Rsun
     crossPoints[sstep,cross].rpy=pt[1]
     crossPoints[sstep,cross].rpz=pt[2]
     
     sphpt=cart2sph([cross_points[0,cross]-suncenter.xcenter,cross_points[1,cross]-suncenter.ycenter,cross_points[2,cross]-suncenter.zcenter]/sunrad)
     crossPoints[sstep,cross].bmag=bfield_pfss(sphpt,sph_data)
     crossPoints[sstep,cross].linid=reform(pind[0,cross])
     crossPoints[sstep,cross].open=pfsslines[crossPoints[sstep,cross].linid].open
     ;if debug gt 0 and ff mod 100 eq 0 then print,'CSGS section, cross #'+strtrim(string(cross),2)
  endfor
  
;DEBUG!!!
  ;Check for open field lines that crossed in the previous time step,
  ;but were skipped in this one.
  if sstep gt 0 then begin
     tmp=where(crossPoints[sstep-1,*].open eq 1)
     if tmp[0] ne -1 then begin
        for ll=0,n_elements(tmp)-1 do begin
                                ;The pfss index of the crossing line
           lind=crossPoints[sstep-1,tmp[ll]].linid
                                ;Here check if this line has been covered in this time step - if not, continue to the next one
           res=where(crossPoints[sstep,*].linid eq lind)
           if res[0] ne -1 then continue
           
           dr2=(reform(pfss_cartpos[lind,0,*])-csgs_init_coords[0])^2 + $
               (reform(pfss_cartpos[lind,1,*])-csgs_init_coords[1])^2 + $
               (reform(pfss_cartpos[lind,2,*])-csgs_init_coords[2])^2
           pfssrad=sqrt(dr2)
           ptind=where(dr2 le (shockrad+shockthick)^2 and dr2 ge shockrad^2)
           ;minnn=min(abs(shockrad+shockthick-pfssrad),ptind)
           
           if ptind gt -1 then begin
                                ;pind=array_indices(reform(pfss_cartpos[lind,0,*]),ptind)
                                ;srt=sort(pind[0,*])
                                ;pind=pind[*,srt]
                                ;tst=reform(pind[0,*])
                                ;unique_lines=pind[*,uniq(tst)]
                                ;pind=unique_lines
              
              cross_point=reform(pfss_cartpos[lind,*,ptind])
              crosspt=crosspoint ;crosspoint struct already exists, just copy it over.
              crosspt.px=cross_point[0]
              crosspt.py=cross_point[1]
              crosspt.pz=cross_point[2]
              pt=(reform(cross_point)-[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter])/sunrad
              crosspt.rpx=pt[0] ;X-, Y-, and Z- positions of the point in Rsun
              crosspt.rpy=pt[1]
              crosspt.rpz=pt[2]
              sphpt=cart2sph([cross_point[0]-suncenter.xcenter,cross_point[1]-suncenter.ycenter,cross_point[2]-suncenter.zcenter]/sunrad)
              crosspt.bmag=bfield_pfss(sphpt,sph_data)
              crosspt.linid=lind
              crosspt.open=pfsslines[crosspt.linid].open
              if ncrosses eq nmaxcrosses then begin
              ;Concatenate the crossPoints structure of arrays with the new point info
                 newCrossPoints=replicate(crossPoint,nsteps,nmaxcrosses+1)
                 newCrossPoints[*,0:ncrosses-1]=crossPoints
                 newCrossPoints[sstep,ncrosses]=crosspt
                 crossPoints=newCrossPoints
              endif else begin
                 crossPoints[sstep,ncrosses]=crosspt
              endelse
              
              ;Concatenate the cross_points array with the new point location
              new_cross_points=fltarr(3,ncrosses+1)
              new_cross_points[*,0:ncrosses-1]=cross_points
              new_cross_points[*,ncrosses]=cross_point
              cross_points=new_cross_points
              ncrosses++
           endif
        endfor
     endif
  endif
;DEBUG!!!        

end
;---------------------------------------------------------------------


;+--------------------------------------------------------------------
pro pfss_shock_run_csgs,event,plot=plot,png=png,hires=hires,lores=lores,newtimes=newtimes,_extra=_extra
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
;       plot - plot the result of running the model
;       png - the same, but save the plots to png files
;       hires - use a high resolution PFSS model
;       lores - use a low resolution PFSS model (default)
;       newtimes - if a different time resolution AIA data is loaded,
;                  use the timestamps of that to calculate the CSGS radii.
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
  print,'Executing pfss_shock_run_csgs'
  wav='193'
  pfssfile=file_search(event.mfhcpath+event.pfss.loresmap_savename)
  if keyword_set(hires) then pfssfile=file_search(event.mfhcpath+event.pfss.hiresmap_savename)

  ;Load the measured shock wave radii.
  shockfile=file_search(event.annuluspath+replace_string(event.annplot.analyzed.radial.avg_savename,'WAV',wav))
  ;shockfile=replace_string(event.annplot.analyzed.radial.savename,'SSSSS','_m01')
  ;shockfile=event.annuluspath+replace_string(shockfile,'WAV',wav)
  print, 'Loading shock info file '+shockfile
  if shockfile[0] ne '' then begin
     restore,shockfile[0]
  endif else begin
     print,'No Shock data present. Quitting...'
     return
  endelse 
  
  
  ;Load the AIA subdata
  aiafile=file_search(event.savepath+replace_string(event.aia_subdata_savename,'WAV',wav))
  print,'Loading AIA File '+aiafile
  if aiafile[0] ne '' then begin
     restore,aiafile[0]
     if not keyword_set(png) and not keyword_set(plot) then subdata=0
     aiatime=anytim(subindex.date_obs)
     aiatime=aiatime-aiatime[0]
  endif else begin
     print,'No AIA data present. Quitting...'
     return
  endelse
  
;--------------------------------------------------------------



;--------------------------------------------------------------
;Constants and definitions
  
  loadct,8,/silent
  sp=rad_data.timefitrange[0]
  ep=rad_data.timefitrange[1]
  time=(rad_data.time[sp:ep].relsec-rad_data.time[sp].relsec)                              
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
  csgsradius=radiusfitlines/kmpx
  nsteps=n_elements(time)
  
  winsize=1024
  xcenter=subindex[0].x0_mp
  ycenter=subindex[0].y0_mp
  zcenter=0.0
  suncenter={xcenter:xcenter,ycenter:ycenter,zcenter:zcenter}
  
  sunrad=subindex[0].r_sun+10   ;For some reason the R_SUN variable is 10 px short...
  minshockrad=csgsradius[0]/kmpx
  maxshockrad=csgsradius[nsteps-1]/kmpx
  
  ;Get the proper indices for the AIA subindex array based on actual times
  match2,rad_data.time[sp:ep].date_obs,subindex.date_obs,suba,tmp
  subindex=subindex[suba]
  
  rsun_m=subindex[0].rsun_ref ;Solar radius in m.
  minAU=1.49598e11 ;m in AU
  mpix=rsun_m/sunrad ;conversion between pixels and m.
  shockthick=4.0                ;shock thickness, in pixels
  
;Calculate the number of steps and their size.
  dt = time[1]-time[0]           ;The cadence (maxshockrad-minshockrad)*mpix/(nsteps*1.0)/vshock ;timestep in seconds
  
;Variables for the crossing points information
  nmaxcrosses=5.0e4
  crossPoint={time:rad_data.time[sp],rpx:0.0D,rpy:0.0D,rpz:0.0D,px:0.0D,py:0.0D,pz:0.0D,thbn:0.0,linid:0L,$
              bmag:0.0D,density:0.0D,temperature:0.0,open:0,shockjump:0.0,vshock:0.0}
  ;crossPoint=generate_struct('crosspoint')
  crossPoints=replicate(crossPoint,nsteps,nmaxcrosses)
  allcrossAngles=fltarr(nsteps,nmaxcrosses)
  allcrosses=fltarr(nsteps)
;--------------------------------------------------------------
  
 

;--------------------------------------------------------------

;THIS IS THE STEPS LOOP!
     for sstep=0,nsteps-1 do begin             
        print,'Step #'+strtrim(string(sstep,format='(i3)'),2)
        shockrad=csgsradius[sstep] ;Get this from the measurements
        event=load_events_info(label=event.label)
;Save the rotation angles
        rotationAngles=get_rotation_angles(sstep,nsteps,event.arlat,event.arlon)
        
;+==============================================================================
;1. Plot the field lines on disk center.
        if sstep eq 0 then begin
;Convert the spherical to x,y,z coordinates.
;Switch y and z axes to make the coordinate system right-handed.
           carrlon=subindex[sstep].crln_obs*!PI/180.0 ;l
           carrlat=subindex[sstep].crlt_obs*!PI/180.0 ;b
           
           ;Get all the necessary information from the PFSS model
           if keyword_set(hires) then $
              pfss_get_field_line_info,event,pfssLines=pfssLines,$
                                       pfssfile=pfssfile,sph_data=sph_data,/hires $
           else $
              pfss_get_field_line_info,event,pfssLines=pfssLines,$
                                       pfssfile=pfssfile,sph_data=sph_data,/lores
           
           nlines=n_elements(pfssLines)

;Apply the rotations and translations and plot
           for ff=0.0D,nlines-1 do begin
              npt=pfssLines[ff].npts     ;the number of points on this particular line.
              
              pfss_sphtocart,pfssLines[ff].ptr,pfssLines[ff].ptth,pfssLines[ff].ptph,$
                             carrlon,carrlat,pfss_px,pfss_pz,pfss_py
              if ff eq 0 then begin
                 maxnpts=n_elements(pfss_px)
                 pfss_cartpos=fltarr(nlines,3,maxnpts)
              endif
              
              ;SAVE THE FIELD LINE INFORMATION TO A STRUCTURE ARRAY
              pos = transpose([[reform(pfss_px[0:npt-1])],$
                               [reform(pfss_py[0:npt-1])],$
                               [reform(pfss_pz[0:npt-1])]])
              pos = transform_volume(pos,rotation=[rotationAngles.pfssrot.xrot_pfss,$
                                                   rotationAngles.pfssrot.yrot_pfss,$
                                                   rotationAngles.pfssrot.zrot_pfss],$
                                     scale=[sunrad,sunrad,sunrad])
              pos = transform_volume(pos,translate=[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter])
              pfss_cartpos[ff,*,0:npt-1]=pos
              ;if ff mod 100 eq 0 then print,'PFSS section, line #'+strtrim(string(fix(ff)),2)+'/'+strtrim(string(fix(nlines)),2)
           endfor
           
           ;Free some memory
           linecols=0
           pfss_px=0
           pfss_pz=0
           pfss_py=0
           pos=0
           print,'PFSS section'
        endif
;-==============================================================================

        
;PLOT THE AIA IMAGE!
           if keyword_set(plot) or keyword_set(png) then begin
              aia_lct,rr,gg,bb,wavelnth=subindex[sstep].wavelnth,/load
              
              wdef,0,event.aiafov[0],event.aiafov[1]
              
              tv,bytscl(sqrt(subdata[*,*,sp+sstep]),min=1,max=50)
              
;OVERPLOT THE LIMB LOCATION
              circ=aia_circle(suncenter.xcenter,suncenter.ycenter,sunrad,/plot)

;PLOT THE PFSS INFORMATION
              for ff = 0.0D,nlines-1,4 do begin
                 npt = pfssLines[ff].npts ;the number of points in this particular line.
                 pos = reform(pfss_cartpos[ff,*,0:npt-1])
                 plots,pos,color=250,/device,psym=3
              endfor
           endif
           
;+==============================================================================
;3. Calculate and plot the spherical surface:
        
;Create the shock surface
; Calculate the latitudinal extension of the shock surface below theta=0.
;It is equal to -asin(shockrad/(2*sunrad))
        
        MESH_OBJ, 4, Vertex_List, Polygon_List, $ ;lists of polygons and vertices
           Replicate(shockrad, 100, 100)  , $
           p3=-asin(shockrad/(2*sunrad))
        
;apply rotation and translation to the surface
        Vertex_List = transform_volume(vertex_list,$
                                       translate=[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter+sunrad])
        vert_transmat=!P.T
        
        Vertex_List = transform_volume(vertex_list,$
                                       rotation=[rotationAngles.csgsrot.xrot_shock,$
                                                 rotationAngles.csgsrot.yrot_shock,$
                                                 rotationAngles.csgsrot.zrot_shock],$
                                       centre_rotation=[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter])
        vert_rotmat=!P.T
        
        ;Coordinates of the center of the shock surface.
        csgs_center_coords = transform_volume([0,0,0],translate=[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter+sunrad])
        csgs_center_coords = transform_volume(csgs_center_coords,rotation=[rotationAngles.csgsrot.xrot_shock,$
                                           rotationAngles.csgsrot.yrot_shock,$
                                           rotationAngles.csgsrot.zrot_shock],$
                                            centre_rotation=[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter])
        
        nverts=n_elements(vertex_list[0,*])
        index=subindex[sstep]
        
        if keyword_set(plot) or keyword_set(png) then begin
           ;aia_plot_hemisphere,index,shockrad,vertex_list=vertex_list
           loadct,9,/silent
           plots,csgs_center_coords[0],csgs_center_coords[1],psym=2,color=0,symsize=2,/device
           plots,vertex_list,color=0,thick=0.05,/device
           plots,vertex_list,color=0,thick=0.05,symsize=0.3,psym=sym(1),/device
        endif
;-==============================================================================


;+==============================================================================
;4. Determine the points where the field lines cross the shock
;surface of thickness shockthick
        ;get_crossing_points,sstep,nsteps,pfss_cartpos,csgs_center_coords,shockrad,shockthick,$
        ;                   suncenter,sunrad,sph_data,pfsslines,ncrosses,$
        ;                    crossPoints,cross_points,pind,exitstatus
                                ;if exitstatus eq -1 then continue
        ;A structure array that will hold the information about the field-shock crossings
        
        dr2=(reform(pfss_cartpos[*,0,*])-csgs_center_coords[0])^2 + $
            (reform(pfss_cartpos[*,1,*])-csgs_center_coords[1])^2 + $
            (reform(pfss_cartpos[*,2,*])-csgs_center_coords[2])^2
        pfssrad=sqrt(dr2)
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
           exitstatus=-1
           continue
        endelse
        
        cross_points=fltarr(3,ncrosses)
        for cross=0,ncrosses-1 do begin 
           cross_points[*,cross]=reform(pfss_cartpos[pind[0,cross],*,pind[1,cross]])
           crossPoints[sstep,cross].px=cross_points[0,cross]
           crossPoints[sstep,cross].py=cross_points[1,cross]
           crossPoints[sstep,cross].pz=cross_points[2,cross]
           
           pt=(reform(cross_points[*,cross])-[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter])/sunrad
           crossPoints[sstep,cross].rpx=pt[0] ;X-, Y-, and Z- positions of the point in Rsun
           crossPoints[sstep,cross].rpy=pt[1]
           crossPoints[sstep,cross].rpz=pt[2]
           
           sphpt=cart2sph([cross_points[0,cross]-suncenter.xcenter,cross_points[1,cross]-suncenter.ycenter,cross_points[2,cross]-suncenter.zcenter]/sunrad)
           crossPoints[sstep,cross].bmag=bfield_pfss(sphpt,sph_data)
           crossPoints[sstep,cross].linid=reform(pind[0,cross])
           crossPoints[sstep,cross].open=pfsslines[crossPoints[sstep,cross].linid].open
           crossPoints[sstep,cross].time=rad_data.time[sstep+sp]
           crossPoints[sstep,cross].vshock=rad_data.savgolfits.front[sstep+sp].speed
                                ;if debug gt 0 and ff mod 100 eq 0 then print,'CSGS section, cross #'+strtrim(string(cross),2)
        endfor
        
;DEBUG!!!
        ;Check for open field lines that crossed in the previous time step,
        ;but were skipped in this one.
        if sstep gt 0 then begin
           tmp=where(crossPoints[sstep-1,*].open eq 1)
           if tmp[0] ne -1 then begin
              for ll=0,n_elements(tmp)-1 do begin
                                ;The pfss index of the crossing line
                 lind=crossPoints[sstep-1,tmp[ll]].linid
                                ;Here check if this line has been covered in this time step - if not, continue to the next one
                 res=where(crossPoints[sstep,*].linid eq lind)
                 if res[0] ne -1 then continue
                 
                 dr2=(reform(pfss_cartpos[lind,0,*])-csgs_center_coords[0])^2 + $
                     (reform(pfss_cartpos[lind,1,*])-csgs_center_coords[1])^2 + $
                     (reform(pfss_cartpos[lind,2,*])-csgs_center_coords[2])^2
                 pfssrad=sqrt(dr2)
                 ptind=where(dr2 le (shockrad+shockthick)^2 and dr2 ge shockrad^2)
                                ;minnn=min(abs(shockrad+shockthick-pfssrad),ptind)
                 
                 if ptind gt -1 then begin
                                ;pind=array_indices(reform(pfss_cartpos[lind,0,*]),ptind)
                                ;srt=sort(pind[0,*])
                                ;pind=pind[*,srt]
                                ;tst=reform(pind[0,*])
                                ;unique_lines=pind[*,uniq(tst)]
                                ;pind=unique_lines
                    
                    cross_point=reform(pfss_cartpos[lind,*,ptind])
                    crosspt=crosspoint ;crosspoint struct already exists, just copy it over.
                    crosspt.time=rad_data.time[sstep+sp]
                    crosspt.vshock=rad_data.savgolfits.front[sstep+sp].speed
                    crosspt.px=cross_point[0]
                    crosspt.py=cross_point[1]
                    crosspt.pz=cross_point[2]
                    pt=(reform(cross_point)-[suncenter.xcenter,suncenter.ycenter,suncenter.zcenter])/sunrad
                    crosspt.rpx=pt[0] ;X-, Y-, and Z- positions of the point in Rsun
                    crosspt.rpy=pt[1]
                    crosspt.rpz=pt[2]
                    sphpt=cart2sph([cross_point[0]-suncenter.xcenter,cross_point[1]-suncenter.ycenter,cross_point[2]-suncenter.zcenter]/sunrad)
                    crosspt.bmag=bfield_pfss(sphpt,sph_data)
                    crosspt.linid=lind
                    crosspt.open=pfsslines[crosspt.linid].open
                    if ncrosses eq nmaxcrosses then begin
                    ;Concatenate the crossPoints structure of arrays with the new point info
                       newCrossPoints=replicate(crossPoint,nsteps,nmaxcrosses+1)
                       newCrossPoints[*,0:ncrosses-1]=crossPoints
                       newCrossPoints[sstep,ncrosses]=crosspt
                       crossPoints=newCrossPoints
                    endif else begin
                       crossPoints[sstep,ncrosses]=crosspt
                    endelse
                    
                    ;Concatenate the cross_points array with the new point location
                    new_cross_points=fltarr(3,ncrosses+1)
                    new_cross_points[*,0:ncrosses-1]=cross_points
                    new_cross_points[*,ncrosses]=cross_point
                    cross_points=new_cross_points
                  
                    ncrosses++
                 endif
              endfor
           endif
        endif
;DEBUG!!!
        allcrosses[sstep]=ncrosses
;Plot the field lines that pass through the shock surface
        if keyword_set(plot) or keyword_set(png) then begin
           colors=abs(randomn(10L,ncrosses))*255.
           loadct,13,/silent
           for ff=0,ncrosses-1 do begin
              npt=pfssLines[pind[0,ff]].npts
              if pfssLines[pind[0,ff]].open eq 1 then color=110 else color=230
              loadct,0,/silent
              plots,reform(pfss_cartpos[pind[0,ff],*,0:npt-1]),$
                    color=0,/device,thick=4
              loadct,13,/silent
              plots,reform(pfss_cartpos[pind[0,ff],*,0:npt-1]),$
                    color=color,/device,thick=2.5
           endfor
;plot the points of crossing in red.
           loadct,0,/silent
           plots,cross_points,color=0,psym=sym(1),symsize=0.9,/device
           loadct,13,/silent
           plots,cross_points,color=190,psym=sym(1),symsize=0.9,/device
           ;plot the points of crossing.

        endif
;-==============================================================================

       
;+==============================================================================
;6. Find the crossing angles. Find the local field direction and the
;normal to the shock first, then the angle ThetaBN at every point.
        ncrosses=allcrosses[sstep]
        normals=fltarr(3,ncrosses)
        local_field=fltarr(3,ncrosses)
        mag=fltarr(ncrosses)

;find the shock normals
        for i=0,ncrosses-1 do mag[i]=sqrt((cross_points[0,i] - csgs_center_coords[0])^2 + $
                                          (cross_points[1,i] - csgs_center_coords[1])^2 + $
                                          (cross_points[2,i] - csgs_center_coords[2])^2)
        for i=0,2 do begin
           normals[i,*] = (cross_points[i,*] - csgs_center_coords[i])/mag
        endfor
        
;find the local field direction
        for i=0,ncrosses-1 do begin
           ;Take the crossing point, and subtract
           ;its position from that of the one two points farther on the line to get the direction
           p1= reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
           p2= reform(pfss_cartpos[pind[0,i],*,pind[1,i]+2])
           mag=sqrt((p2[0]-p1[0])^2+(p2[1]-p1[1])^2+(p2[2]-p1[2])^2)
           local_field[*,i] = (p2-p1)/mag
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
        crossPoints[sstep,0:ncrosses-1].thbn=allcrossAngles[sstep,0:ncrosses-1]
;-==============================================================================
        
  
        ;Save the PFSS/CSGS/AIA image
        if keyword_set(png) then begin
           tvlct,rr,gg,bb,/get
           image=tvrd(true=1)
           stp=strtrim(string(sstep),2)
           if stp lt 100 then stp='0'+stp
           if stp lt 10 then stp='0'+stp
           timstring=strjoin(strsplit(strmid(subindex[sstep].date_obs,11,8),':',/extract))
           savename=event.mfhcpath+'aia_pfss_shock_'+event.date+'_'+event.label+'_lores_'+timstring+'.png'
           if keyword_set(hires) then savename=event.mfhcpath+'aia_pfss_shock_'+event.date+'_'+event.label+'_hires_'+timstring+'.png'
           write_png,savename,image,rr,gg,bb
        endif
        
     endfor;--------------- END TIMESTEP LOOP! ---------------
     nmaxcrosses=max(allcrosses)
     crossPoints=crossPoints[*,0:nmaxcrosses-1]

;Save the results to a file
     event=load_events_info(label=event.label)
     fname=event.csgs.lores.map_savename
     if keyword_set(hires) then fname=event.csgs.hires.map_savename
     print,'Saving file '+event.mfhcpath+fname

     
     save,filename=event.mfhcpath+fname,$
          allcrosses,dt,csgsradius,time,rotationAngles,crossPoints,carrlon,carrlat,$
          suncenter,nsteps,csgs_center_coords,radiusfitlines,subindex,$
          vertex_list,vert_rotmat,vert_transmat

     if keyword_set(hires) then pfss_shock_aschdem_plasma_info,event,/hires $
        else pfss_shock_aschdem_plasma_info,event,/lores
     
;-==============================================================================     
end ; END PFSS_SHOCK_RUN_CSGS


;function fieldline_isopen,ptc,sph_data
;  ;Find out if the field line is open or not.
;  r2d=180./!PI
;  d2r=!PI/180.
;  isopen=0
;  irc=get_interpolation_index(*sph_data.rix,ptc[0])
;  ithc=get_interpolation_index(*sph_data.lat,90-ptc[1]*r2d)
;  iphc=get_interpolation_index(*sph_data.lon,(ptc[2]*r2d+360) mod 360)
;  brc=interpolate(*sph_data.br,iphc,ithc,irc)
;  if brc gt 0 then isopen=1 else isopen=-1
;  return,isopen
;end
