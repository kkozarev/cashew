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


;function fieldline_isopenfieldline_isopen,ptc,sph_data
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


;+--------------------------------------------------------------------
pro test_pfss_shock_run_csgs_v1
; A small procedure to run several instances of the coronal shock
; model.
  
  event=load_events_info(label='paper')
  ;pfss_shock_run_csgs,event,/lores,/newtimes
  pfss_shock_run_csgs_v1,event,/hires,/newtimes;,/plot;,/png
  
end
;---------------------------------------------------------------------



;+--------------------------------------------------------------------
pro pfss_shock_run_csgs_v1,event,plot=plot,png=png,hires=hires,lores=lores,newtimes=newtimes
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
  print,'Executing pfss_shock_run_csgs...'
  
  wav='193'
  evnum=event.label
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  eventname='AIA_'+date+'_'+label+'_'+wav

;Figure out the name of the local machine.
  pcname=hostname()
  
  savepath=event.savepath
  datapath=savepath
  pfsspath=event.pfsspath
  

  pfssfile=file_search(pfsspath+'pfss_results_'+date+'_'+label+'_lores.sav')
  ;if keyword_set(lores) then pfssfile=file_search(pfsspath+'pfss_results_'+date+'_'+label+'_lores.sav')
  if keyword_set(hires) then pfssfile=file_search(pfsspath+'pfss_results_'+date+'_'+label+'_hires.sav')
  
  aiafile=file_search(datapath+'normalized_'+eventname+'_subdata.sav')
  shockfile=file_search(event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed_radial.sav')
  
  
  ;Load the measured shock wave radius.
  ;This was created with measure_wave_sphere.pro
  ;print, 'Loading shock info file '+datapath+eventname+'_shocklocations.sav'
  ;restore,datapath+eventname+'_shocklocations.sav'
  print, 'Loading shock info file '+shockfile
  if shockfile[0] ne '' then begin
     restore,shockfile[0]
  endif else begin
     print,'No Shock data present. Quitting...'
     return
  endelse 
  
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
  xcenter=subindex[0].x0_mp
  ycenter=subindex[0].y0_mp
  zcenter=0.0
  suncenter=[xcenter,ycenter,zcenter]
  
  sunrad=subindex[0].r_sun+10;For some reason the R_SUN variable is 10 px short...
  minshockrad=radius[0]/kmpx
  maxshockrad=radius[nsteps-1]/kmpx
  subindex=subindex[sp:ep]
  
  rsun_m=subindex[0].rsun_ref ;Solar radius in m.
  minAU=1.49598e11 ;m in AU
  mpix=rsun_m/sunrad ;conversion between pixels and m.
  
  shockthick=3.5                ;shock thickness, in pixels
  
;Calculate the number of steps and their size.
  dt= time[1]-time[0]           ;The cadence (maxshockrad-minshockrad)*mpix/(nsteps*1.0)/vshock ;timestep in seconds
  
;Variables for the crossing points information
  nmaxcrosses=5.0e4
 ; allcrossPoints=fltarr(nsteps,3,nmaxcrosses)
  allcrossAngles=fltarr(nsteps,nmaxcrosses)
 ; allcrossBmag=fltarr(nsteps,nmaxcrosses)
 ; allcrossLineIndices=fltarr(nsteps,nmaxcrosses)
  allcrosses=fltarr(nsteps)
  
;A structure array that will hold the information about the field-shock crossings
  crossPoint={rpx:0.0D,rpy:0.0D,rpz:0.0D,px:0.0D,py:0.0D,pz:0.0D,thbn:0.0D,linid:0L,bmag:0.0D,open:0}
  crossPoints=replicate(crossPoint,nsteps,nmaxcrosses)
           
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
           
           ;Get all the necessary information from the PFSS model
           if keyword_set(hires) then $
              pfss_get_field_line_info,event,pfssLines=pfssLines,pfssfile=pfssfile,sph_data=sph_data,/hires $
           else pfss_get_field_line_info,event,pfssLines=pfssLines,pfssfile=pfssfile,sph_data=sph_data,/lores
           
           nlines=n_elements(pfssLines)
          ;   pfss_sphtocart,ptr,ptth,ptph,carrlon,carrlat,pfss_px,pfss_pz,pfss_py
           
          ;  nlines=n_elements(pfss_px[0,*])*1.0D
           
          ; maxnpts=n_elements(pfss_px[*,0])
           
;Apply the rotations and translations and plot
          ; pfss_cartpos=fltarr(nlines,3,maxnpts)
           
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
              pos = transform_volume(pos,rotation=[xrot_pfss,yrot_pfss,zrot_pfss],$
                                     scale=[sunrad,sunrad,sunrad])
              pos = transform_volume(pos,translate=[xcenter,ycenter,zcenter])
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
              
              if sstep eq 0 then begin
                 wdef,0,event.aiafov[0],event.aiafov[1]
                                ;tvlct,rrr,ggg,bbb,/get
              endif
              tv,bytscl(sqrt(subdata[*,*,sp+sstep]),min=1,max=50)
              
;OVERPLOT THE LIMB LOCATION
              circ=aia_circle(xcenter,ycenter,sunrad,/plot)

              
;PLOT THE PFSS INFORMATION
              for ff = 0.0D,nlines-1,4 do begin
                 npt = pfssLines[ff].npts ;the number of points in this particular line.
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
        for cross=0,ncrosses-1 do begin 
           cross_points[*,cross]=reform(pfss_cartpos[pind[0,cross],*,pind[1,cross]])
           crossPoints[sstep,cross].px=cross_points[0,cross]
           crossPoints[sstep,cross].py=cross_points[1,cross]
           crossPoints[sstep,cross].pz=cross_points[2,cross]
           
           pt=(reform(cross_points[*,cross])-suncenter)/sunrad
           crossPoints[sstep,cross].rpx=pt[0] ;X-, Y-, and Z- positions of the point in Rsun
           crossPoints[sstep,cross].rpy=pt[1]
           crossPoints[sstep,cross].rpz=pt[2]
           
           sphpt=cart2sph([cross_points[0,cross]-xcenter,cross_points[1,cross]-ycenter,cross_points[2,cross]-zcenter]/sunrad)
           cross_bmag[cross]=bfield_pfss(sphpt,sph_data)
           crossPoints[sstep,cross].bmag=cross_bmag[cross]
           crossPoints[sstep,cross].linid=reform(pind[0,cross])
           crossPoints[sstep,cross].open=pfsslines[crossPoints[sstep,cross].linid].open
           ;if ff mod 100 eq 0 then print,'CSGS section, cross #'+strtrim(string(cross),2)
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
                 
                 dr2=(reform(pfss_cartpos[lind,0,*])-sc[0])^2 + $
                     (reform(pfss_cartpos[lind,1,*])-sc[1])^2 + $
                     (reform(pfss_cartpos[lind,2,*])-sc[2])^2
                 pfssrad=sqrt(dr2)
                 ;ptind=where(dr2 le (shockrad+shockthick)^2 and dr2 ge shockrad^2)
                 minnn=min(abs(shockrad+shockthick-pfssrad),ptind)
                 
                 if ptind gt -1 then begin
                    cross_point=reform(pfss_cartpos[lind,*,ptind])
                    crosspt=crosspoint ;crosspoint struct already exists, just copy it over.
                    crosspt.px=cross_point[0]
                    crosspt.py=cross_point[1]
                    crosspt.pz=cross_point[2]
                    pt=(reform(cross_point)-suncenter)/sunrad
                    crosspt.rpx=pt[0] ;X-, Y-, and Z- positions of the point in Rsun
                    crosspt.rpy=pt[1]
                    crosspt.rpz=pt[2]
                    sphpt=cart2sph([cross_point[0]-xcenter,cross_point[1]-ycenter,cross_point[2]-zcenter]/sunrad)
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
                    ;Concatenate the pind variable
                    new_pind=lonarr(2,ncrosses+1)
                    new_pind[*,0:ncrosses-1]=pind
                    new_pind[*,ncrosses]=[lind,ptind]
                    pind=new_pind
                    ncrosses++
                 endif
              endfor
           endif
        endif
;DEBUG!!!        
        
;        allcrossPoints[sstep,*,0:ncrosses-1]=cross_points*1.0D
;        allcrossBmag[sstep,0:ncrosses-1]=cross_bmag*1.0D
;        allcrossLineIndices[sstep,0:ncrosses-1]=reform(pind[0,*])
        allcrosses[sstep]=ncrosses

;Plot the field lines that pass through the shock surface
        if keyword_set(plot) or keyword_set(png) then begin
           colors=abs(randomn(10L,ncrosses))*255.
           loadct,13,/silent
           for ff=0,ncrosses-1 do begin
              npt=pfssLines[pind[0,ff]].npts
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
        crossPoints[sstep,0:ncrosses-1].thbn=allcrossAngles[sstep,0:ncrosses-1]
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
        
     endfor;--------------- END TIMESTEP LOOP! ---------------
     nmaxcrosses=max(allcrosses)
     crossPoints=crossPoints[*,0:nmaxcrosses]

     
;CREATE A STRUCTURE TO HOLD THE CROSS-POINT RESULTS FOR EASY PROCESSING LATER
;     nmaxcrosses=max(allcrosses)
;     crossPoint={px:0.0D,py:0.0D,pz:0.0D,thbn:0.0D,linid:0L,bmag:0.0D}
;     crossPoints=replicate(crossPoint,nsteps,nmaxcrosses)
     
        ;This array will hold the closest and farthest point coordinates for every interacting
        ;field line
          ; endPointCoords=replicate({ptr:0.0,ptth:0.0,ptph:0.0,linid:0L},nsteps,nmaxcrosses,2)
         ;  lineSpread=dblarr(nsteps,max(allcrosses),3,2)
         ;  maxLonExtent=dblarr(nsteps)
           
;     for sstep=0,nsteps-1 do begin
;        ncrosses=allcrosses[sstep]
;        for cross=0,ncrosses-1 do begin
;           crossPoints[sstep,cross].px=allcrosspoints[sstep,0,cross]
;           crossPoints[sstep,cross].py=allcrosspoints[sstep,1,cross]
;           crossPoints[sstep,cross].pz=allcrosspoints[sstep,2,cross]
;           crossPoints[sstep,cross].linid=allcrossLineIndices[sstep,cross]
;           crossPoints[sstep,cross].thbn=allcrossAngles[sstep,cross]
;           crossPoints[sstep,cross].bmag=allcrossBmag[sstep,cross]          
           
;Get the spherical coordinates for the starting and ending point for
;every interacting field line.
;           linid=allcrossLineIndices[sstep,cross]
;        endPointCoords[sstep,cross,0].ptr=pfssLines[linid].ptr[0]
;        endPointCoords[sstep,cross,0].ptth=pfssLines[linid].ptth[0]
;        endPointCoords[sstep,cross,0].ptph=pfssLines[linid].ptph[0]
;        endPointCoords[sstep,cross,0].linid=pfssLines[linid].linid
;        endPointCoords[sstep,cross,1].ptr=pfssLines[linid].ptr[pfssLines[linid].npts-1]
;        endPointCoords[sstep,cross,1].ptth=pfssLines[linid].ptth[pfssLines[linid].npts-1]
;        endPointCoords[sstep,cross,1].ptph=pfssLines[linid].ptph[pfssLines[linid].npts-1]
;        endPointCoords[sstep,cross,1].linid=pfssLines[linid].linid
;        endfor
;      endfor


;This array will hold the closest and farthest point coordinates for every interacting
;field line
  ;lineSpread=dblarr(nsteps,nmaxcrosses,3,2)
  ;maxLonExtent=dblarr(nsteps)
;for sstep=0,nsteps-1 do begin
;   ncrosses=allcrosses[sstep]
;   for cross=0,ncrosses-1 do begin
;Get the largest and smallest coordinates for each field line, for
;each time step.
;      linid=crossPoints[sstep,cross].linid
;      lineSpread[sstep,cross,0,*]=minmax(pfssLines[linid].ptr[0:pfssLines[linid].npts-1])
;      lineSpread[sstep,cross,1,*]=minmax(pfssLines[linid].ptth[0:pfssLines[linid].npts-1])
;      lineSpread[sstep,cross,2,*]=minmax(pfssLines[linid].ptph[0:pfssLines[linid].npts-1])
;   endfor  
;Find the largest longitudinal extent for this particular time step.
;   tmp=abs(reform(lineSpread[sstep,0:ncrosses-1,2,*])-lon*!PI/180.)
;   maxLonExtent[sstep]=max(tmp)*180./!PI
;endfor

;Save the results to a file
     resstr='_hires'
     if keyword_set(lores) then resstr='_lores'
     fname='csgs_results_'+event.date+'_'+event.label+resstr+'.sav'
     print,'Saving file '+pfsspath+fname
     save,filename=pfsspath+fname,$
          allcrosses,dt,radius,time,rotationAngles,crossPoints,carrlon,carrlat,$
          suncenter,nsteps,sc,radiusfitlines,subindex,$
          vertex_list,vert_rotmat,vert_transmat
;-==============================================================================     
end ; END PFSS_SHOCK_RUN_CSGS
