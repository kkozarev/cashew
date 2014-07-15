function bfield,rmag
;Return field as a function of radius from Gopalswamy paper
return,0.409*rmag^(-1.3) * 1.0e-4 ;Field magnitude in Tesla
end

function bfield_pfss,ptc,sph_data
;Use interpolation on the PFSS model - this is better!
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


;+--------------------------------------------------------------------
pro test_aia_model_shock_pfss
; A small procedure to run several instances of the coronal shock
; model.
  
  compression=[1.13,2,4]     ;shock compression ratios.
  ;compression=4.00
  vshock=400.
  event=load_events_info(label='test')
  
  ;Run the model here for all shock jump cases!
  for cc=0,n_elements(compression)-1 do begin
     aia_model_shock_pfss,event,shockcomp=compression[cc],vupstream=vshock,/plot,/png
     stop
  endfor

;Plot the energy histogram
  pfss_shock_plot_energy_histogram,event,compression=compression,vshock=vshock
end
;---------------------------------------------------------------------



;+--------------------------------------------------------------------
pro aia_model_shock_pfss,event,wav=wav,shockcomp=shockcomp,plot=plot,png=png,vupstream=vupstream
;This procedure runs the pfss/shock model for estimating shock
;orientation to magnetic fields, and 
;Kamen Kozarev 01/15/2012
;Last Updated: 12/06/2013 by KAK
  resolve_routine,'sym',/either,/compile_full_file
  

;LOAD DATA
  print,''
  print,'Loading data...'
  
  if not keyword_set(wav) then wav='193'
  evnum=event.label
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  eventname='AIA_'+date+'_'+evnum+'_'+wav
  

;Figure out the name of the local machine.
  pcname=hostname()

  
;for sloncho-2
  if pcname eq 'sloncho-2' then begin
     restore,'/Users/kkozarev/AIA/pfss_results_'+date+'_1.0Rs.sav'
  endif else begin
     
;for arien
     if pcname eq 'arien' then begin
        savepath=event.savepath
        datapath=savepath
        pfsspath=event.pfsspath
        
        pfssfile=pfsspath+'pfss_results_'+date+'_'+label+'_hires.sav'
        shockfile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed.sav'
        
        print,'Loading AIA File '+datapath+'normalized_'+eventname+'_subdata.sav'
        restore,datapath+'normalized_'+eventname+'_subdata.sav'
        
        ;Load the measured shock wave radius.
        ;This was created with measure_wave_sphere.pro
        ;print, 'Loading shock info file '+datapath+eventname+'_shocklocations.sav'
        ;restore,datapath+eventname+'_shocklocations.sav'
        
        print, 'Loading shock info file '+shockfile
        restore,shockfile
        sp=rad_data.xfitrange[0]
        ep=rad_data.xfitrange[1]
        time=(rad_data.time[sp:ep]- rad_data.time[sp])*3600.
        nsteps=n_elements(time)
                                ;timestep in seconds
        ;dt=wave_times[1:nsteps-1]-wave_times[0:nsteps-2]
  
          ;Convert radius to km above the surface
        RSUN=subindex[0].rsun_ref/1000. ;Solar radius in km.  
        KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
        fit=reform(rad_data.fitparams[0,*].front)
        radiusfitlines=(fit[0]+fit[1]*time+0.5*fit[2]*time^2)/RSUN
        radiusfitlines-=1.
        radiusfitlines*=RSUN*event.geomcorfactor
        radius=radiusfitlines/kmpx
        
        
        lon=event.arlon
        lat=event.arlat
        
        ;Load the PFSS model results
        print,'Loading PFSS File '+pfssfile
        restore,pfssfile
        
        
     endif else begin
     endelse
  endelse
;--------------------------------------------------------------



;--------------------------------------------------------------
;Constants and definitions
  loadct,8,/silent
  

  winsize=1024
  xcenter=subindex[0].x0_mp;winsize/2.0
  ycenter=subindex[0].y0_mp;winsize/2.0
  zcenter=0.0;winsize/2.0
  suncenter=[xcenter,ycenter,zcenter]

  sunrad=subindex[0].r_sun+10;For some reason the R_SUN variable is 10 px short...
  minshockrad=radius[0]/kmpx
  maxshockrad=radius[nsteps-1]/kmpx
  subindex=ind_arr[sp:ep]
;Physical constants
  rsun_m=subindex[0].rsun_ref ;Solar radius in m.
  minAU=1.49598e11 ;m in AU
  mp=1.67e-27 ;proton mass in kg
  MeVinJ=1.6e-19 ;conversion between MeV and Joules
  kboltz=1.38e-23 ;Boltzmann constant in SI
  echarge=1.602e-19 ;Electron charge in Coulomb
  mpix=rsun_m/sunrad ;conversion between pixels and m.
  
  ;;;;;;;;;;;;;;;;;;;;;;;
  ;Physical parameters
  
  ;A. Solar Corona
  ;A.1. Need a 1D (spherically symmetric) model for the B-field.
  ;Use the one from Gopalswamy (2011) - B(r)=pr^(-q),p=0.409,q=1.30
  ;
  ;A.2. Need a 1D (spherically symmetric) model for the solar wind speed.
  ;Use a model from a SWMF run by Rebekah Evans out to 2 Rs.
  ;
  
  ;The solar coronal temperature, in Kelvin
  Temp=8000
  ;The ratio of the perpendicular to parallel diffusion.
  kperptokpar=0.01
  
  ;B. Shock parameters
  ;B.1. Shock compression - it will be set from the observed
  ;shock front speed and the upstream modeled speed.
  ;currently it is manually specified.
  if keyword_set(shockcomp) then shockjumps=shockcomp else shockjumps=4
  print,shockjumps
  nshockjumps=n_elements(shockjumps)
  if not keyword_set(vupstream) then vshock=450 * 1000.0 $ ;upstream plasma speed in the shock frame, m/s
  else vshock=vupstream * 1000.0
  ;
  ;B.2. Shock Thickness
  shockthick=3.0                ;shock thickness, in pixels
  ;
  
  
  ;C. Particles
  mfp=0.001 * minAU ;assume some parallel mean free path, in m.
  mine=0.01 ;minimum energy of protons, MeV
  maxe=1000.0 ;maximum energy of protons, MeV; protons will escape at this energy.
  ;;;;;;;;;;;;;;;;;;;;;;;
  
  
;Calculate the number of steps and their size.
  arcoords=[lat,lon]
  dt= time[1]-time[0] ;The cadence (maxshockrad-minshockrad)*mpix/(nsteps*1.0)/vshock ;timestep in seconds
  
  
  ;Create the proton momentum grid
  nenrgs=100
  gride=dblarr(nenrgs) ;The energy grid in eV
  ;Use a scheme in which energy bins are created exponentially. It covers the energy
  ;range better. In that scheme, E[i]=E0*((E1/E0)^(i/Nergs))
  emin=mine * MeVinJ
  emax=maxe * MeVinJ
   
  ;Create a momentum grid
  for i=0,nenrgs-1 do gride[i]=mine*(maxe/mine)^(i/((nenrgs-1)*1.0))
  egrid=gride * MeVinJ ;The energy grid in Joules
  pgrid = sqrt(mp *2) * sqrt(gride*MeVinJ) ;momentum in kg m/s
  nerg=n_elements(egrid)
  
  
  ;Calculate the Maxwellian momentum distribution
  ;for these energies and split it into particles.
  aa=(mp/(2*!PI*kboltz*Temp))^1.5
  bb=4*!PI*(pgrid/mp)^2
  cc=(pgrid/sqrt(mp))^2/(2*kboltz*Temp)
  initdist= aa * bb * exp(-cc)
  
  
  ;Get some proxy for the number of particles
  initparticles=initdist/min(initdist)
  
  
;Save the crossing points information
  allcrossPoints=fltarr(nsteps,3,100000)
  allcrossAngles=fltarr(nsteps,100000)
  allcrossLineIndices=intarr(nsteps,100000)
  allcrosses=fltarr(nsteps)
  allcrossMomentum=fltarr(nsteps,100000)
;--------------------------------------------------------------
  

;--------------------------------------------------------------
;--------------------------------------------------------------
;Loop over all the shock jumps
  for jump=0,nshockjumps-1 do begin            ; THIS IS THE SHOCKJUMP LOOP!
     shockjump=shockjumps[jump]
     print,'Shock Jump is r='+string(shockjump)
;Loop over all the steps 
     for sstep=0,nsteps-1 do begin             ;THIS IS THE STEPS LOOP!
        print,'Step #'+string(sstep)
                                ;calculate the shock radius
                                ;shockrad=minshockrad+((maxshockrad-minshockrad)*sstep)/(nsteps-1)
        
        shockrad=radius[sstep] ;Get this from the measurements
        print,shockrad
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
        if sstep eq 0 and jump eq 0 then begin
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
              
              npt=nstep[ff]     ;the number of points in this particular line.

              

              ;SAVE THE FIELD LINE INFORMATION TO A STRUCTURE ARRAY
              if ff eq 0 then begin
                 pfssLine={npts:0L,px:dblarr(max(nstep)),py:dblarr(max(nstep)),pz:dblarr(max(nstep)),open:0}
                 pfssLines=replicate(pfssLine,nlines)
              endif
              pfssLines[ff]={npts:npt,px:pfss_px[*,ff],py:pfss_py[*,ff],px:pfss_px[*,ff],open:0,linid:ff}
              

              
              ;Transform to the current view
              pos = transpose([[reform(pfss_px[0:npt-1,ff])],$
                               [reform(pfss_py[0:npt-1,ff])],$
                               [reform(pfss_pz[0:npt-1,ff])]])
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
        
        
;PLOT THE AIA IMAGE!
        if jump eq 0 then begin
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
        endif
        
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
        
        ;Initial coordinates of the center of the shock surface.
        sc = transform_volume([0,0,0],translate=[xcenter,ycenter,zcenter+sunrad])
        sc = transform_volume(sc,rotation=[xrot_shock,yrot_shock,zrot_shock],$
                              centre_rotation=[xcenter,ycenter,zcenter])
        
        nverts=n_elements(vertex_list[0,*])
        index=subindex[sstep]
        if jump eq 0 then begin
           if keyword_set(plot) or keyword_set(png) then begin
                                ;aia_plot_hemisphere,index,shockrad,vertex_list=vertex_list
              loadct,9,/silent
              plots,sc[0],sc[1],psym=2,color=0,symsize=2,/device
              plots,vertex_list,color=0,thick=0.05,/device
              plots,vertex_list,color=180,thick=0.1,symsize=0.6,psym=sym(1),/device
           endif
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
        for i=0,ncrosses-1 do cross_points[*,i]=reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
        allcrossPoints[sstep,*,0:ncrosses-1]=cross_points*1.0D
        allcrossLineIndices[sstep,0:ncrosses-1]=reform(pind[0,*])
        allcrosses[sstep]=ncrosses
;Plot the field lines that pass through the shock surface
        if jump eq 0 then begin
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
        ;print,dotp
        
        
        ;The angles are acos((a dot b)/(|a|*|b|))
        ;prod=total(abs(normals),1) * total(abs(local_field),1)
        ;thetabn=acos(dotp/prod) ;I've already divided by the magnitudes
        thetabn=acos(dotp)
        
        ind=where(thetabn gt !PI/2.0)
        if ind[0] gt -1 then thetabn[ind]=!PI-thetabn[ind]
        th=thetabn*180.0/!PI
        allcrossAngles[sstep,0:ncrosses-1]=th
;-==============================================================================
        



;+==============================================================================
;5. Go over the field lines and add particles if there are newly-crossing
;field lines
        ;newLines=reform(pind[1,inds])
;        newLines=reform(allcrossLineIndices[sstep,0:ncrosses-1])
;        cc=0
;        diffind=-1
;        if sstep eq 0 then begin
;           allIndLines=newLines
;           allPartMomentum=fltarr(n_elements(newLines))+pgrid[0]
;        endif else begin
;          ;Loop over all the crossing lines,
;           ;checking if this line index exists already
;           for i=0,ncrosses-1 do begin
;              tmp=where(newLines[i] eq allIndLines)
;              ;If it doesn't, save it.
;              if tmp[0] eq -1 then begin
;                 if cc eq 0 then diffind = i else $
;                    diffind=[diffind,i]
;                 cc++
;              endif
;           endfor
;           if diffind[0] gt -1 then begin
;              allIndLines=[allIndLines,newLines[diffind]]
;              allPartMomentum=[allPartMomentum,(fltarr(n_elements(diffind))+pgrid[0])]
;              ;Order the field line positions in the arrays.
;              ;Don't forget to reorder the energies as well!
;              srt=sort(allIndLines)
;              allIndLines=allIndLines[srt]
;              allPartMomentum=allPartMomentum[srt]
;           endif
;           stop
;        endelse
        
;WRITING A BETTER VERSION
           allPartMomentum=dblarr(ncrosses)+pgrid[0]
           if sstep gt 0 then begin
           ;Loop over all the crossing lines in the previous time step.
              prevlines=reform(allcrossLineIndices[sstep-1,0:allcrosses[sstep-1]-1])
              currlines=reform(allcrossLineIndices[sstep,0:allcrosses[sstep]-1])
              for i=0,allcrosses[sstep-1]-1 do begin
              ;Check if the line repeats in this time step.
                 tmp=where(currlines eq prevlines[i])
                                ;If it does, transfer the momentum
                                ;from the previous time step.
                ; stop
                 if tmp[0] gt -1 then begin
                    allPartMomentum[tmp[0]]=allcrossMomentum[sstep-1,i]
                 endif else begin
                 ;If it doesn't, retire that line and save the momentum as the final momentum
                 ;reached on that line.
                    szz=size(finalMomentum)
                    if szz[1] eq 0 then begin
                       finalMomentum=allcrossMomentum[sstep-1,i]
                       finalMomentumLineIndex=prevlines[i]
                       endif else begin
                          finalMomentum=[finalMomentum,allcrossMomentum[sstep-1,i]]
                          finalMomentumLineIndex=[finalMomentumLineIndex,prevlines[i]]
                       endelse
                    endelse
              endfor
              ;stop
           endif
;-==============================================================================


        
;+==============================================================================
;8. Calculate the energization of the particles at each crossing point
        
        for i=0,ncrosses-1 do begin
           fline=allcrossLineIndices[sstep,i]
           ;Get the particle and field information
           p0=reform(allPartMomentum[i]) ;current initial momentum
           pt=reform(allcrossPoints[sstep,*,i])   ;the point location
           ;Point radial distance from sun center, in Rs
        rmag=sqrt((pt[0]-xcenter)^2+(pt[1]-ycenter)^2+(pt[2]-zcenter)^2)/sunrad

        ;THIS IS THE NEW VERSION, USING PFSS LINEAR INTERPOLATION
        sphpt=cart2sph([pt[0]-xcenter,pt[1]-ycenter,pt[2]-zcenter]/sunrad)
        local_B=bfield_pfss(sphpt,sph_data)

        ;AND THIS IS THE OLD VERSION, USING GOPALSWAMY ET AL., 2011 MODEL
        ;local_B=bfield(rmag)   ;The local magnetic field, in Tesla

           rg=p0/(echarge*local_B) ;The particle's gyroradius, in meters
           thetabn=allcrossAngles[sstep,i] ;degrees
                                ;Calculate the diffusion coefficients
           kpar = reform(p0*mfp/(3*mp)) ;parallel diffusion coeff., in m^2/s
                                ;kperp = kpar*kperptokpar ;if choose a constant relationship
           kperp = kpar/(1+(rg/mfp)^2);*kperptokpar
           kxx = kperp*sin(thetabn)^2 + kpar*cos(thetabn)^2
           
           ;Finally, do the calculation here!
           dp=p0*(dt*vshock^2*(shockjump-1))/(3*shockjump*kxx)
                                ;Increase the energy on that field line by dp
                                ;NOTE: There should be some limit, like checking if the field
                                ;line is open or if the momentum has reached its final value.
           allPartMomentum[i] += dp
                                ;Alternatively, rearranging, we get
                                ;dp[ncr,0:nenrgs-1]= (vshock^2 * (shockjump-1) * mp * dt) / $
                                ;                    (shockjump * mfp * $
                                ;                     (sin(thetabn[ncr])^2/(1+(mfp/rg)^2) + $
                                ;                      cos(thetabn[ncr])^2))
           
           if sstep eq nsteps-1 then begin
              finalMomentumLineIndex=[finalMomentumLineIndex,fline]
              finalMomentum=[finalMomentum,allPartMomentum[i]]
           endif
        endfor
;-==============================================================================
        
        allcrossMomentum[sstep,0:ncrosses-1]=allPartMomentum;[where(fline eq allIndLines)]
                                ;sss=total(dp,1)
                                ;if sstep eq 0 then plot,gride,sss/pgrid,/ylog,yrange=[0.001,10000.],ystyle=1 $
                                ;else oplot,gride,sss/pgrid
        
        ;Save the PFSS/SHOCK/AIA image
        if jump eq 0 and keyword_set(png) then begin
           tvlct,rr,gg,bb,/get
           image=tvrd(true=1)
           stp=strtrim(string(sstep),2)
           if stp lt 100 then stp='0'+stp
           if stp lt 10 then stp='0'+stp
           write_png,pfsspath+'aia_pfss_shock_'+event.date+'_'+event.label+'_'+stp+'.png',image,rr,gg,bb
        endif
        
     endfor  ;END TIMESTEP LOOP
     

     
;CREATE A STRUCTURE TO HOLD THE RESULTS FOR EASY PROCESSING LATER
     nmaxcrosses=max(allcrosses)
     crossPoint={px:0.0D,py:0.0D,pz:0.0D,thbn:0.0D,linid:0L}
     CrossPoints=replicate(crossPoint,nsteps,nmaxcrosses)
     
     for sstep=0,nsteps-1 do begin
        ncrosses=allcrosses[sstep]
        for cross=0,ncrosses-1 do begin
           CrossPoints[sstep,cross].px=allcrosspoints[sstep,0,cross]
           CrossPoints[sstep,cross].py=allcrosspoints[sstep,1,cross]
           CrossPoints[sstep,cross].pz=allcrosspoints[sstep,2,cross]
           CrossPoints[sstep,cross].linid=allcrossLineIndices[sstep,cross]
           CrossPoints[sstep,cross].thbn=allcrossAngles[sstep,cross]
        endfor
     endfor



;Save the results to a file
     fname='model_shock_vsh_'+strtrim(string(vshock/1000.0,format='(f7.1)'),2)+'_r_'+$
           strtrim(string(shockjump,format='(f4.2)'),2)+'.sav'
     ;save,filename=pfsspath+fname,allcrossAngles,allcrossPoints,allcrosses,allcrossLineIndices,$
     ;     allcrossMomentum,dt,nsteps,shockjumps,subindex,radiusmoments,radiusfitlines,suncenter,$
     ;     vert_rotmat,vert_transmat,vertex_list,sc,finalMomentum,finalMomentumLineIndex,evnum
     save,filename=pfsspath+fname,allPartEnergy,allPartMomentum,allIndLines,allcrossAngles,vshock,$
          allcrossPoints,allcrosses,allcrossLineIndices,allcrossMomentum,dt,subindex,radius,time,$
          shockjumps,vert_rotmat,vert_transmat,vertex_list,suncenter,nsteps,sc,radiusfitlines,ind_arr
     
  endfor      ;END SHOCKJUMP LOOP
  
;+==============================================================================
;9. Plot all the crossing points on the shock surface
  
  ;pfss_shock_plot_crossing_angles,event,pfsspath+fname
;-==============================================================================
  

;+==============================================================================
;10. Plot the final energy histogram
  
     ;pfss_shock_plot_energy_histogram,event;,compression=shockjumps,vshock=vshock

;-==============================================================================     
end ; END AIA_MODEL_SHOCK_PFSS


pro pfss_shock_plot_energy_histogram,event,compression=compression,vshock=vshock
; A small procedure to run several instances of the coronal shock
; model.
  
  mp=1.67e-27                   ;proton mass in kg
  JinEV=1.6e-19                 ;conversion between eV and Joules
  
  if not keyword_set(vshock) then vshock=[400]     ;shock speeds, km/s. (400,800,1200)
  if not keyword_set(compression) then compression=[1.13,2,4]     ;shock compression ratios.
  mine=0.01                     ;minimum energy of protons, MeV
  binsiz=0.1             ;the energy bin size for the histograms, MeV
  
  nruns=n_elements(compression) 
  dts=fltarr(nruns)
  nparticles=intarr(nruns)
  
  shockspeed=strtrim(string(vshock,format='(f7.1)'),2)
  
  savepath=event.pfsspath
;Restore the data, make the histogram.
  for i=0,nruns-1 do begin
     comp=strtrim(string(compression[i],format='(f4.2)'),2)
     fname='model_shock_vsh_'+shockspeed+$
        '_r_'+comp+'.sav'
     print,''
     print,'Loading file '+fname
     print,''
     restore,savepath+fname
     dts[i]=dt[0]
     nparticles[i]=n_elements(allPartMomentum)
     if i eq 0 then begin
        nsteps=n_elements(allcrosses)
        enrgs=fltarr(nruns,nparticles[i])
     endif
     
     ;convert momentum to energy.
     enrg=(allPartMomentum/sqrt(mp*2))^2/JinEv
     enrgs[i,*]=enrg
     print,max(enrg)
     ;Find the overall minimum and maximum energy.
     if i eq 0 then minerg=min(enrg) else $
        if min(enrg) lt minerg then minerg=min(enrg)
     if i eq 0 then maxerg=max(enrg) else $
        if max(enrg) gt maxerg then maxerg=max(enrg)
  endfor
  
  
  ;Create the histograms
  nbins=fix((maxerg-mine)/binsiz)
  if nbins eq 0 then nbins=1
  bins=FINDGEN(nbins)*binsiz + mine
  histgrid=fltarr(nruns,nbins)
  for i=0,nruns-1 do begin
     hist=histogram(enrgs[i,*],binsize=binsiz,min=mine,nbins=nbins)
     histgrid[i,*]=hist
  endfor
  
  ;Create the initial histogram for comparison
  inithist=fltarr(nbins)
  inithist[where(bins eq mine)]=max(nparticles)
  
  ;Here plot the various histograms
  yrng=[min(histgrid[where(histgrid gt 0.0)])/2.,max(inithist)*1.5]
  !P.position=[0.14,0.12,0.96,0.95]
  !p.font=-1
  tvlct,rr,gg,bb,/get
  loadct,0,/silent
  wdef,0,800,800
  !P.charthick=2
  
  PLOT, bins,histgrid[0,*],$
        PSYM = 10, $ 
        TITLE = '!6Shock-accelerated proton energies', $
        XTITLE = '!6Particle energy [MeV]', $
        YTITLE = '!6# protons', $
        xrange=[mine,maxerg*1.1],yrange=yrng,$
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=3,ythick=3,thick=3,charsize=2.2,charthick=2,/nodata,/ylog,/xlog
 
  ;Plot the histogram with the initial energies
  oplot,bins,inithist,psym=10,color=0,thick=6
  

  loadct,13,/silent
  for i=0,nruns-1 do begin
     col=80+i*80
     oplot,bins,histgrid[i,*],$
           psym=10,$
           color=col,$
           thick=3
     ;Denote the maximum energies for each run
     plots,[max(enrgs[i,*]),max(enrgs[i,*])],[yrng[0],yrng[1]],$
           color=col,thick=4,linestyle=2
     ;Label the runs
     xyouts,!P.position[2]-0.5,!P.position[3]-0.1-i*0.04,$
            '!6V!Dshock!N = '+shockspeed+'!6 km/s; r = '+strtrim(string(compression[i]),2),$
            charsize=2.2,charthick=2,/normal,color=col
  endfor
  loadct,0,/silent
  axis,xaxis=0,/xlog,xthick=3,/data,color=0,xrange=[mine,maxerg],$
       xstyle=1,xtickname=replicate(' ',round(alog(maxerg-mine)))
  
  image=tvrd(true=1)
  write_png,savepath+'model_shock_acceleration.png',image,rr,gg,bb
  
end
