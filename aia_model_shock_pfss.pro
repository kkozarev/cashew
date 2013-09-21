function bfield,rmag
;Return field as a function of radius from Gopalswamy paper
return,0.409*rmag^(-1.3) * 1.0e-4 ;Field magnitude in Tesla
end

pro aia_model_shock_pfss,evindex,wav,shockcomp=shockcomp,plot=plot,png=png
;This procedure runs the pfss/shock model for estimating shock
;orientation to magnetic fields, and 
;Kamen Kozarev 01/15/2012
;Last Updated: 07/13/2012 by KAK
resolve_routine,'sym',/either,/compile_full_file
;--------------------------------------------------------------
;LOAD DATA
  print,''
  print,'Loading data...'
  
  wavelength=['171','193','211','335','094','131','304']
  evnums=['05','06','07','13','19','20','23','32','37','38','41','113']
  sts=['2011/01/25 11:56:00','2011/01/27 11:50:00','2011/01/28 00:45:00',$
       '2011/02/11 12:30:00','2011/03/07 19:35:00','2011/03/08 03:30:00',$
       '2011/03/12 15:20:00','2011/04/27 02:05:00','2011/05/11 02:10:00',$
       '2011/05/29 10:00:00','2012/01/05 06:56:00','2010/06/13 05:35:00']
  evnum=evnums[evindex]
  std=strsplit(sts[evindex],'/ :',/extract)
  ;print,std
  date=std[0]+std[1]+std[2]
  eventname='AIA_'+date+'_'+evnum+'_'+wav
  
;Figure out the name of the local machine.
  pcname=hostname()
  
  
;for sloncho-2
  if pcname eq 'sloncho-2' then begin
;restore,'/Users/kkozarev/AIA/algoTests/yaftawave/normalized_'+eventname+'_subdata.sav'
     ;restore,'/Users/kkozarev/AIA/pfss_results_'+date+'_1.1Rs_dens_2.sav'
     restore,'/Users/kkozarev/AIA/pfss_results_'+date+'_1.1Rs.sav'
  endif else begin
     
;for arien
     if pcname eq 'arien' then begin
        datapath='/Volumes/PLUME/AIA_data/studies/2011events/e'+evnum+'/'
        ;goto,next
        print,'Loading AIA File '+datapath+'normalized_'+eventname+'_subdata.sav'
        restore,datapath+'normalized_'+eventname+'_subdata.sav'
        
        ;Load the measured shock wave radius.
        ;This was created with measure_wave_sphere.pro
        print, 'Loading shock info file '+datapath+eventname+'_shocklocations.sav'
        restore,datapath+eventname+'_shocklocations.sav'
        
        ;Load the PFSS model results
        print,'Loading PFSS File '+datapath+'pfss_results_'+date+'_1.1Rs_dens_1.sav'
        restore,datapath+'pfss_results_'+date+'_1.1Rs_dens_1.sav'
        
     endif else begin
        ;restore,'/Users/kkozarev/AIA/pfss_results_'+date+'_1.0Rs_dens_1.sav'
        ;restore,'/Users/kkozarev/AIA/pfss_results_'+date+'_1.1Rs_dens_2.sav'
        restore,'/Users/kkozarev/AIA/pfss_results_'+date+'_1.1Rs.sav'
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
  minshockrad=radiusfitlines[0]/kmpx
  maxshockrad=radiusfitlines[nprofs-1]/kmpx
  
;Physical constants
  rsun_m=6.955e8 ;Solar radius in m.
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
  nshockjumps=n_elements(shockjumps)
  
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
  nsteps=nprofs
  arcoords=[-20,-70]
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
        shockrad=radiusfitlines[sstep]/kmpx ;Get this from the measurements
        vshock=radiusmoments[sstep,1]*1000.0 ;Shock speed in m/s
        
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
;Latitude and longitude for the shock location
                                ;lat=subindex[0].arlat
                                ;lon=subindex[0].arlon
        
        ;This is a hack for June 13, since the WCS functions crap out
        lat=-24.0
        lon=84.0
        subindex[sstep].arlat=lat
        subindex[sstep].arlon=lon
        
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

           
;Apply the rotations and translations and plot
           pfss_cartpos=fltarr(nlines,3,maxnpts)
           for ff=0.0D,nlines-1 do begin
              
              npt=nstep[ff]     ;the number of points in this particular line.
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
              tv,bytscl(sqrt(subdata[*,*,sstep]),min=1,max=50)
              
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
              plots,vertex_list,color=0,thick=0.1,/device
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





;NB - THE UNIQUE CROSSINGS WERE FOUND IN THE PREVIOUS PART ALREADY -
;     THIS PART IS WRONG AND REDUNDANT!
findunq=0
if findunq eq 1 then begin
;+==============================================================================
;5. Find the unique crossings on each line - take the
;point on each line that is farthest upstream in the shock region
           rmags = reform(sqrt(allcrossPoints[sstep,0,0:ncrosses-1]^2 + $
                            allcrossPoints[sstep,1,0:ncrosses-1]^2 + $
                               allcrossPoints[sstep,2,0:ncrosses-1]^2))
        inds=intarr(ncrosses)
        inds[0]=0
        nn=0
        
        ;find the indices of the unique crossings.
        ;pind[1,*] is the index of the points.
        for i=1,ncrosses-1 do begin
           if pind[1,i] ne pind[1,i-1] then begin
              nn++
              inds[nn]=i
           endif else begin
              if rmags[i] gt rmags[i-1] then inds[nn]=i $
              else continue
           endelse
        endfor
        ncrosses=nn+1
        inds=inds[0:ncrosses-1]

        
; Update the crossing points information
        allcrossLineIndices[sstep,0:ncrosses-1]=reform(pind[0,inds])
        allcrossPoints[sstep,*,0:ncrosses-1]=allcrossPoints[sstep,*,inds]
        allcrossAngles[sstep,0:ncrosses-1]=allcrossAngles[sstep,inds]
 
;-==============================================================================
endif
        



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
           local_B=bfield(rmag)    ;The local magnetic field, in Tesla
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
           write_png,datapath+'AIA_PFSS_SHOCK_'+stp+'.png',image,rr,gg,bb
        endif
        
     endfor  ;END TIMESTEP LOOP
     

;Save the results to a file
     fname='aia_shock_pfss_r'+$
           strtrim(string(shockjump,format='(f4.1)'),2)+'.sav'
     save,filename=datapath+fname,allcrossAngles,allcrossPoints,allcrosses,allcrossLineIndices,$
          allcrossMomentum,dt,nsteps,shockjumps,subindex,radiusmoments,radiusfitlines,suncenter,$
          vert_rotmat,vert_transmat,vertex_list,sc,finalMomentum,finalMomentumLineIndex,evnum
     
  endfor      ;END SHOCKJUMP LOOP

;+==============================================================================
;9. Plot all the crossing points on the shock surface
next:
  fname='aia_shock_pfss_r2.0.sav'
  restore,datapath+fname
  plot_pfss_shock_crossing_angles,allcrossPoints,nsteps,allcrosses,radiusfitlines,$
                                  vertex_list,allcrossangles,subindex,suncenter,$
                                  vert_rotmat,vert_transmat,sc,datapath=datapath
;-==============================================================================
  

;+==============================================================================
;10. Plot the final energy histogram

     plot_pfss_shock_energy_histogram,shockjumps=shockcomp,datapath=datapath

;-==============================================================================   
  
end ; END AIA_MODEL_SHOCK_PFSS





;---------------------------------------------------------------------
pro rungrid_aia_shock_pfss_model
; A small procedure to run several instances of the coronal shock
; model.
  datapath='/Volumes/PLUME/AIA_data/studies/2011events/e113/'
  
  compression=[1.12,2,4]     ;shock compression ratios.
;  compression=4
  
  nruns=n_elements(compression)
  dts=fltarr(nruns)
  nparticles=intarr(nruns)
  
  ;Run the model here for all shock jump cases!
  aia_model_shock_pfss,11,'193',shockcomp=compression;,/png

  
end

