pro test_model_coronal_shock
;Test the shock modeling procedure

  path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
  ev='test'
  shockfile=path+ev+'/AIA_20110511_110511_01_193_shocklocations.sav'
  restore,shockfile
  
  pfssfile=path+ev+'/pfss_results_20110511_1.1Rs_dens_1.sav'

  model_coronal_shock,shockfile,pfssfile
  stop
end
;-==========================================================================



function bfield,rmag
;Return field as a function of radius from Gopalswamy (2011?) paper
return,0.409*rmag^(-1.3) * 1.0e-4
end
;-==========================================================================



pro model_coronal_shock,shockfile,pfssfile,vupstream=vupstream,shockcomp=shockcomp
;PURPOSE:
; Run the coronal shock model. This procedure needs to be revisited.
;
;CATEGORY:
; PFSS_Shock
;
;INPUTS:
; shockfile - position of the shock wave in time from the kinematics measurements
; pfssfile - the pfss model for the given time
;
;KEYWORDS:
; vupstream - optional upstream plasma speed
; shockcomp - optional shock compression
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;pfss_sphtocart, transform_volume, bfield (this file)
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev,  
;
;Kamen Kozarev 11/08/2011

;+==================================================================================
;LOAD DATA
  print,''
  print,'Loading data...'
  
  restore, shockfile
  ;get some relevant info from the file name
  res=strsplit(shockfile,'_',/extract)
  date=res[1]
  event=res[2]
  wav=res[3]
  
  ;Calculate the number of steps and their size.
  nsteps=fix((maxshockrad-minshockrad)/(shockthick*1.01))
  dt=(maxshockrad-minshockrad)*mpix/(nsteps*1.0)/vshock ;timestep in seconds
  
  stop

  restore, pfssfile
  ;get relevant info from file name
  res=strsplit(pfssfile,'_',/extract)
  lowr=res[3]
  
  
;if not keyword_set(files) then begin
;Figure out the name of the local machine.
;  pcname=hostname()

;for sloncho-2
;  if pcname eq 'sloncho-2' then begin
;restore,'/Users/kkozarev/AIA/algoTests/yaftawave/normalized_AIA_20110125_05_193_subdata.sav'
     ;restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs_dens_2.sav'
;     restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs.sav'
;  endif else begin
     
;for arien
;     if pcname eq 'arien' then begin
;        datapath='/Volumes/Backscratch/Users/kkozarev/AIA/events/05/'
        ;restore,datapath+'normalized_AIA_20110125_05_193_subdata.sav'
;        shockfile=datapath+'AIA_20110125_05_193_shocklocations.sav'
;        pfssfile=datapath+'pfss_results_20110125_1.0Rs.sav'
;     endif else begin
        ;restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.0Rs_dens_1.sav'
        ;restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs_dens_2.sav'
;        restore,'/Users/kkozarev/AIA/pfss_results_20110125_1.1Rs.sav'
;     endelse
;  endelse
;endif else begin
;   shockfile=files[0]
;   pfssfile=
;endelse
;-==================================================================================



;+==================================================================================
;Constants and definitions
  loadct,8,/silent
  winsize=800
  xcenter=winsize/2.0
  ycenter=winsize/2.0
  zcenter=winsize/2.0
  sunrad=0.26*winsize
  minshockrad=0.1*sunrad
  maxshockrad=0.7*sunrad  

;Physical constants
  rsun_m=6.955e8 ;Solar radius in m.
  minAU=1.49598e11 ;m in AU
  mp=1.67e-27 ;proton mass in kg
  JinEV=1.6e-19 ;conversion between eV and Joules
  kboltz=1.38e-23 ;Boltzmann constant in SI
  echarge=1.602e-19 ;Electron charge in Coulomb
  mpix=rsun_m/sunrad ;conversion between pixels and m.

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;Physical parameters

  ;A. Solar Corona
  ;Need a 1D (spherically symmetric)
  ;model for the B-field.
  ;Use the one from Gopalswamy (2011) - B(r)=pr^(-q),p=0.409,q=1.30
  ;and wind speed
  ;as a function of radial distance.
  ;The solar coronal temperature, in Kelvin
  Temp=8000
  ;The ratio of the perpendicular to parallel diffusion.
  kperptokpar=0.01
  
  ;B. Shock
  if not keyword_set(vupstream) then vshock=400 * 1000.0 $  ;upstream plasma speed in the shock frame, m/s
  else vshock=vupstream * 1000.0
  if not keyword_set(shockcomp) then shockjump=1.1 $    ;shock jump, r (bigger than 1)
  else shockjump=shockcomp
  shockthick=0.4                ;shock thickness, in pixels


  ;C. Particles
  mfp=0.01 * minAU ;assume some parallel mean free path, in m.
  mine=0.01 ;minimum energy of protons, MeV
  maxe=100.0 ;maximum energy of protons, MeV; protons will escape at this energy.
  ;;;;;;;;;;;;;;;;;;;;;;;

  
  ;Create the proton momentum grid
  nenrgs=100
  gride=dblarr(nenrgs) ;The energy grid in eV
  ;Use a scheme in which energy bins are created exponentially. It covers the energy
  ;range better. In that scheme, E[i]=E0*((E1/E0)^(i/Nergs))
  emin=mine * JinEV
  emax=maxe * JinEV

  ;Create a momentum grid
  for i=0,nenrgs-1 do gride[i]=mine*(maxe/mine)^(i/((nenrgs-1)*1.0))
  egrid=gride * JinEV ;The energy grid in Joules
  pgrid = sqrt(mp *2) * sqrt(gride*JinEV)

  ;Calculate the Maxwellian momentum distribution
  ;for these energies and split it into particles.
  aa=(mp/(2*!PI*kboltz*Temp))^1.5
  bb=4*!PI*(pgrid/mp)^2
  cc=(pgrid/sqrt(mp))^2/(2*kboltz*Temp)
  initdist= aa * bb * exp(-cc)


  ;Get some proxy for the number of particles
  initparticles=initdist/min(initdist)


;Save the crossing points information
  allcrossPoints=fltarr(nsteps,3,10000)
  allcrossAngles=fltarr(nsteps,10000)
  allcrossLineIndices=intarr(nsteps,10000)
  allcrosses=fltarr(nsteps)
  allcrossMomentum=fltarr(nsteps,50000)
;-==================================================================================


;+==================================================================================
;Loop over all the steps ;THIS IS THE MAIN LOOP!
  for sstep=0,nsteps-1 do begin
     print,'Step #'+string(sstep)
     ;calculate the shock radius
     shockrad=minshockrad+((maxshockrad-minshockrad)*sstep)/(nsteps-1)
     
;Rotation angles for the entire plot
     xrot_gen=(sstep*0.0)/nsteps
     yrot_gen=(sstep*0.0)/nsteps
     zrot_gen=(sstep*0.0)/nsteps
     
;Rotation angles for the PFSS points
     xrot_pfss=0+xrot_gen
     yrot_pfss=0+yrot_gen
     zrot_pfss=0+zrot_gen
     
;Latitude and longitude for the shock location
     lat=0.0
     lon=0.0
     
;Rotation angles for the shock surface points
     xrot_shock=-lat+xrot_gen ;-lat+xrot_gen
     yrot_shock=lon+yrot_gen
     zrot_shock=0+zrot_gen
     
  
     
;+==============================================================================
;1. Plot the field lines on disk center.
     if sstep eq 0 then begin
;Convert the spherical to x,y,z coordinates. 
;Switch y and z axes to make the coordinate system right-handed.
        pfss_sphtocart,ptr,ptth,ptph,l,b,pfss_px,pfss_pz,pfss_py
        nlines=n_elements(pfss_px[0,*])
        maxnpts=n_elements(pfss_px[*,0])  
        
        
;Apply the rotations and translations and plot
        pfss_cartpos=fltarr(nlines,3,maxnpts)
        for ff=0.0,nlines-1 do begin
           npt=nstep[ff]        ;the number of points in this particular line.
           pos = transpose([[reform(pfss_px[0:npt-1,ff])],$
                            [reform(pfss_py[0:npt-1,ff])],$
                            [reform(pfss_pz[0:npt-1,ff])]])
           pos = transform_volume(pos,rotation=[xrot_pfss,yrot_pfss,zrot_pfss],$
                                  scale=[sunrad,sunrad,sunrad])
           pos = transform_volume(pos,translate=[xcenter,ycenter,zcenter])
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
     Vertex_List = transform_volume(vertex_list,$
                                    rotation=[xrot_shock,yrot_shock,zrot_shock],$
                                    centre_rotation=[xcenter,ycenter,zcenter])
     
     ;Initial coordinates of the center of the shock surface.
     sc = transform_volume([0,0,0],translate=[xcenter,ycenter,zcenter+sunrad])
     sc = transform_volume(sc,rotation=[xrot_shock,yrot_shock,zrot_shock],$
                           centre_rotation=[xcenter,ycenter,zcenter])
     nverts=n_elements(vertex_list[0,*])
;-============================================================================== 


;+==============================================================================
;4. Determine the points where the field lines cross the shock
;surface of thickness shockthick
     dr2=(reform(pfss_cartpos[*,0,*])-sc[0])^2 + $
         (reform(pfss_cartpos[*,1,*])-sc[1])^2 + $
         (reform(pfss_cartpos[*,2,*])-sc[2])^2
     ptind=where(dr2 le (shockrad+shockthick)^2 and dr2 ge shockrad^2)
     
     if ptind[0] gt -1 then begin
        pind=array_indices(reform(pfss_cartpos[*,0,*]),ptind)
        ncrosses=n_elements(pind[0,*])
     endif else begin
        print,'No field/shock crossings. Continuing...'
        continue
     endelse

     cross_points=fltarr(3,ncrosses)
     for i=0,ncrosses-1 do cross_points[*,i]=reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
     allcrossPoints[sstep,*,0:ncrosses-1]=cross_points
     
     ;find the normals at the crossing
     ;points and the local field direction.
     ;Compute ThetaBN.
     normals=fltarr(3,ncrosses)
     local_field=fltarr(3,ncrosses)
     
     
     ;find the normals
     for i=0,2 do normals[i,*] = cross_points[i,*] - sc[i]
     ;find the local field direction
     for i=0,ncrosses-1 do $
        local_field[*,i] = reform(pfss_cartpos[pind[0,i],*,pind[1,i]+1]) - $
        reform(pfss_cartpos[pind[0,i],*,pind[1,i]])
     
     ;compute the dot products
     dotp=normals[0,*]*local_field[0,*] + $
          normals[1,*]*local_field[1,*] + $
          normals[2,*]*local_field[2,*]
     dotp=reform(dotp)
     
     ;The angles are acos((a dot b)/(|a|*|b|))
     prod=total(abs(normals),1) * total(abs(local_field),1)
     thetabn=acos(dotp/prod)
     ind=where(thetabn gt !PI/2.0)
     if ind[0] gt -1 then thetabn[ind]=!PI-thetabn[ind]
     th=thetabn*180/!PI
     allcrossLineIndices[sstep,0:ncrosses-1]=pind[1,*]
     allcrossAngles[sstep,0:ncrosses-1]=th
     allcrosses[sstep]=ncrosses
;-==============================================================================


;+==============================================================================
;5. Find the unique crossings on each line - take the
;point on each line that is farthest upstream.
     rmags = reform(sqrt(allcrossPoints[sstep,0,0:ncrosses-1]^2 + $
                  allcrossPoints[sstep,1,0:ncrosses-1]^2 + $
                  allcrossPoints[sstep,2,0:ncrosses-1]^2))
     
     inds=intarr(ncrosses)
     inds[0]=0
     nn=0
     ;find the indices of the unique crossings.
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
     
;Then update the crossing points information
     allcrossLineIndices[sstep,0:ncrosses-1]=reform(pind[1,inds])
     allcrossPoints[sstep,*,0:ncrosses-1]=allcrossPoints[sstep,*,inds]
     allcrossAngles[sstep,0:ncrosses-1]=allcrossAngles[sstep,inds]
     allcrosses[sstep]=ncrosses

     ;Go over the field lines and add
     ;particles if there are newly-crossing
     ;field lines
     newLines=reform(pind[1,inds])
     cc=0
     diffind=-1
     if sstep eq 0 then begin
        allIndLines=newLines
        allPartMomentum=fltarr(n_elements(newLines))+pgrid[0]
        endif else begin
        for i=0,ncrosses-1 do begin
           tmp=where(newLines[i] eq allIndLines)
           if tmp eq -1 then begin
              if cc eq 0 then diffind = i else $
                 diffind=[diffind,i]
              cc++
           endif
        endfor
        if diffind[0] gt -1 then begin
           allIndLines=[allIndLines,newLines[diffind]]
           allPartMomentum=[allPartMomentum,fltarr(n_elements(diffind))+pgrid[0]]
     ;Order the field line positions in the arrays.
     ;Don't forget to reorder the energies as well!
           srt=sort(allIndLines)
           allIndLines=allIndLines[srt]
           allPartMomentum=allPartMomentum[srt]
        endif
     endelse
     ;print,allIndLines
     ;stop
;-==============================================================================


;+==============================================================================
;6. Calculate the energization of the particles at each crossing point

     for ncr=0,ncrosses-1 do begin
        fline=allcrossLineIndices[sstep,ncr]
        ;print,where(fline eq allIndLines)
        
        ;Get the particle and field information
        p0=allPartMomentum[where(fline eq allIndLines)] ;current initial momentum
        pt=allcrossPoints[sstep,*,ncr] ;the point location
        rmag=sqrt((pt[0]-xcenter)^2+(pt[1]-ycenter)^2+(pt[2]-zcenter)^2)/sunrad
        local_B=bfield(rmag) ;The local magnetic field
        rg=pgrid/(echarge*local_B) ;The particle's gyroradius
        thetabn=allcrossAngles[sstep,ncr]
        ;Calculate the diffusion coefficients
        kpar = p0*mfp/(3*mp)
        ;kperp = kpar*kperptokpar ;if choose a constant relationship
        kperp = kpar/(1+(rg/mfp)^2)
        kxx = kperp*sin(thetabn)^2 + kpar*cos(thetabn)^2
        
        ;Finally, do the calculation here!
        dp=(p0*dt*vshock^2*(shockjump-1))/(3*shockjump*kxx)
        
        ;Increase the energy on that field line by dp
        allPartMomentum[where(fline eq allIndLines)] += dp
        ;Alternatively, rearranging, we get
        ;dp[ncr,0:nenrgs-1]= (vshock^2 * (shockjump-1) * mp * dt) / $
        ;                    (shockjump * mfp * $
        ;                     (sin(thetabn[ncr])^2/(1+(mfp/rg)^2) + $
        ;                      cos(thetabn[ncr])^2))
     endfor
;-==============================================================================
     allcrossMomentum[sstep,0:ncrosses-1]=allPartMomentum[where(fline eq allIndLines)]
     ;sss=total(dp,1)
     ;if sstep eq 0 then plot,gride,sss/pgrid,/ylog,yrange=[0.001,10000.],ystyle=1 $
     ;else oplot,gride,sss/pgrid     

  endfor

;+==============================================================================
;6. Plot the histogram of all angles at all steps.
;binsiz=5

;for sstep=0,nsteps-1 do begin
;   th=allcrossangles[sstep,0:allcrosses[sstep]-1]
;   hist=histogram(th,binsize=binsiz)
;   bins = FINDGEN(N_ELEMENTS(hist))*binsiz + MIN(th)
;   if sstep eq 0 then PLOT, bins, hist, YRANGE = [10, 500], PSYM = 10, $ 
;                        XTITLE = 'Theta!DBn!N', YTITLE = 'Density per Bin',$
;                        XRANGE= [min(th),max(th)+1],xstyle=1,ystyle=1 $
;   else oplot,bins,hist,psym=10
;endfor
;-==============================================================================
  
;convert momentum to energy
  allPartEnergy=(allPartMomentum/sqrt(mp*2))^2/JinEv

;Save the results to a file
  fname='/model_shock_vsh_'+strtrim(string(vshock/1000.0,format='(f7.1)'),2)+$
        '_r_'+strtrim(string(shockjump,format='(f4.1)'),2)+'.sav'
  save,filename=fname,allPartEnergy,allPartMomentum,allIndLines,allcrossAngles,$
       allcrossPoints,allcrosses,allcrossLineIndices,allcrossMomentum,dt
end




pro rungrid_model_shock
; A small procedure to run several instances of the coronal shock
; model.

  mp=1.67e-27                   ;proton mass in kg
  JinEV=1.6e-19                 ;conversion between eV and Joules

  shockspeed=[400,800,1200]     ;shock speeds, km/s.
  compression=[1.2,2,4]     ;shock compression ratios.
  mine=0.01                     ;minimum energy of protons, MeV
  binsiz=0.1             ;the energy bin size for the histograms, MeV

  nruns=n_elements(shockspeed)
  dts=fltarr(nruns)
  nparticles=intarr(nruns)
  
  ;Run the model here!
  ;for i=0,nrums-1 do model_coronal_shock,shockv=shockspeed[i],shockcomp=compression[i]

  shockspeed=strtrim(string(shockspeed,format='(f7.1)'),2)
  compression=strtrim(string(compression,format='(f4.1)'),2)

;Restore the data, make the histogram.
  for i=0,nruns-1 do begin
     fname='model_shock_vsh_'+shockspeed[i]+$
        '_r_'+compression[i]+'.sav'
     print,''
     print,'Loading file '+fname
     print,''
     restore,fname
     dts[i]=dt
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
  !P.position=[0.12,0.13,0.94,0.93]
  !p.font=-1
  tvlct,rr,gg,bb,/get
  loadct,0,/silent
  wdef,0,1200,800
  !P.charthick=2

  PLOT, bins,histgrid[0,*],$
        PSYM = 10, $ 
        TITLE = '!6Shock-accelerated proton energies', $
        XTITLE = '!6Particle energy [MeV]', $
        YTITLE = '!6# protons', $
        xrange=[mine,maxerg*1.1],yrange=yrng,$
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=3,ythick=3,thick=3,charsize=2.4,charthick=2,/nodata,/ylog,/xlog
 
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
     xyouts,!P.position[2]-0.4,!P.position[3]-0.1-i*0.04,$
            '!6V!Dshock!N = '+shockspeed[i]+'!6 km/s; r = '+compression[i],$
            charsize=2.4,charthick=2,/normal,color=col
  endfor
  loadct,0,/silent
  axis,xaxis=0,/xlog,xthick=3,/data,color=0,xrange=[mine,maxerg],$
       xstyle=1,xtickname=replicate(' ',round(alog(maxerg-mine)))
  
  image=tvrd(true=1)
  write_png,'model_shock_acceleration.png',image,rr,gg,bb
  
end

