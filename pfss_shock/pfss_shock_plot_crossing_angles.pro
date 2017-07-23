pro test_pfss_shock_plot_crossing_angles
;Testing the shock crossing angles plotting procedure
  event=load_events_info(label='110607_01') ;110607_01
  pfss_shock_plot_crossing_angles,event,/hires,/force,/oplot
  ;pfss_shock_plot_crossing_angles,event,/lores,/force,/oplot
end

pro pfss_shock_plot_crossing_angles,event,infile=infile,oplot=oplot,hires=hires,lores=lores,pfssLines=pfssLines,force=force
;PURPOSE:
;Plot the crossing points on the polar projection of the shock
;surface with their color signifying the crossing angle.
;If I want to make a movie out of it, then plot this for every step
;with the same size dots. If it is to be presented as a single slide,
;make the size of the dots correspond to the number of points with
;that angle in that area of the projection.
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;       event - an event structure
;
;KEYWORDS:
;       oplot - if set, plot all crossing points/angles up to the current
;               timestep at every timestep. Otherwise, only plot the
;               new crossing points.
;       infile - full name of the file containing information from the
;                shock modeling procedure.
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;transform_volume, sym, 
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;Updated by Kamen Kozarev on 12/06/2013 - integrated with the event
;                                         framework
;Updated by Kamen Kozarev on 02/20/2014 - added the option to plot
;                                         instantaneous vs. cumulative
;                                         angles at each timestep
;                                         (keyword oplot). Also, made
;                                         infile an optional input.
;
  wav='193'
  set_plot,'x'
  
 ;if not keyword_set(infile) then infile=find_latest_file(event.mfhcpath+'csgs_results_*')
  infile=file_search(event.mfhcpath+event.csgs.lores.map_savename)
  if keyword_set(hires) then infile=file_search(event.mfhcpath+event.csgs.hires.map_savename)
  resolution='lores'
  if keyword_set(hires) then resolution='hires'
  if infile[0] eq '' then begin
     print,'The file to load is not properly set or does not exist. Quitting.'
     return
  endif
  
  print,''
  print,'Loading file '+infile
  print,''
  restore,infile
  strtime=subindex[*].date_obs
  for tt=0,n_elements(strtime)-1 do begin
     tmp=strjoin(strsplit(strtime[tt],'T',/extract),'!C')
     tmp=strjoin(strsplit(tmp,'-',/extract),'/')
     tmp2=strsplit(tmp,'.',/extract)
     strtime[tt]=tmp2[0]
  endfor
  
                                ;Load the Annulusplot analysis results
  wavefile=event.annuluspath+replace_string(event.annplot.analyzed.radial.avg_savename,'WAV',wav)
  print, 'Loading shock info file '+wavefile
  restore,wavefile
  
  ;Get the proper indices for the AIA subindex array based on actual times
  sp=rad_data.timefitrange[0]
  ep=rad_data.timefitrange[1]
  match2,rad_data.time[sp:ep].date_obs,subindex.date_obs,suba,tmp
  if suba[0] ne -1 then subindex=subindex[suba]
  
  sunrad=subindex[0].r_sun+10
  KMPX=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)
  minshockrad = min(radiusfitlines)/kmpx
  maxshockrad = max(radiusfitlines)/kmpx

  ;Figure out the proper scaling of the points to the normalized plot
  shockrad=radiusfitlines[nsteps-1]/kmpx
  shscale=maxshockrad/shockrad
  ncrosses=allcrosses[nsteps-1]
  cpsx=crossPoints[nsteps-1,0:ncrosses-1].px
  cpsy=crossPoints[nsteps-1,0:ncrosses-1].py
  cpsz=crossPoints[nsteps-1,0:ncrosses-1].pz
  
  pos=[cpsx,cpsy,cpsz]
  
  pos=transform_volume(pos,t3dmat=invert(vert_rotmat))
  pos=transform_volume(pos,t3dmat=invert(vert_transmat))
  pos=transform_volume(pos,scale=[shscale,shscale,shscale])
  
  maxcrossX=max(pos[0,*])
  maxcrossY=max(pos[1,*])
  maxcrossZ=max(pos[2,*])


  
  
  xcenter=suncenter.xcenter
  ycenter=suncenter.ycenter
  if not keyword_set(datapath) then datapath=event.mfhcpath
  
;Get the field line info from the PFSS model results
  if not keyword_set(pfssLines) then $
     if keyword_set(hires) then $
        pfss_get_field_line_info,event,pfssLines=pfssLines,/hires $
     else pfss_get_field_line_info,event,pfssLines=pfssLines,/lores
  
;DEBUG
;This simulates open field lines to help the plotting
;  nlins=n_elements(pfsslines)
;  tmpind=indgen(fix(nlins/10))*10
;  pfsslines[tmpind].open=1
;DEBUG

;---------------------------------------------------------------------------------------
;TRANSFORM THE SHOCK SURFACE POINTS
;---------------------------------------------------------------------------------------
  Vertex_List = transform_volume(vertex_list,t3dmat=invert(vert_rotmat))
  Vertex_List = transform_volume(vertex_list,t3dmat=invert(vert_transmat))
;---------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------
;PLOTTING SECTION
;---------------------------------------------------------------------------------------
;Calculate the levels for the color plotting.
  nlev=36
  th=crossPoints.thbn
  minn=min(th[where(th gt 0.0 and finite(th))])
  maxx=max(th[where(th gt 0.0 and finite(th))])
  levstep=abs(alog10(minn)-alog10(maxx))/nlev
  levels=minn*10^(findgen(nlev)*levstep)
  
  
                                ;figure out the x and y range for plotting the points.
                                ;xrng=[-maxshockrad*1.001,maxshockrad*1.001]
                                ;yrng=[-maxshockrad*1.001,maxshockrad*1.001]
  xrng=[-1.01,1.01]
  yrng=[-1.01,1.01]

  !P.position=[0.14,0.12,0.78,0.92]
  !P.charthick=2
  set_plot,'ps'
  ;device,set_resolution=[1000,800],SET_PIXEL_DEPTH=24,DECOMPOSED=0
  loadct,0,/silent
                                ;!P.background=0
  chsize=3
  closedsym=1
  opensym=6
  !p.background=255
  
;THE TIMESTEP LOOP!
  startstep=2
  for sstep=startstep,nsteps-1 do begin

     in=strtrim(string(sstep),2)
     if sstep lt 100 then in='0'+in
     if sstep lt 10 then in='0'+in
     timstring=strjoin(strsplit(strmid(subindex[sstep].date_obs,11,8),':',/extract))
     savefname=datapath+'thetabn_'+event.date+'_'+event.label+'_'+resolution+'_'+timstring
     if keyword_set(oplot) then savefname=datapath+'thetabn_'+event.date+'_'+event.label+'_'+resolution+'_oplot_'+timstring
     if file_test(savefname) and not keyword_set(force) then begin
        print,'Files '+'thetabn_'+event.date+'_'+event.label+'_'+resolution+'_*.png'
        print,'already exist. To overwrite, rerun with /force.'
        print,'----'
        break
     endif else begin

        device,file=savefname+'.eps',/inches,xsize=10.0,ysize=8.0,$
               /encaps,/color,/helvetica

        shockrad=radiusfitlines[sstep]/kmpx
        shscale=maxshockrad/shockrad
        ncrosses=allcrosses[sstep]
        cpsx=crossPoints[sstep,0:ncrosses-1].px
        cpsy=crossPoints[sstep,0:ncrosses-1].py
        cpsz=crossPoints[sstep,0:ncrosses-1].pz
        
        pos=[cpsx,cpsy,cpsz]
        
        pos=transform_volume(pos,t3dmat=invert(vert_rotmat))
        pos=transform_volume(pos,t3dmat=invert(vert_transmat))
        pos=transform_volume(pos,scale=[shscale,shscale,shscale])
        ;scale=[maxcrossX/shockrad,maxcrossY/shockrad,maxcrossZ/shockrad]
        
        flx=reform(pos[0,*])/maxcrossX
        fly=reform(pos[1,*])/maxcrossY
        
        th=reform(crossPoints[sstep,0:ncrosses-1].thbn)
        
                                ;Plot Axes and the shock mesh
        loadct,0,/silent
        thlet='!9' + String("161B) + '!X'
        ;For x and z buffers, use for Theta
        ;'!4' + String("110B) + '!X'
        
        PLOT, flx/maxshockrad, fly/maxshockrad, PSYM = 3, $ 
              TITLE = thlet+'!DBN!N at B-Shock Crossings', $
              XTITLE = 'X', $
              YTITLE = 'Y', $
              xrange=xrng,yrange=yrng,$
              xstyle=1,ystyle=1,color=0,background=255,$
              xthick=3,ythick=3,thick=3,charsize=chsize,/nodata
        PLOTS,vertex_list[0,*]/maxshockrad,vertex_list[1,*]/maxshockrad,psym=3,color=0 ,/data

;Save the !X and !Y global variables, since fcolorbar modifies them...
        if sstep eq startstep then begin
           saveX=!X
           saveY=!Y
        endif

        ;if keyword_set(oplot) and sstep gt 0 then begin
        ;   xyouts,0.8,0.9,strtime[sstep-1],charsize=chsize-1,charthick=2,/norm,color=255
        ;endif
        xyouts,0.8,0.9,strtime[sstep],charsize=chsize-1,charthick=2,/norm,color=0
        
        ;Do the plotting of all previous steps
        if keyword_set(oplot) then begin
           for substep=startstep,sstep-1 do begin
              subshockrad=radiusfitlines[substep]/kmpx
              subshscale=maxshockrad/subshockrad
              subncrosses=allcrosses[substep]
              subcpsx=crossPoints[substep,0:subncrosses-1].px
              subcpsy=crossPoints[substep,0:subncrosses-1].py
              subcpsz=crossPoints[substep,0:subncrosses-1].pz
              
              subpos=[subcpsx,subcpsy,subcpsz]
              
              subpos=transform_volume(subpos,t3dmat=invert(vert_rotmat))
              subpos=transform_volume(subpos,t3dmat=invert(vert_transmat))
              subpos=transform_volume(subpos,$
                                      scale=[subshscale,subshscale,subshscale])
              ;scale=[maxcrossX/subshockrad,maxcrossY/subshockrad,maxcrossZ/subshockrad])
              
              subflx=reform(subpos[0,*])/maxcrossX
              subfly=reform(subpos[1,*])/maxcrossY
              subth=reform(crossPoints[substep,0:subncrosses-1].thbn)
              loadct,13,/silent
              
               ;If the field line is open, plot an open circle, else a filled circle
               symind=reform(pfsslines[crossPoints[substep,0:subncrosses-1].linid].open)
               openind=where(symind eq 1)
               if openind[0] ne -1 then symind[openind]=opensym
               closedind=where(symind eq 0)
               if closedind[0] ne -1 then symind[closedind]=closedsym
               symind=reform(symind)  
               
               ;Plot the closed field line crossings
               if closedind[0] ne -1 then begin
                  PLOTS,subflx[closedind],subfly[closedind],psym=sym(closedsym),$
                        symsize=1.8,color=(subth[closedind]-minn)/(maxx-minn)*255.,/data     
               endif
               
               ;Plot the open field line crossings
               if openind[0] ne -1 then begin
                  PLOTS,subflx[openind],subfly[openind],psym=sym(closedsym),$
                        symsize=1.8,color=(subth[openind]-minn)/(maxx-minn)*255.,/data    
                  loadct,0,/silent
                  PLOTS,subflx[openind],subfly[openind],psym=sym(closedsym),$
                        symsize=1.2,color=0,/data
                  PLOTS,subflx[openind],subfly[openind],psym=sym(closedsym),$
                        symsize=0.8,color=255,/data
                  
               endif
            endfor
        endif
        
        ;Contour data on regular grid
        loadct,13,/silent
        
        ;If the field line is open, plot an open circle, else a filled circle
        symind=reform(pfsslines[crossPoints[sstep,0:ncrosses-1].linid].open)
        openind=where(symind eq 1)
        if openind[0] ne -1 then symind[openind]=opensym
        closedind=where(symind eq 0)
        if closedind[0] ne -1 then symind[closedind]=closedsym
        symind=reform(symind)  
        
        ;Plot the closed field line crossings
        if closedind[0] ne -1 then begin
           PLOTS,flx[closedind],fly[closedind],psym=sym(closedsym),$
                 symsize=1.8,color=(th[closedind]-minn)/(maxx-minn)*255.,/data     
        endif
        
        ;Plot the open field line crossings
        if openind[0] ne -1 then begin
           PLOTS,flx[openind],fly[openind],psym=sym(closedsym),$
                 symsize=1.8,color=(th[openind]-minn)/(maxx-minn)*255.,/data    
           loadct,0,/silent
           PLOTS,flx[openind],fly[openind],psym=sym(closedsym),$
                 symsize=1.2,color=0,/data
           PLOTS,flx[openind],fly[openind],psym=sym(closedsym),$
                 symsize=0.8,color=255,/data
           loadct,13,/silent
        endif
        

        fcolorbar, MIN=minn,MAX=maxx,Divisions=4, $
                   Color=0,VERTICAL=1,RIGHT=1, TITLE=thlet+'!DBN!N [deg]',$
                   CHARSIZE=chsize, charthick=2, format='(i)',Position=[0.88, 0.3, 0.91, 0.7]
        
        !X=saveX
        !Y=saveY
        
        if sstep lt nsteps then begin
             device,/close
             exec='convert -flatten '+savefname+'.eps '+savefname+'.png ; rm -rf '+savefname+'.eps'
             spawn,exec
          endif
        print,'Saving image '+in
     endelse
  endfor
  loadct,0,/silent
  set_plot,'x'
;---------------------------------------------------------------------------------------
end
