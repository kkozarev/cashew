pro test_pfss_shock_plot_crossing_angles
;Testing the shock crossing angles plotting procedure
event=load_events_info(label='paper')

pfss_shock_plot_crossing_angles,event,/hires;,/oplot

end

pro pfss_shock_plot_crossing_angles,event,infile=infile,oplot=oplot,hires=hires,lores=lores,pfssLines=pfssLines
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

  savepath=event.pfsspath
  ;if not keyword_set(infile) then infile=find_latest_file(event.pfsspath+'csgs_results_*')
  infile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_lores.sav')
  if keyword_set(hires) then infile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_hires.sav')
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
  
  sunrad=subindex[0].r_sun+10
  KMPX=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)
  minshockrad = radiusfitlines[0]/kmpx
  maxshockrad = radiusfitlines[nsteps-1]/kmpx
  xcenter=suncenter[0]
  ycenter=suncenter[1]
  if not keyword_set(datapath) then datapath=savepath
  
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
  thlet='!4' + String("110B) + '!X'
  
  ;figure out the x and y range for plotting the points.
  ;xrng=[-maxshockrad*1.001,maxshockrad*1.001]
  ;yrng=[-maxshockrad*1.001,maxshockrad*1.001]
  xrng=[-1.01,1.01]
  yrng=[-1.01,1.01]

  !P.position=[0.16,0.14,0.78,0.92]
  !P.charthick=2
  set_plot,'z'
  device,set_resolution=[1000,800],SET_PIXEL_DEPTH=24,DECOMPOSED=0
  ;!P.background=0
  chsize=2

;THE TIMESTEP LOOP!
  for sstep=0,nsteps-1 do begin
     
     in=strtrim(string(sstep),2)
     if sstep lt 100 then in='0'+in
     if sstep lt 10 then in='0'+in
     savefname=datapath+'thetabn_'+event.date+'_'+event.label+'_'+resolution+'_'+in+'.png'
     if keyword_set(oplot) then savefname=datapath+'thetabn_'+event.date+'_'+event.label+'_'+resolution+'_oplot_'+in+'.png'
      if file_test(savefname) and not keyword_set(force) then begin
        print,'Files '+'thetabn_'+event.date+'_'+event.label+'_'+resolution+'_*.png'
        print,'already exist. To overwrite, rerun with /force.'
        print,'----'
        break
     endif else begin

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

     flx=reform(pos[0,*])
     fly=reform(pos[1,*])
     
     th=reform(crossPoints[sstep,0:ncrosses-1].thbn)
     
     ;Plot Axes and the shock mesh
     loadct,0,/silent
     
     if keyword_set(oplot) then begin 
        if sstep eq 0 then begin
           PLOT, flx/maxshockrad, fly/maxshockrad, PSYM = 3, $ 
                 TITLE = thlet+'!DBN!N at B-Shock Crossings', $
                 XTITLE = 'X', $
                 YTITLE = 'Y', $
                 xrange=xrng,yrange=yrng,$
                 xstyle=1,ystyle=1,color=0,background=255,$
                 xthick=3,ythick=3,thick=3,charsize=chsize,/nodata
           PLOTS,vertex_list[0,*]/maxshockrad,vertex_list[1,*]/maxshockrad,psym=3,color=0 ,/data
        endif
     endif else begin
        PLOT, flx/maxshockrad, fly/maxshockrad, PSYM = 3, $ 
              TITLE = thlet+'!DBN!N at B-Shock Crossings', $
              XTITLE = 'X', $
              YTITLE = 'Y', $
              xrange=xrng,yrange=yrng,$
              xstyle=1,ystyle=1,color=0,background=255,$
              xthick=3,ythick=3,thick=3,charsize=chsize,/nodata
        PLOTS,vertex_list[0,*]/maxshockrad,vertex_list[1,*]/maxshockrad,psym=3,color=0 ,/data
     endelse
;Save the !X and !Y global variables, since fcolorbar modifies them...
     if sstep eq 0 then begin
        saveX=!X
        saveY=!Y
     endif

     if keyword_set(oplot) and sstep gt 0 then begin
        xyouts,0.171,0.885,strtime[sstep-1],charsize=chsize,charthick=2,/norm,color=255
     endif
     xyouts,0.171,0.885,strtime[sstep],charsize=chsize,charthick=2,/norm,color=0
     
     ;Contour data on regular grid
     loadct,13,/silent
     
     ;If the field line is open, plot an open circle, else a filled circle
     closedsym=1
     opensym=6
     symind=reform(pfsslines[crossPoints[sstep,0:ncrosses-1].linid].open)
     openind=where(symind eq 1)
     if openind[0] ne -1 then symind[openind]=opensym
     closedind=where(symind eq 0)
     if closedind[0] ne -1 then symind[closedind]=closedsym
     symind=reform(symind)  
     
     ;Plot the closed field line crossings
     if closedind[0] ne -1 then begin
        PLOTS,flx[closedind]/maxshockrad,fly[closedind]/maxshockrad,psym=sym(closedsym),$
              symsize=1.8,color=(th[closedind]-minn)/(maxx-minn)*255.,/data     
     endif

     ;Plot the open field line crossings
     if openind[0] ne -1 then begin
        PLOTS,flx[openind]/maxshockrad,fly[openind]/maxshockrad,psym=sym(closedsym),$
              symsize=1.8,color=(th[openind]-minn)/(maxx-minn)*255.,/data    
        loadct,0,/silent
        PLOTS,flx[openind]/maxshockrad,fly[openind]/maxshockrad,psym=sym(closedsym),$
              symsize=1.2,color=0,/data
        PLOTS,flx[openind]/maxshockrad,fly[openind]/maxshockrad,psym=sym(closedsym),$
              symsize=0.8,color=255,/data
        loadct,13,/silent
     endif
     
     ;for pp=0,ncrosses-1 do begin
     ;   PLOTS,flx[pp]/maxshockrad,fly[pp]/maxshockrad,psym=sym(closedsym),$
     ;         symsize=1.8,color=(th[pp]-minn)/(maxx-minn)*255.,/data     
     ;   if symind[pp] eq opensym then begin
     ;      loadct,0,/silent
     ;      PLOTS,flx[pp]/maxshockrad,fly[pp]/maxshockrad,psym=sym(closedsym),$
     ;            symsize=1.0,color=255,/data
     ;      loadct,13,/silent
     ;   endif
     ;endfor
     

     fcolorbar, MIN=minn,MAX=maxx,Divisions=4, $
                Color=0,VERTICAL=1,RIGHT=1, TITLE=thlet+'!DBN!N [deg]',$
                CHARSIZE=chsize, charthick=2, format='(i)',Position=[0.89, 0.4, 0.92, 0.8]
     
     !X=saveX
     !Y=saveY
     
     if sstep lt nsteps then begin
        image=tvrd(true=1)
        write_png,savefname,image,rr,gg,bb
     endif
     print,'Saving image '+in
     endelse
  endfor
;---------------------------------------------------------------------------------------
end
