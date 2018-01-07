pro test_pfss_plot_field_lines
;Testing the CSGS model plotting procedure
event=load_events_info(label='paper')
pfss_plot_field_lines,event,/hires
end


pro pfss_plot_field_lines,event,hires=hires,lores=lores,pfssLines=pfssLines
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
  mfhcpath=event.mfhcpath


;+==============================================================================
;LOAD THE DATA
  
  ;Load the AIA observations
  aiafile=file_search(event.savepath+'normalized_'+eventname+'_subdata.sav')
  print,'Loading AIA File '+aiafile
  if aiafile[0] ne '' then begin
     restore,aiafile[0]
     aiatime=anytim(subindex.date_obs)
     aiatime=aiatime-aiatime[0]
     subdata=reform(subdata[0])
     subindex=reform(subindex[0])
  endif else begin
     print,'No AIA data present. Quitting...'
     return
  endelse
  
  ;Find a file to load with the latest results of applying the CSGS model
  ;csgsfile=find_latest_file(event.mfhcpath+'csgs_results_*')
  csgsfile=file_search(event.mfhcpath+'csgs_results_'+event.date+'_'+event.label+'_lores.sav')
  if keyword_set(hires) then csgsfile=file_search(event.mfhcpath+'csgs_results_'+event.date+'_'+event.label+'_hires.sav')
  if csgsfile eq '' then begin
     print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
;-==============================================================================



;+==============================================================================
;Constants and definitions
  lon=event.arlon
  lat=event.arlat
  xcenter=suncenter[0]
  ycenter=suncenter[1]
  zcenter=suncenter[2]
  sunrad=subindex.r_sun+10;For some reason the R_SUN variable is 10 px short...
  set_plot,'z'
;-==============================================================================



;+==============================================================================
;PLOT THE AIA IMAGE
  aia_lct,rr,gg,bb,wavelnth=subindex.wavelnth,/load     
        ;device,set_resolution=[winsize,winsize],SET_PIXEL_DEPTH=24, DECOMPOSED=0
  device,set_resolution=[event.aiafov[0],event.aiafov[1]],SET_PIXEL_DEPTH=24,DECOMPOSED=0
  tv,bytscl(sqrt(subdata[*,*]),min=1,max=50)
     
;Overplot the limb location
  circ=aia_circle(xcenter,ycenter,sunrad,/plot)
;-============================================================================== 



;+==============================================================================
; Plot the field lines on disk center.
  
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
  
  for ll=0.0D,nPlotLines-1 do begin
     ff=plotLinesIndex[ll]
     npt=pfssLines[ff].npts
                                ;Plot the field lines
     if pfssLines[ff].open eq 1 then color=110 else color=230
     if pfss_cartpos[ff,2,0] gt 0.0 and pfss_cartpos[ff,2,npt-1] gt 0.0 then begin
        plots,reform(pfss_cartpos[ff,*,0:npt-1]),/device,color=0,thick=4
        plots,reform(pfss_cartpos[ff,*,0:npt-1]),/device,color=color,thick=2.5
     endif
  endfor
;-==============================================================================
     
     

;+==============================================================================     
; Save the plot in a png file
  tvlct,rr,gg,bb,/get
  image=tvrd(true=1)
  resolution='lores'
  if keyword_set(hires) then resolution='hires'
  write_png,mfhcpath+'aia_pfss_'+event.date+'_'+event.label+'_field_lines_'+resolution+'.png',image,rr,gg,bb
  set_plot,'x'
;+==============================================================================

end
