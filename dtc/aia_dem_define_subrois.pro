pro test_aia_dem_define_subrois
;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     labels=['140708_01','131212_01','130517_01','130423_01','120915_01','120526_01',$
	  '120424_01','110607_01','110211_02','110125_01']
     for ev=0,n_elements(labels)-1 do begin
         label=labels[ev]
         event=load_events_info(label=label)
         aia_dem_define_subrois,event,/force
     endfor
  endif
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev] 
        aia_dem_define_subrois,event
     endfor
  endif

end


pro aia_dem_define_subrois,event,savepath=savepath,force=force
;PURPOSE:
;
;This procedure defines the AIA ROIs for the ionization and DEM
;calculations. The difference between this procedure and
;aia_dem_define_subrois_manual is that this one automatically positions 8 rectangular
;regions tangentially to the shock surface, four along the shock
;surface for the middle time step, and four along a radial direction.
;Also, it only saves the pixel positions of each region, not the data itself.
;
;CATEGORY:
; AIA
;
;INPUTS:
;       event - the event structure
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
; aia_circle
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 04/01/2014
;

  wav='193'
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  ionizpath=event.ionizationpath
  eventname='AIA_'+date+'_'+label+'_'+wav
  aiafile=event.savepath+'normalized_'+eventname+'_subdata.sav'
  shockfile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed_radial.sav'

  if file_exist(ionizpath+'rois_'+date+'_'+label+'.sav') and not keyword_set(force) then begin
     print,''
     print,'The file '+ionizpath+'rois_'+date+'_'+label+'.sav'+' exists.'
     print,'Run with the /force option to overwrite. Quitting.'
     print,''
     return
  endif  
  
;+==============================================================================
;LOAD THE DATA
  print,''
  print,'Loading data...'
  
  ;Load the AIA observations
  print,'Loading AIA File '+aiafile
  restore,aiafile
  
  ;Load the shock information
  print, 'Loading shock info file '+shockfile
  restore,shockfile
;-==============================================================================
  
  
  ;Create the AR coordinates and 
  ar=[subindex[0].arx0,subindex[0].ary0]
  xcenter=subindex[0].x0_mp
  ycenter=subindex[0].y0_mp
  sunrad=subindex[0].R_SUN
  zcenter=0.0
  suncenter=[xcenter,ycenter,zcenter]
  ;Create the wave position values
  sp=rad_data.xfitrange[0]
  ep=rad_data.xfitrange[1]
  time=(rad_data.time[sp:ep]-rad_data.time[sp]) ;relative time in seconds
  RSUN=subindex[0].rsun_ref/1000.               ;Solar radius in km.
  KMPX=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS) ; km/px
  pfss_shock_generate_csgs_radii,ind_arr,rad_data,radiusfitlines
  
  ;fit=reform(rad_data.fitparams[0,*].front)
  ;radiusfitlines=(fit[0]+fit[1]*time+0.5*fit[2]*time^2)/RSUN
  ;radiusfitlines-=1.
  radiusfitlines*=RSUN*event.geomcorfactor
  radius=radiusfitlines/kmpx ;The fitted radius values
  nsteps=n_elements(time)
  rr=floor(nsteps/2.)            ; this is the step of the region for which to save the positions.
  ;The angle corresponding to latitude of the active region
  rad_angle=atan((ar[1]-suncenter[1])/(ar[0]-suncenter[0]))
  ;The distance from the AR position to the shock front
  total_ar_rad=sqrt((ar[0]-suncenter[0])^2+(ar[1]-suncenter[1])^2)
  newrad=radius+total_ar_rad
  
  
;Plot stuff
  ;wdef,0,1024
  loadct,0,/silent
  scaling=0.5
  wdef,0,event.aiafov[0]*scaling,event.aiafov[1]*scaling
  ;im=reform(sqrt(subdata[*,*,20]))
  baseim=subdata[*,*,0]
  im=bytscl(subdata[*,*,floor(sp+rr)]-baseim,-50,50)
  if scaling ne 1.0 then im = congrid(im,n_elements(im[*,0])*scaling,n_elements(im[0,*])*scaling)
  
  tvscl,im
  ;Calculate the positions of the 
  shock_x=(newrad*cos(rad_angle)+suncenter[0])*scaling
  shock_y=(newrad*sin(rad_angle)+suncenter[1])*scaling
  xx=(findgen(2500)*cos(rad_angle)+suncenter[0])*scaling
  yy=(findgen(2500)*sin(rad_angle)+suncenter[1])*scaling
  
  plots,shock_x,shock_y,psym=2,/device
  plots,xx,yy,/device
  res=aia_circle(ar[0]*scaling,ar[1]*scaling,radius[rr]*scaling,/plot)
 
  
;Calculate the region rectangle prototype
  ROISIZE=[100,20]*scaling
  regx=[0-roisize[0]/2.,0+roisize[0]/2.]
  regy=[0-roisize[1]/2.,0+roisize[1]/2.]
  regz=[0,0]
  regvolume=[[regx[0],regy[0],regz[0]],$
             [regx[1],regy[0],regz[0]],$
             [regx[1],regy[1],regz[0]],$
             [regx[0],regy[1],regz[0]]]


  NUMROI=8
  roiStart_x=fltarr(NUMROI)
  roiEnd_x=roiStart_x
  roiStart_y=roiStart_x
  roiEnd_y=roiStart_x
  roi_radheight=dblarr(NUMROI)  ;The average distance of the ROI from Sun center.
  roi_polydata=dblarr(NUMROI,n_elements(subindex))
  roi_positions=replicate({npix:0,posind:dblarr(2,1.1*ROISIZE[0]*ROISIZE[1])},NUMROI) ;Hold the positions of all ROI pixels


;+-----------------------------------------------------------------------------
;Here, extract the angular information
  for roi=0,NUMROI-4 do begin
     roiname='R'+strtrim(string(roi+1),2)
     angle=(rad_angle+!PI/2.*roi/(NUMROI-4))-!PI/4.
;Transform the region to its place.
     
     reg = transform_volume(regvolume,rotation=[0,0,-(90-angle*180./!PI)],$
                             translate=[(ar[0]+radius[rr]*cos(angle))*scaling,(ar[1]+radius[rr]*sin(angle))*scaling,0])
    
;Here, check if the points are within the image. If not, set them to
;the image limits. NOTE: THIS IS A HACK! There should be a better way
;to determine if a region is entirely outside the AIA FOV
     ind=where(reg[0,*] ge event.aiafov[0]*scaling)
     if ind[0] ne -1 then reg[0,ind]=n_elements(im[*,0])-n_elements(ind)+ind
     ind=where(reg[1,*] ge event.aiafov[1]*scaling)
     if ind[0] ne -1 then reg[1,ind]=n_elements(im[0,*])-n_elements(ind)+ind
    
     
;Record the starting and ending positions
     roiStart_x[roi]=reg[0,0]
     roiEnd_x[roi]=reg[0,1]
     roiStart_y[roi]=reg[1,0]
     roiEnd_y[roi]=reg[1,3]
     xrange=[roiStart_x[roi],roiEnd_x[roi]]
     yrange=[roiStart_y[roi],roiEnd_y[roi]]
     roi_radheight[roi]=sqrt((avg(reg[0,*])-xcenter*scaling)^2+(avg(reg[1,*])-ycenter*scaling)^2)/(sunrad*scaling)
     
;Obtain the polygon of positions inside the rectangle.
     reg_poly_ind=polyfillv(reform(reg[0,*]),reform(reg[1,*]),n_elements(im[*,0]),n_elements(im[0,*]))
     ;The two-dimensional position indices
     arrind=array_indices(im,reg_poly_ind)
     for tt=0,n_elements(subindex)-1 do roi_polydata[roi,tt]=avg(subdata[arrind[0,*],arrind[1,*],tt])
     npix=n_elements(arrind[0,*])
     roi_positions[roi].npix=npix
     roi_positions[roi].posind[*,0:npix-1]=arrind
;Fill the polygon rectangle with solid color.
     polyfill,reform(reg[0,*]),reform(reg[1,*]),/device
     xyouts,xrange[0]+roisize[0]/6.0,yrange[0]+roisize[1]/4.0,roiname,/device,$
            charsize=3,charthick=4,color=255
     stop
  endfor
;------------------------------------------------------------------------------


;+-----------------------------------------------------------------------------
;Record the positions of three more regions along the radial direction
  angle=rad_angle
  rads=radius[rr]+[(-100),100,200]
  for roi=NUMROI-3, NUMROI-1 do begin
     roiname='R'+strtrim(string(roi+1),2)
     rad=rads[roi-(NUMROI-3)]
     reg = transform_volume(regvolume,rotation=[0,0,-(90-angle*180./!PI)],$
                            translate=[ar[0]+rad*cos(angle),ar[1]+rad*sin(angle),0])
     ;Record the starting and ending positions
     roiStart_x[roi]=reg[0,0]
     roiEnd_x[roi]=reg[0,1]
     roiStart_y[roi]=reg[1,0]
     roiEnd_y[roi]=reg[1,3]
     xrange=[roiStart_x[roi],roiEnd_x[roi]]
     yrange=[roiStart_y[roi],roiEnd_y[roi]]
     roi_radheight[roi]=sqrt((avg(reg[0,*])-xcenter)^2+(avg(reg[1,*])-ycenter)^2)/sunrad 
     
     ;Obtain the polygon of positions inside the rectangle.
     reg_poly_ind=polyfillv(reform(reg[0,*]),reform(reg[1,*]),n_elements(im[*,0]),n_elements(im[0,*]))
     ;The two-dimensional position indices
     arrind=array_indices(im,reg_poly_ind)
     for tt=0,n_elements(subindex)-1 do roi_polydata[roi,tt]=avg(subdata[arrind[0,*],arrind[1,*],tt])
     npix=n_elements(arrind[0,*])
     roi_positions[roi].npix=npix
     roi_positions[roi].posind[*,0:npix-1]=arrind
;Fill the polygon rectangle with solid color.
     polyfill,reform(reg[0,*]),reform(reg[1,*]),/device
     xyouts,xrange[0]+roisize[0]/6.0,yrange[0]+roisize[1]/4.0,roiname,/device,$
            charsize=3,charthick=4,color=255
  endfor
;------------------------------------------------------------------------------
  tvlct,rr,gg,bb,/get
  image=tvrd(/true)
  write_png,ionizpath+'rois_'+date+'_'+label+'.png',image,rr,gg,bb
  
;Update the index using add_tag
     newind=subindex
     newind=add_tag(newind,roiStart_x,'ROISTART_X')
     newind=add_tag(newind,roiStart_y,'ROISTART_Y')
     newind=add_tag(newind,roiEnd_x,'ROIEND_X')
     newind=add_tag(newind,roiEnd_y,'ROIEND_Y')
     roi_subindex=newind
     
;Save the ROIs and updated index for each event and wavelength
     save, filename=ionizpath+'rois_'+date+'_'+label+'.sav',roi_subindex,roi_radheight,roi_positions
     


end
