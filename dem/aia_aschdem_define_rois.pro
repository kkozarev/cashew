pro test_aia_aschdem_define_rois
;The actual events and AIA channels to measure...

  waves=['193','211','335','171','94','131']
  
 ;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     event=load_events_info(label='110511_01')
     aia_aschdem_define_rois,event,numroi=8,roisize=10;,/automatic
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(waves)-1 do begin
           aia_aschdem_define_rois,event,waves=waves[w],numroi=8
        endfor
     endfor
  endif
end



pro aia_aschdem_define_rois,event,savepath=savepath,automatic=automatic,waves=waves,numroi=numroi,roisize=roisize
;PURPOSE:
;
;This procedure defines the ROIs for the ionization and DEM
;calculations.
;
;CATEGORY:
; AIA/Ionization
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
; aia_circle, roiselect_square
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 10/01/2011
;

evlabel=event.label
evdate=event.date
ionizpath=event.ionizationpath

;Kamen Kozarev, 
  
  loadct,9,/silent
  tvlct,rr,gg,bb,/get
  ;tvlct,reverse(rr),reverse(gg),reverse(bb)
  ;aia_lct,rr,gg,bb,wavelnth=fix(wave[wav]),/load

  if not keyword_set(savepath) then savepath=event.savepath
  if not keyword_set(numroi) then NUMROI=5
  if not keyword_set(roisize) then ROISIZE=50
  if not keyword_set(waves) then waves=['193','211','335','171','94','131']
  roi_positions=replicate({npix:0,pos:dblarr(2,1.1*ROISIZE*ROISIZE)},NUMROI) ;Hold the positions of all ROI pixels

  roiStart_x=fltarr(NUMROI)
  roiEnd_x=roiStart_x
  roiStart_y=roiStart_x
  roiEnd_y=roiStart_x
  
;loop over the wavelengths
  for wav=0,n_elements(waves)-1 do begin
     wave=waves[wav]
     
;Load the data
                                ;fname='normalized_AIA_'+evdate[ii]+'_'+events[ii]+'_'+wave+'_subdata.sav'
                                ;print,''
                                ;print,'Now loading file '+fname
                                ;print,''
                                ;if file_search(event.savepath+'/'+fname) eq '' then begin
                                ;   print,''
                                ;   print,'File '+basepath+event.savepath+'/'+fname+' does not exist. Quitting...'
                                ;   print,''
                                ;   return
                                ;endif
                                ;restore,basepath+event+'/'+fname
                                ;lim=[begstep[ii]+1,endstep[ii]]
     infile='normalized_AIA_'+evdate+'_'+evlabel+'_'+wave+'_subdata.sav'
     restored=0
  
  if (file_exist(savepath+infile)) and (not keyword_set(force)) then begin
     restore, savepath+infile
     restored=1 ;Flag to know that we've restored a datacube file
     
  endif else begin
     aia_load_data,event.st,event.et,wave,index,data,coords=coords,$
                   subdata=subdata,subindex=subindex,/remove_aec,/subroi
  endelse
     
     baseim=subdata[*,*,0]
     sunrad=subindex[0].R_SUN
     xcenter=subindex[0].X0_MP
     ycenter=subindex[0].Y0_MP
     
     
     if keyword_set(automatic) then begin
                                ;Load the 193 angstrom data,
                                ;We're sure this will always be there
        ionizfile=ionizpath+'rois_'+evdate+'_'+evlabel+'_'+'193'+'.sav'
        if file_search(ionizfile) eq '' then begin
           print,''
           print,'File '+ionizfile+' does not exist. Rerun manually. Quitting...'
           print,''
           return
        endif
        restore,ionizfile
        
;Check if the tags exist, otherwise complain and quit.
        if not tag_exist(roi_subindex,'roiStart_x') then begin
           print,''
           print,'ROIs have not been determined. Rerun manually.'
           print,'Quitting...'
           print,''
           return
        endif
        roiStart_x=roi_subindex.roiStart_x
        roiStart_y=roi_subindex.roiStart_y
        roiEnd_x=roi_subindex.roiEnd_x
        roiEnd_y=roi_subindex.roiEnd_y
        roi_subindex=0
        roi_subdata=0
        sunrad=subindex[0].R_SUN
        xcenter=subindex[0].X0_MP
        ycenter=subindex[0].Y0_MP
        
        wdef,0,event.aiafov[0],event.aiafov[1]
        tvscl,bytscl(subdata[*,*,fix(n_elements(subindex)/2.0)]-baseim,-50,50)
        circ=aia_circle(xcenter,ycenter,sunrad,/plot)
        
        for roi=0,NUMROI-1 do begin
           xrange=[roiStart_x[roi],roiEnd_x[roi]]
           yrange=[roiStart_y[roi],roiEnd_y[roi]]
           roiname='R'+strtrim(string(roi+1),2)
           plots,[xrange[0],xrange[1]],[yrange[0],yrange[0]],/device,thick=3,color=255
           plots,[xrange[0],xrange[0]],[yrange[0],yrange[1]],/device,thick=3,color=255
           plots,[xrange[1],xrange[1]],[yrange[0],yrange[1]],/device,thick=3,color=255
           plots,[xrange[0],xrange[1]],[yrange[1],yrange[1]],/device,thick=3,color=255
           xyouts,xrange[0]+roisize/6.0,yrange[0]+roisize/4.0,roiname,/device,$
                  charsize=3,charthick=4,color=255
           if not file_exist(ionizpath+'rois_'+evdate+'_'+evlabel+'.png') then begin
              image=tvrd(/true)
              write_png,ionizpath+'rois_'+evdate+'_'+evlabel+'.png',image,rr,gg,bb
           endif
           if wav eq 0 then begin
              ;Obtain the polygon of positions inside the rectangle.
              reg_poly_ind=polyfillv([xr[0],xr[1],xr[1],xr[0]],[yr[0],yr[0],yr[1],yr[1]],1024,1024)
              ;The two-dimensional position indices
              arrind=array_indices(baseim,reg_poly_ind)
              npix=n_elements(arrind[0,*])
              roi_positions[roi].npix=npix
              roi_positions[roi].pos[*,0:npix-1]=arrind
           endif
        endfor  
     endif else begin
        
        
;Define the regions using roiselect.pro
        ;DO IT ONLY FOR THE FIRST WAVELENGTH SO THE ROIS ARE THE SAME
        if wav eq 0 then begin
           wdef,0,1024
           print,'Showing the entire data sequence...'
           print,''
           
           for i=0,n_elements(subindex)-1 do begin
              tvscl,bytscl(subdata[*,*,i]-baseim,-60,30)
              circ=aia_circle(xcenter,ycenter,sunrad,/plot)
           endfor
           print,'Now showing a frame in the middle of the sequence:'
           print,''
           tvscl,bytscl(subdata[*,*,fix(n_elements(subindex)/2.0)]-baseim,-60,30)
           circ=aia_circle(xcenter,ycenter,sunrad,/plot)
           plots,[xcenter,1024],[ycenter,1024*tan(event.arlat*!PI/180.)],/device,thick=1.6,linestyle=2
           
           roi_radheight=dblarr(NUMROI) ;The average distance of the ROI from Sun center.
           for roi=0,NUMROI-1 do begin
              print,'----------------------------------------------'
              print,'Please select ROI #'+string(roi+1)
              roiselect_square,xr,yr,roisize=ROISIZE,roiname='R'+strtrim(string(roi+1),2)
              roiStart_x[roi]=xr[0]
              roiEnd_x[roi]=xr[1]
              roiStart_y[roi]=yr[0]
              roiEnd_y[roi]=yr[1]
              roi_radheight[roi]=sqrt((avg(xr)-xcenter)^2+(avg(yr)-ycenter)^2)/sunrad
              
              ;Obtain the polygon of positions inside the rectangle.
              reg_poly_ind=polyfillv([xr[0],xr[1],xr[1],xr[0]],[yr[0],yr[0],yr[1],yr[1]],$
                                     1024,1024)
              ;The two-dimensional position indices
              arrind=array_indices(baseim,reg_poly_ind)
              npix=n_elements(arrind[0,*])
              roi_positions[roi].npix=npix
              roi_positions[roi].pos[*,0:npix-1]=arrind
           endfor
           print,''
           print,'We have all the necessary information, thank you!'
           print,''
           
                                ;HERE, plot all the ROIs and save the
                                ;image as png for future reference...
           tvscl,bytscl(subdata[*,*,fix(n_elements(subindex)/2.0)]-baseim,-60,30)
           circ=aia_circle(xcenter,ycenter,sunrad,/plot)
           
           for roi=0,NUMROI-1 do begin
              xrange=[roiStart_x[roi],roiEnd_x[roi]]
              yrange=[roiStart_y[roi],roiEnd_y[roi]]
              roiname='R'+strtrim(string(roi+1),2)
              plots,[xrange[0],xrange[1]],[yrange[0],yrange[0]],/device,thick=3,color=255
              plots,[xrange[0],xrange[0]],[yrange[0],yrange[1]],/device,thick=3,color=255
              plots,[xrange[1],xrange[1]],[yrange[0],yrange[1]],/device,thick=3,color=255
              plots,[xrange[0],xrange[1]],[yrange[1],yrange[1]],/device,thick=3,color=255
              xyouts,xrange[0]+roisize/6.0,yrange[0]+roisize/4.0,roiname,/device,$
                     charsize=3,charthick=4,color=255
           endfor
           tvlct,rr,gg,bb,/get
           image=tvrd(/true)
           write_png,savepath+'ionization/'+'rois_'+evlabel+'.png',image,rr,gg,bb
           
        endif                   ;if wav eq 0
        
     endelse                    ;if keyword_set(automatic)
;Update the index using add_tag
     newind=subindex
     newind=add_tag(newind,roiStart_x,'ROISTART_X')
     newind=add_tag(newind,roiStart_y,'ROISTART_Y')
     newind=add_tag(newind,roiEnd_x,'ROIEND_X')
     newind=add_tag(newind,roiEnd_y,'ROIEND_Y')
     roi_subindex=newind
     
;Crop the ROIs
     roi_subdata=dblarr(NUMROI,ROISIZE,ROISIZE,n_elements(subindex))
     for roi=0,NUMROI-1 do begin
        roi_subdata[roi,*,*,*]=subdata[roiStart_x[roi]:roiEnd_x[roi],roiStart_y[roi]:roiEnd_y[roi],*]
     endfor
     
     
;Save the ROIs and updated index for each event and wavelength
     save, filename=ionizpath+'rois_'+evdate+'_'+evlabel+'_'+wave+'.sav',roi_subindex,roi_subdata,roi_radheight,roi_positions
     
  endfor
end
