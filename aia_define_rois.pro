pro test_aia_define_rois
;The actual events and AIA channels to measure...
  
  event=load_events_info(label='110511_01')
  waves=['193']
  for w=0,n_elements(waves)-1 do begin
      aia_define_rois,event,wave=waves[0],numroi=8
  endfor
end



pro aia_define_rois,event,savepath=savepath,automatic=automatic,wave=wave,numroi=numroi,roisize=roisize
;PURPOSE:
;
;This procedure defines the ROIs for the ionization and DEM
;calculations.
;
;CATEGORY:
; AIA/Ionization
;
;INPUTS:
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
tmp=strsplit(event.date,'/',/extract)
evdate=tmp[0]+tmp[1]+tmp[2]


;Kamen Kozarev, 
  
  loadct,9,/silent
  tvlct,rr,gg,bb,/get
  ;tvlct,reverse(rr),reverse(gg),reverse(bb)
  ;aia_lct,rr,gg,bb,wavelnth=fix(wave[wav]),/load

  if not keyword_set(savepath) then $
     basepath=event.savepath
  if not keyword_set(numroi) then NUMROI=5
  if not keyword_set(roisize) then ROISIZE=50
  if not keyword_set(waves) then waves=['193','211','335','171','94','131']
  
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

        aia_load_event,event.st,event.et,wave,index,data,coords=coords,$
                  subdata=subdata,subindex=subindex,/remove_aec,/subroi
        baseim=subdata[*,*,0]
        sunrad=subindex[0].R_SUN
        xcenter=subindex[0].X0_MP
        ycenter=subindex[0].Y0_MP
        if keyword_set(automatic) then begin
           ;Load the 193 angstrom data,
           ;We're sure this will always be there
           ionizfile=savepath+'ionization/rois_'+evlabel+'_'+'193'+'.sav'
           if file_search(ionizfile) eq '' then begin
              print,''
              print,'File '+ionizfile+' does not exist. Rerun manually. Quitting...'
              print,''
              return
           endif
           restore,savepath+'ionization/rois_'+evlabel+'_'+'193'+'.sav'

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
              if not file_exist(savepath+'ionization/'+'rois_'+evlabel+'.png') then begin
                 image=tvrd(/true)
                 write_png,savepath+'ionization/'+'rois_'+evlabel+'.png',image,rr,gg,bb
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
           
           for roi=0,NUMROI-1 do begin
              print,'----------------------------------------------'
              print,'Please select ROI #'+string(roi+1)
              roiselect_square,xr,yr,roisize=ROISIZE,roiname='R'+strtrim(string(roi+1),2)
              roiStart_x[roi]=xr[0]
              roiEnd_x[roi]=xr[1]
              roiStart_y[roi]=yr[0]
              roiEnd_y[roi]=yr[1]
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
           
        endif ;if wav eq 0
        
     endelse  ;if keyword_set(automatic)
;Update the index using add_tag
        newind=subindex
        newind=add_tag(newind,roiStart_x,'ROISTART_X')
        newind=add_tag(newind,roiStart_y,'ROISTART_Y')
        newind=add_tag(newind,roiEnd_x,'ROIEND_X')
        newind=add_tag(newind,roiEnd_y,'ROIEND_Y')
        roi_subindex=newind
        
;Crop the ROIs
        roi_subdata=fltarr(NUMROI,ROISIZE,ROISIZE,n_elements(subindex))
        for roi=0,NUMROI-1 do begin
           roi_subdata[roi,*,*,*]=subdata[roiStart_x[roi]:roiEnd_x[roi],roiStart_y[roi]:roiEnd_y[roi],*]
        endfor
        
        
;Save the ROIs and updated index for each event and wavelength
        save, filename=savepath+'ionization/rois_'+evlabel+'_'+wave+'.sav',roi_subindex,roi_subdata
        
     endfor
  endfor
end
