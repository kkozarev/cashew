pro test_yaftawave_track_gprep
  
  event=load_events_info(label='110511_01')
;  testpath='/home/kkozarev/algoTests/yafta/test/'
;  respath=testpath              ;+'test/'
  
 ; path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
   
  ;Rebin to 512^2 pixels
  ;yaftawave_track_features2,event,features512,all_masks512,$
  ;         rebin=[event.aiafov[0]/2.,event.aiafov[1]/2.],$
  ;         /ps,min_size=500
 ;                           savepath=savepath,min_size=500,/ps
 ; stop
  ;Rebin to 256^2 pixels
 ; yaftawave_track_features, subindex,subdata,baseim,features256,all_masks256,rebin=[256,256],start_step=start_step,end_step=end_step,$
 ;                           savepath=savepath,min_size=200,/ps
  ;stop
  ;Run original size
  
                                ;yaftawave_track_features2,event,features,all_masks,/ps,min_size=2000,level=2,savepath=event.yaftawavepath+'2000px/'
  smooth=16
  min_size=20000
  prepversion='v1'
  yaftawave_track_gprep,event,features,all_masks,/ps,min_size=min_size,smooth=smooth,prepversion=prepversion;,rebin=[512,1024];,/dilate;,savepath=event.yaftawavepath+'14000px/'
end


pro yaftawave_track_gprep,event,features,all_masks,min_size=min_size,savepath=savepath,wav=wav,dilate=dilate,$
                          threshold=threshold,ps=ps,nohanning=nohanning, level=level,eventname=eventname,$
                          start_step=start_step,end_step=end_step,rebin=rebin,running=running,nodisk=nodisk,$
                          smooth=smooth, prepversion=prepversion
;PURPOSE
;This test is the state of YAFTAWave development as of 08/31/2011
;The skeleton of the procedure is taken from the original yafta test
;procedure.
;This version uses base difference images, removes noise with a
;Hanning filter, and then applies YAFTA with the appropriate parameters.
;
;CATEGORY:
;AIA/YAFTA
;
;INPUTS:
;       subindex - an index array for the AIA data.
;       subdata - the AIA datacube.
;
;
;OPTIONAL INPUT:
;       min_size - if set, this is the smallest number of pixels that
;                  are allowed in a feature.
;       wav - a string holding the wavelength channel name
;       ps - if set, this keyword will contain a string folder name to
;            save .eps and .png files with the features from each time step.
;       threshold - if set, this keyword should contain a constant minimum pixel
;                   intensity threshold for detecting features. The
;                   default is (0.6*standard deviation of each image).
;
;       level - if set, contains the multiplicative constant that is
;               applied in calculating the threshold (see above) for
;               feature detection. The formula is threshold = level * stdev(image)
;               The default is level = 0.6.
;
;       nohanning - if set, do not apply the Hanning smoothing of the image.
;
;OUTPUT:
;       features - a one-dimensional array of structures containing
;                  the feature information.
;       all_masks - an array of the pixel masks of the detected
;                   features. This array has the same dimensions as
;                   subdata.
;
;OPTIONAL OUTPUT:
;
;
;DEPENDENCIES:
; contiguous_mask, create_features, plot_edges, display_yafta, match_features_v01
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 08/31/2011
;Update: 09/12/2011 - KAK - This is an early production version of the
;                     yaftawave feature tracking routine for AIA data.
;
  
;restore,testpath+'normalized_AIA_20110125_05_211_subdata_testdata.sav'
;enum='37'
;date='20110511'
;wav='193'
;file='normalized_AIA_'+date+'_'+enum+'_'+wav+'_subdata.sav'
;restore,path+enum+'/'+file
;subdata=subdata[*,*,10:119]
;subindex=subindex[10:119]
  set_plot,'x'
  label=event.label
  date=event.date
  
  if not keyword_set(savepath) then $
     if not keyword_set (event) then savepath='./' else savepath=event.yaftawavepath
  
;========================================================
;SELECT THE PROPER KEYWORDS
  if not keyword_set(wav) then wav='193'
  ;LOADING THE AIA DATA - ORIGINAL VERSION
  ;aia_load_data,event.st,event.et,wav,subindex=subindex,subdata=subdata,event=event,/subroi
  ;GPREP VERSION: LOAD THE GPREPPED DATA
  date='20110511'
  basepath='/Users/kkozarev/Desktop/paper_YAFTA_Wave/'
  aia_folder='20110511_193'
  ;event=load_events_info(label='20110511')
  if keyword_set(prepversion) then version=prepversion else version='v1'
  if keyword_set(smooth) then smoothfactor=smooth else smoothfactor=8
  path=basepath+aia_folder+'_'+version+'/'
 

;========================================================
; LOAD THE AIA DATA 
  aia_data_savename=savepath+"AIA_20110125_193_"+version
  if not file_exist(aia_data_savename+".sav") then begin
     
     files=file_search(path+'*')
     nfiles=n_elements(files)
     
;get the times
     for f=0,nfiles-1 do begin
        res=strsplit(file_basename(files[f]),'_',/extract)
        if f eq 0 then time=res[3] else time=[time,res[3]]
     endfor
     
     read_sdo,files,subindex,subdata,/uncomp_delete
     help,subdata
     nt=n_elements(subdata[0,0,*])
     tmpdata=fltarr(1024,2048,nt)
                                ;GET RID OF THE NANs!!!
     tmp=where(finite(subdata,/nan))
     if tmp[0] ne -1 then subdata[tmp]=0.0
                                ;Make it a nice 1024x2048 array
     for tt=0,nt-1 do tmpdata[*,*,tt] = subdata[1087:2110,448:2495,tt]
     subdata=tmpdata
     
;========================================================		
  
     save,filename=aia_data_savename+".sav",subindex,subdata
     save,filename=aia_data_savename+"_index.sav",subindex
  endif else begin
     restore,aia_data_savename+".sav"
     nt=n_elements(subindex)
  endelse

;========================================================
;KEYWORD REBIN
  if keyword_set(rebin) then begin
     if n_elements(rebin) ne 2 then begin
        print,'Keyword rebin must be a 2-element array. Continuing with original dimensions...'
     endif else begin
        newsubdata=dblarr(rebin[0],rebin[1],nt)
        for i=0,nt-1 do $
           newsubdata[*,*,i]=rebin(reform(subdata[*,*,i]),rebin[0],rebin[1])
        subdata=newsubdata
     endelse
  endif
;========================================================

  nx = n_elements(subdata[*,0,0])
  ny = n_elements(subdata[0,*,0])
  
  if not keyword_set(wav) then wav='193'
  baseavgnsteps=5
  
  if keyword_set(start_step) then begin
     if start_step eq 0 then start_step=1
     data_start_step=start_step
  endif else begin
     start_step=1
     data_start_step=baseavgnsteps+1
  endelse
  if keyword_set(end_step) then begin
     
  endif else begin
     end_step=nt-1
  endelse
  data_end_step=end_step
 
;========================================================
;BASE IMAGE PREPARATION
  baseim=fltarr(nx,ny)
  for i=0,baseavgnsteps-1 do baseim += subdata[*,*,i]
  baseim /=(1.0*baseavgnsteps)
  
  tmpdata2=fltarr(512,1024,nt)
  for tt=0,nt-1 do tmpdata2[*,*,tt]=rebin(subdata[*,*,tt]-baseim,512,1024)
;========================================================

  ;wdef,2,512,1024
  ;tv,bytscl(tmpdata2[*,*,30],min=-0.1,max=0.1)
  ;wdef,3,512,1024
  ;tmp=rebin(subdata[*,*,30],512,1024)
  ;tvscl,tmp
  ;stop
  
; set some parameters of tracking run
;====================================
  dx=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS) ; AIA Full Disk pixel size in km
  if not keyword_set(min_size) then min_size = 2000                          ; only track features containing min_size pixels or more
  if not keyword_set(ps) then wdef,0,nx,ny

  
;THE TIME STEP LOOP
  for t = data_start_step,data_end_step do begin
     tmp=subindex[t].date_obs
     tmp=strsplit(tmp,'T:.',/extract)
     strtime=tmp[1]+tmp[2]+tmp[3]
     wav=strtrim(string(subindex[t].wavelnth),2)
     print, "Tracking step:",string(t+1)
     filenum = STRMID(STRING(1000 + fix(t+1), FORMAT = '(I4)'), 1)
     filename = savepath+'yaftawave_gprep_'+version+'_'+event.date+'_'+event.label+'_'+wav+'_'+strtime
;filename = savepath+STRCOMPRESS('ywave_'+'AIA_'+wav+'_'+strtrim(string(nx),2)+'_'+ filenum, /REMOVE_ALL)
     
                                ; get current data array (suffix "2" is from
                                ; current step, "1" is from prev. step)
                                ;===========================================
     
     ;stop
     ;Despike the image, and subtract the base image from it.
     timind=subindex[t]
     
     if keyword_set(running) then begin
        img2=(subdata[*,*,t]-subdata[*,*,t-1])*1000.
        origim=img2
     endif else begin
        im=subdata[*,*,t]
        img2 = (im-baseim)*1000.
        origim=img2
     endelse

;===========================================
;SMOOTH THE IMAGE
     img2=smooth(img2,smoothfactor,/edge_truncate)
     img2[where(img2 lt 0.0)]=0.0
;===========================================
     
     ;Obtain some statistical information
     mom=moment(img2)
     meanv=mom[0]
     stdv=sqrt(mom[1])
     
     
     
     if not keyword_set(threshold) then begin
        if keyword_set(level) then thresh=stdv*level else thresh=stdv*0.2
     endif else begin
        thresh=threshold
     endelse
     print,stdv,thresh
;stop
;Here, enhance the brighter features some more:
;===========================================
  ;  eqim=adapt_hist_equal(img2)
  ;  img2=eqim
     
;Apply a binary mask
;    imm=img2
;    imm=origim
;    ind=where(imm ge thresh)
;    if ind[0] ne -1 then imm[ind]*=2.0
;    ind=where(imm le thresh)
;    if ind[0] ne -1 then imm[ind]=0.0
;    img2=imm
;    set_plot,'x'
;  stop  
    
;remove the disk so the algorithm doesn't get confused.
    if keyword_set(nodisk) then begin
       resim=aia_hide_disk(timind,img2,value=meanv)
       img2=resim
       ;if keyword_set(rebin) then resim=rebin(resim,rebin[0],rebin[1])
    endif
    
    ;tv,img2;bytscl(img2,min=-40,max=50)
    
    ; group pixels in current data array
    ;=====================================
    rankdown, img2, mask2, threshold=thresh,/pad; , pad=1
    ; do same for contiguous pixel groupings & their structures
    contiguous_mask,img2,mask2,threshold=thresh,/pad
    
    ; defines structures for current data array
    ;============================================
    create_features,img2, mask2, features2, min_size=min_size, $ ;/unipolar
                    vx=vx, vy=vy, peakthreshold=peakthreshold, dx=dx
    
    ;error management - if no features are
    ;detected, move on to the next image
    sz=size(features2)
    nels=n_elements(sz)
    if sz[nels-2] eq 0 then begin
       print,'No features found in this image. Continuing...'
       continue
    endif
       
    features2.step=t+1 ; insert current time step
    
    ;Optionally, dilate the masks to make the features smoother
    ;========================================= 
    dil_features=features2
    if keyword_set(dilate) then begin
       newmask=mask2
       newmask[*,*]=0
       origmask=newmask
       for ff=0,n_elements(dil_features)-1 do begin
          mask_str=dil_features[ff].mask_str
          addresses=long(strsplit(mask_str,/extract))
          dil_mask = origmask
          dil_mask[addresses] =dil_features[ff].label
          ;s1 = replicate(1,3,3) ; dilates to nearest neighbors
          ;dilate1 = (dilate(dil_mask, s1))*dil_features[ff].label
          s2 = replicate(1,5,5) ; dilates to 2 nearest neighbors
          dilate2 = (dilate(dil_mask, s2))*dil_features[ff].label
          ;s3 = replicate(1,7,7) ; dilates to 3 nearest neighbors
          ;dilate3 = (dilate(dil_mask, s3))*dil_features[ff].label
          ;s4 = replicate(1,9,9) ; dilates to 4 nearest neighbors
          ;dilate4 = (dilate(dil_mask, s4))*dil_features[ff].label
          newmask+=dilate2
          dil_features[ff].mask_str=strjoin(string(where(newmask eq dil_features[ff].label)))
          dil_features[ff].size=n_elements(where(newmask eq dil_features[ff].label))
;          display_yafta,bytscl(origim,min=-50,max=40), /asp
;          plot_edges, dilmask
;          stop
;          plot_edges, dilate1
;          stop
;          plot_edges, dilate2
;          stop
;          plot_edges, dilate3
          ;stop
       endfor
       dil_mask=newmask
       ;mask2=newmask
       ;features2=dil_features


;DEBUGGG
;TRY TO FIX THE OVERLAPPING PIXELS IN THE DILATED IMAGES
       dil_max_label=max([dil_features.label, features2.label])
       match_features_v01, features2, dil_features, mask2, dil_mask, img2, img2, $
                           old_max_label=dil_max_label
;DEBUGGG
    endif
    

    ; after at least two steps, begin matching
    ;========================================= 
    if (t gt data_start_step+1) then begin 
       
        ; match features from previous and current steps
        ;================================================
        match_features_v01, features1, features2, mask1, mask2, img1, img2, $
          old_max_label=old_max_label 
        
        ; concantenate masks from current step with prev. steps' masks
        ;==============================================================
        all_masks = [[[all_masks]],[[mask2]]]
        if keyword_set(dilate) then all_dil_masks = [[[all_dil_masks]],[[dil_mask]]]
        ; concantenate features from current step with prev. steps'
        ;==========================================================
        if (n_elements(all_features) eq 0) then  all_features = features1 $
        else all_features = [all_features, features1]

        ; find highest existing label, for unique labels on next step
        ;=============================================================
        old_max_label = max([all_features.label, features2.label]) 
    
     endif else begin
        all_masks = mask2       ; store current mask
        if keyword_set(dilate) then all_dil_masks = dil_mask
     endelse

    ; raname current data array, mask, and
    ; feature as same from previous step
    ;=========================================    
    img1 = temporary(img2)
    mask1 = temporary(mask2)
    features1 = temporary(features2)
    
    if keyword_set(ps) then begin
       set_plot,'ps'
       device,/color,filename=STRCOMPRESS(filename+'.eps',/re),/inches,xsize=nx/102.4,ysize=ny/102.4
    endif
    
    ; graphical output of features, labels
    ;=========================================
    if not keyword_set(ps) then wdef,0,nx,ny
    ;Calculate the arcsec values of the pixels along the X- and Y-axes.
    ;x_values=timind.IMSCL_MP*(findgen(event.aiafov[0])+timind.subroi_x0-2048.)
    ;y_values=timind.IMSCL_MP*(findgen(event.aiafov[1])+timind.subroi_y0-2048.)
    ;Plot the image
    
    display_yafta,bytscl(origim,min=-50,max=40),$;x_values,y_values,$
                  tit=timind.origin+'/'+timind.instrume+$
                  '/'+timind.wave_str+'  '+timind.date_obs+'   '+filenum,/aspect
    if keyword_set(dilate) then begin
       plot_edges,dil_mask,thick=4
       plot_labels,dil_features,thick=4
    endif else begin
       plot_edges,mask1,thick=4
       plot_labels,features1,thick=4
    endelse
    
    ; for generating graphical output
    ;================================
    ; Close output to .ps file, then convert to png
    ;=========================
    if keyword_set(ps) then begin
        device,/close
        set_plot,'x'
        exec='convert -flatten '+filename+'.eps '+filename+'.png'+'; rm '+filename+'.eps'
        ;print,exec
        spawn,exec
     endif
    
; Include most recent step's features.
;=====================================
    if t eq data_start_step then begin
       all_features=features1
       if keyword_set(dilate) then all_dil_features=dil_features
    endif else begin
       if all_features ne !NULL then begin
          all_features = [all_features, features1]
          if keyword_set(dilate) then all_dil_features=[all_dil_features,dil_features]
       endif
    endelse

 endfor


; Rename for use with YAFTA_IDL.pro
;=====================================
features = all_features


; Write data to YAFTA_output.sav
;=====================================
if keyword_set(dilate) then save,features,all_masks,all_dil_features,all_dil_masks,thresh,min_size,dx,subindex,$
                                filename=savepath+'yaftawave_gprep_'+version+'_'+event.date+'_'+event.label+'_'+wav+'_dilate.sav' $
else save,features,all_masks,thresh,min_size,dx,subindex,$
          filename=savepath+'yaftawave_gprep_'+version+'_'+event.date+'_'+event.label+'_'+wav+'.sav'

end
