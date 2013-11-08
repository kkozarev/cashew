pro test_yaftawave_track_features2
  set_plot,'x'
  testpath='/home/kkozarev/algoTests/yafta/test/'
  respath=testpath              ;+'test/'
  
  path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
  label='37'
  savepath='/Volumes/Backscratch/Users/kkozarev/testAIA/'+label+'/'
  date='20110511'
  wav='193'
  file='normalized_AIA_'+date+'_'+label+'_'+wav+'_subdata.sav'
  restore,path+label+'/'+file
  nt=n_elements(subdata[0,0,*])
  start_step=70
  end_step=90
  
  ;Rebin to 512^2 pixels
 ; yaftawave_track_features, subindex,subdata,features512,all_masks512,rebin=[512,512],start_step=start_step,$ 
 ;                           savepath=savepath,min_size=500,/ps
 ; stop
  ;Rebin to 256^2 pixels
 ; yaftawave_track_features, subindex,subdata,baseim,features256,all_masks256,rebin=[256,256],start_step=start_step,end_step=end_step,$
 ;                           savepath=savepath,min_size=200,/ps
  ;stop
  ;Run original size
  yaftawave_track_features2, subindex,subdata,features,all_masks,start_step=start_step,end_step=end_step,savepath=savepath,/ps,/running
  stop

end


pro yaftawave_track_features2, subindex,subdata,features,all_masks,min_size=min_size,savepath=savepath,$
                              threshold=threshold,ps=ps,nohanning=nohanning, level=level,eventname=eventname,$
                              start_step=start_step,end_step=end_step,rebin=rebin,running=running
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
;       ps - if set, this keyword will contain a string folder name to
;            save .eps and .png files with the features from each time step.
;       threshold - if set, this keyword should contain a constant minimum pixel
;                   intensity threshold for detecting features. The
;                   default is 0.6*standard deviation of each image.
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

if not keyword_set(eventname) then eventname='event'
if not keyword_set (savepath) then savepath='./'
nx = n_elements(subdata[*,0,0])
ny = n_elements(subdata[0,*,0])
nt = n_elements(subdata[0,0,*])
if not keyword_set(start_step) then start_step=25
if not keyword_set(end_step) then end_step=nt-1
baseavgnsteps=5
;prepare base image
baseim=fltarr(nx,ny)
for i=start_step-baseavgnsteps,start_step-1 do baseim += subdata[*,*,i]
baseim /=(1.0*baseavgnsteps)



; set some parameters of tracking run
;====================================
dx=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)   ; AIA Full Disk pixel size in km
if not keyword_set(min_size) then min_size = 2000       ; only track features containing min_size pixels or more
if keyword_set(rebin) then wdef,0,rebin[0],rebin[1] else wdef,0,nx,ny


;THE STEP LOOP
for t = start_step,end_step do begin
   
    wav=strtrim(string(subindex[t].wavelnth),2)
    print, "Tracking step:",string(t+1)
    filenum = STRMID(STRING(1000 + fix(t+1), FORMAT = '(I4)'), 1)
    filename = savepath+STRCOMPRESS('ywave_'+'AIA_'+wav+'_'+strtrim(string(nx),2)+'_'+ filenum, /REMOVE_ALL)
    

    ; get current data array (suffix "2" is from
    ; current step, "1" is from prev. step)
    ;===========================================


    ;Despike the image, and subtract the base image from it.
    ind=subindex[t]
    im=subdata[*,*,t]
    img2 = im-baseim
    if keyword_set(running) then img2=subdata[*,*,t]-subdata[*,*,t-1]
    
    

;Here, enhance the brighter features some more:
;===========================================
    eqim=adapt_hist_equal(img2)
    img2=eqim
    
;Apply a binary mask
    imm=img2
    imm[where(imm gt thresh)]*=100.0
    imm[where(imm le thresh)]/=100.0
    img2=imm

;remove the disk so the algorithm doesn't get confused.
    mom=moment(img2)
    meanv=mom[0]
    stdv=sqrt(mom[1])
    resim=aia_hide_disk(subindex[t],img2,value=meanv)
    img2=resim
    ;tv,img2
    
    
;END DEBUG

    if keyword_set(rebin) then begin
       img2=rebin(img2,rebin[0],rebin[1])
       resim=rebin(resim,rebin[0],rebin[1])
       nx=rebin[0]
       ny=rebin[1]
    endif
    ;tv,img2

    if not keyword_set(threshold) then begin
       if keyword_set(level) then thresh=stdv*level else thresh=stdv*0.60
    endif else begin
       thresh=threshold
    endelse

    
    ; group pixels in current data array
    ;=====================================
    rankdown, img2, mask2, threshold=threshold; , pad=1
    contiguous_mask,img2,mask2,/unipolar,threshold=threshold

    ; defines structures for current data array
    ;============================================
    create_features,img2, mask2, features2, min_size=min_size, $
                    vx=vx, vy=vy, peakthreshold=peakthreshold , dx=dx
    ;error management - if no features are
    ;detected, move on to the next image
    sz=size(features2)
    nels=n_elements(sz)
    if sz[nels-2] eq 0 then begin
       print,'No features found in this image. Continuing...'
       continue
    endif
       
    features2.step=t+1 ; insert current time step


    ; after at least two steps, begin matching
    ;========================================= 
    if (t gt start_step+1) then begin 
 
        ; match features from previous and current steps
        ;================================================
        match_features_v01, features1, features2, mask1, mask2, img1, img2, $
          old_max_label=old_max_label 
        
        ; concantenate masks from current step with prev. steps' masks
        ;==============================================================
        all_masks = [[[all_masks]],[[mask2]]]

        ; concantenate features from current step with prev. steps'
        ;==========================================================
        if (n_elements(all_features) eq 0) then  all_features = features1 $
        else all_features = [all_features, features1]

        ; find highest existing label, for unique labels on next step
        ;=============================================================
        old_max_label = max([all_features.label, features2.label]) 
    
    endif else all_masks = mask2  ; store current mask

    ; raname current data array, mask, and
    ; feature as same from previous step
    ;=========================================
    img1 = temporary(img2)
    mask1 = temporary(mask2)
    features1 = temporary(features2)

    if keyword_set(ps) then begin
       set_plot,'ps'
       device,/color,filename=STRCOMPRESS(filename+'.eps',/re),/inches,xsize=10,ysize=10
    endif

    ; graphical output of features, labels
    ;=========================================
    if not keyword_set(ps) then wdef,0,nx,ny
    
    display_yafta,resim,$
                  tit='STEP '+strtrim(string(t+1),2)+'  ('+ind.origin+'/'+ind.instrume+$
                  '/'+ind.wave_str+'  '+ind.date_obs+')',/aspect
    plot_edges,mask1,thick=4
    plot_labels,features1,thick=4
    
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

stop

endfor

; Include most recent step's features.
;=====================================
if t eq start_step then all_features=features1 else $
   all_features = [all_features, features1]


; Rename for use with YAFTA_IDL.pro
;=====================================
features = all_features


; Write data to YAFTA_output.sav
;=====================================
save,subindex,subdata,features,all_masks,thresh,min_size,dx,$
	filename=savepath+'yaftawave_output_'+eventname+'_AIA_'+wav+'.sav'

end
