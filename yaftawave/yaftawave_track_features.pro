pro yaftawave_track_features, subindex,subdata,baseim,features,all_masks,min_size=min_size,$
                              threshold=threshold,ps=ps,nohanning=nohanning, level=level,eventname=eventname

;This test is the state of YAFTAWave development as of 08/31/2011
;The skeleton of the procedure is taken from the original yafta test
;procedure.
;This version uses base difference images, removes noise with a
;Hanning filter, and then applies YAFTA with the appropriate parameters.
;KAK
;Update: 09/12/2011 - KAK - This is an early production version of the
;                     yaftawave feature tracking routine for AIA data.


;INPUT:
;       subindex - an index array for the AIA data.
;       subdata - the AIA datacube.
;
;OPTIONAL INPUT:
;
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



set_plot,'x'
testpath='/home/kkozarev/algoTests/yafta/test/'
respath=testpath;+'test/'
if not keyword_set(eventname) then eventname='event'

;restore,testpath+'normalized_AIA_20110125_05_211_subdata_testdata.sav'
path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
enum='32'
date='20110427'
wav='193'
file='normalized_AIA_'+date+'_'+enum+'_'+wav+'_subdata.sav'
restore,path+enum+'/'+file
start_step=25
nx = n_elements(subdata[*,0,0])
ny = n_elements(subdata[0,*,0])
nt = n_elements(subdata[0,0,*])
end_step=nt-1
baseavgnsteps=10

;subdata=subdata[*,*,10:119]
;subindex=subindex[10:119]

;prepare base image
baseim=fltarr(1024,1024)
for i=start_step-baseavgnsteps,start_step-1 do baseim += subdata[*,*,i]
baseim /=(1.0*baseavgnsteps)




; set some parameters of tracking run
;====================================
dx=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)   ; AIA Full Disk pixel size in km
if not keyword_set(min_size) then min_size = 2000       ; only track features containing min_size pixels or more

for t = start_step,end_step do begin
   
    wav=strtrim(string(subindex[t].wavelnth),2)
    print, "Tracking step:",string(t+1)
    filenum = STRMID(STRING(1000 + fix(t+1), FORMAT = '(I4)'), 1)
    filename = STRCOMPRESS('ywave_'+'AIA_'+wav+'_'+ filenum, /REMOVE_ALL)
    

    ; get current data array (suffix "2" is from
    ; current step, "1" is from prev. step)
    ;===========================================


    ;Despike the image, and subtract the base image from it.
    ind=subindex[t]
    im=smooth(despike_gen(subdata[*,*,t]),8)
    img2 = im-baseim
    ;tv,img2


    ;remove the disk so the algorithm doesn't get confused.
    mom=moment(img2)
    meanv=mom[0]
    resim=aia_hide_disk(subindex[t],img2,value=meanv)

    ;Apply Hanning noise removal filter:
    ;===========================================
if not keyword_set(nohanning) then begin
   image=resim
   imageSize = [nx,ny]
   transform = SHIFT(FFT(image), (imageSize[0]/2), (imageSize[1]/2))
   mask = HANNING(imageSize[0], imageSize[1])
   maskedTransform = transform*mask
   inverseTransform = FFT(SHIFT(maskedTransform,(imageSize[0]/2), (imageSize[1]/2)), /INVERSE)
   img2=REAL_PART(inverseTransform)
   transim=img2
endif


    ;Here, enhance the brighter features some more:
    ;===========================================

;DEBUG    
    ;Set the threshold based on the
    ;standard deviation of the resulting image.
    mom=moment(img2)
    stdv=sqrt(mom[1])
    if not keyword_set(threshold) then begin
       if keyword_set(level) then thresh=stdv*level else thresh=stdv*0.60
       ;print,level
       ;print,thresh
       ;print,''
    endif else begin
       thresh=threshold
    endelse
    
    imm=img2
    imm[where(imm gt thresh)]*=100.0
    imm[where(imm le thresh)]/=100.0
    img2=imm
    tv,imm
    stop
;END DEBUG

    
    ;This was added on September 14 by Kamen Kozarev
    ;img2=smooth(img2,4)
    ;stop


    ; group pixels in current data array
    ;=====================================
    ;rankdown, img2, mask2, threshold=threshold; , pad=1
    contiguous_mask,img2,mask2,/unipolar,threshold=thresh

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
       device,/color,filename=STRCOMPRESS(ps+filename+'.eps',/re),/inches,xsize=10,ysize=10
    endif

    ; graphical output of features, labels
    ;=========================================
    if not keyword_set(ps) then wdef,0,1024
    
    display_yafta,resim,$
                  tit='STEP '+strtrim(string(t+1),2)+'  ('+ind.origin+'/'+ind.instrume+$
                  '/'+ind.wave_str+'  '+ind.date_obs+')',/aspect
    plot_edges,mask1,thick=6
    plot_labels,features1,thick=6
    stop
    ; for generating graphical output
    ;================================
    ; Close output to .ps file, then convert to png
    ;=========================
    if keyword_set(ps) then begin
        device,/close
        set_plot,'x'
        exec='convert -flatten '+ps+filename+'.eps '+ps+filename+'.png'+'; rm '+ps+filename+'.eps'
        print,exec
        spawn,exec
     endif
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
	filename=respath+'yaftawave_output_'+eventname+'_AIA_'+wav+'.sav'

end
