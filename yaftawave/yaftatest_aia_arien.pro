pro yaftatest_aia_arien
testpath='/home/kkozarev/algoTests/yafta/'


;restore,testpath+'normalized_AIA_20110125_05_193_data.sav'
;aia_radial_filter_v2,index,data,outdata,/radial

;stop

;evnum='05'
;wav='193'
;coords=[-955,-300]
;st='2011/01/25 12:05:00'
;et='2011/01/25 12:09:00'
;std=strsplit(st,'/ :',/extract)
;savefile=testpath+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
;newcoords=aia_autoselect_subroi(index[0],coords)
;subdata=aia_inspect_data(index,outdata,autoregion=newcoords)
;subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024)
;save,filename=savefile+'_subdata_testdata'+'.sav',subindex,subdata

;stop

restore,testpath+'normalized_AIA_20110125_05_211_subdata_testdata.sav'

blong=subdata[*,*,0];[0:499,200:699,50:80]
;blong=testdata;reform(testdata[0:99,100:199,5:*])
;blong=smooth(reform(blong[*,*,*]),4,/edge_truncate)
;for i=0,n_elements(blong[0,0,*])-1 do begin
;   blong[*,*,i]-=smooth(blong[*,*,i],20,/edge_truncate)
;   blong[*,*,i]=smooth(blong[*,*,i],4,/edge_truncate)
;endfor
;blong=smooth(blong,10)
;2*min(blong[*,*,i])

; get data --- restore only if not done already
;==============================================
nx = n_elements(blong[*,0,0])
ny = n_elements(blong[0,*,0])
nt = n_elements(blong[0,0,*])

; set some parameters of tracking run
;====================================
threshold = 100                  ; ignore pixels below this value
min_size = 150                   ; only track features with min_size pixels or more
dx = 714.0			; AIA Full Disk pixel size in km
jpg = 0                         ; set to 1 to generate .jpg at each step
ps = 0                         ; set to 1 to generat .ps at each step

wdef,0,800
for i = 0,nt-1 do begin
    ; for generating graphical output
    ;================================
    IF (keyword_set(JPG) or keyword_set(ps)) THEN BEGIN 
        tmpnum = STRING(1000 + fix(i), FORMAT = '(I4)')
        filenum = STRMID(tmpnum , 1)
        fileid = STRCOMPRESS('file-' + filenum, /REMOVE_ALL)

        if keyword_set(ps) then begin
            set_plot,'ps' 
            square_plot,'ps' 
            device,bits=8,/color,filename=STRCOMPRESS(fileid+'.ps',/re)
         endif	    
     endif

    print, "Tracking step:",string(i+1)

    ; get current data array (suffix "2" is from
    ; current step, "1" is from prev. step)
    ;===========================================
    img2 = blong(*,*,i)
    ;aia_img_diagnostics,img2
    mom=moment(img2)
    resim=aia_hide_disk(subindex[i],img2,value=sqrt(mom[1]))
    img=resim
    ;aia_img_diagnostics,img
    ;stop
    mom=moment(img)
    meanv=(mom[0])
   threshold=meanv*3
   img2=img

    ; group pixels in current data array
    ;=====================================
    ;rankdown, img2, mask2, threshold=threshold; , pad=1
    contiguous_mask,img2,mask2,/unipolar,threshold=threshold

    ; defines structures for current data array
    ;============================================
    create_features,img2, mask2, features2, min_size=min_size, $
      vx=vx, vy=vy, peakthreshold=peakthreshold , dx=dx 
    features2.step=i+1 ; insert current time step


    ; after at least two steps, begin matching
    ;========================================= 
    if (i gt 0) then begin 
 
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

    ; graphical output of features, labels
    ;=========================================
   ; if not(keyword_set(ps)) then window,0

    display_yafta,img1,min=-threshold,max=2*threshold,$
      tit='Features at Step'+string(i+1),/aspect
    plot_edges,mask1
    plot_labels,features1

    ; Output to .jpg file
    ;=====================
    IF keyword_set(JPG) THEN image = $
      tvread(FILENAME = fileid, /JPEG, QUALITY = 100, /NODIALOG)

    ; Close output to .ps file
    ;=========================
    if keyword_set(ps) then begin
        device,/cl
        set_plot,'x'
    endif
wait,0.1
;stop
endfor

; Include most recent step's features.
;=====================================
if nt eq 1 then all_features=features1 else $
   all_features = [all_features, features1]

; Rename for use with YAFTA_IDL.pro
;=====================================
features = all_features
stop
; Write data to YAFTA_output.sav
;=====================================
save,subindex,subdata,features,all_masks,threshold,min_size,dx,$
	filename=testpath+'YAFTATEST_AIA_output.sav'


;Kill the features/mask variables
delvar,features,all_features,all_masks
end
