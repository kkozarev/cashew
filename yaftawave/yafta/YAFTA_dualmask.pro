; Tracks both convex & contiguous features, and matches labels.
;===============================================================
device,decompose=0  	; gets color table correct
device,retain=2		; refreshes windows when made foreground	

; get data --- restore only if not done already
;==============================================
if (n_elements(blong) eq 0) then restore,'YAFTA_input.sav'
nx = n_elements(blong(*,0,0))
ny = n_elements(blong(0,*,0))
nt = n_elements(blong(0,0,*))

; set some parameters of tracking run
;====================================
threshold = 30                  ; ignore pixels below this
min_size = 4                    ; only track features w/min_size pixels
dx = 1.433e+8			; MDI Full Disk pixel size in cm
jpg = 0                         ; set to 1 to generate .jpg at each step
ps = 0                          ; set to 1 to generat .ps at each step

for i = 0, nt-1 do begin

    ; for generating graphical output
    ;================================
    IF (keyword_set(JPG) or keyword_set(ps)) THEN BEGIN 
        tmpnum = STRING(1000 + fix(i), FORMAT = '(I4)')
        filenum = STRMID(tmpnum , 1)
        fileid = STRCOMPRESS('file-' + filenum, /REMOVE_ALL)

        if keyword_set(ps) then begin
            square_plot,'ps' 
            device,bits=8,/color,filename=STRCOMPRESS(fileid+'.ps',/re)
        endif	    
    endif

    print, "Tracking step:",string(i+1) 

    ; get current data array (suffix "2" is from
    ; current step, "1" is from prev. step)
    ;===========================================
    img2 = blong(*,*,i)

    ; convex pixel groupings in current data array
    ;=============================================
    rankdown, img2, mask2, threshold=threshold, pad=1

    ; defines structures for current data array
    ;============================================
    create_features,img2, mask2, features2, min_size=min_size, $
      vx=vx, vy=vy, dx=dx, peakthreshold=peakthreshold
    features2.step=i+1 ; insert current time step

    ; do same for contiguous pixel groupings & their structures
    ;===========================================================
    contiguous_mask, img2, alt_mask2, threshold=threshold, pad=1

    create_features,img2, alt_mask2, alt_features2, min_size=min_size, $
      vx=vx, vy=vy, dx=dx, peakthreshold=peakthreshold
    alt_features2.step=i+1 ; insert current time step

    ; after at least two steps, begin matching 
    ;==========================================
    if (i gt 0) then begin 
 
        ; match convex features from previous and current steps
        ;=======================================================
        match_features_v01, features1, features2, mask1, mask2, img1, img2, $
          old_max_label=old_max_label

        ; now match contiguous features
        ;================================================
        match_features_v01, alt_features1, alt_features2, $
		alt_mask1, alt_mask2, img1, img2, $
                old_max_label=alt_max_label

        ; concantenate masks from current step with prev. steps' masks
        ;==========================================================
        all_masks = [[[all_masks]],[[mask2]]]  	  ; convex masks
        alt_masks = [[[alt_masks]],[[alt_mask2]]] ; contiguous masks

	; must match convex and contiguous masks 
        ;========================================
	match_masks, features2, alt_features2, mask2, alt_mask2

        ; concantenate features from current step with prev. steps'
        ;==========================================================
        if (n_elements(all_features) eq 0) then  all_features = features1 $
        else all_features = [all_features, features1]  ; convex

        if (n_elements(alt_features) eq 0) then  alt_features = alt_features1 $
        else alt_features = [alt_features, alt_features1] ; & contiguous

        ; find highest existing label, for unique labels on next step
        ;=============================================================
        old_max_label = max([all_features.label, features2.label]) 
        alt_max_label = max([alt_features.label, alt_features2.label]) 


    endif else begin
	all_masks = mask2  	; store current convex mask
	alt_masks = alt_mask2  	; store current contiguous mask
    endelse

    ; raname current data array, mask, and
    ; feature as same from previous step
    ;=========================================
    img1 = temporary(img2)
    mask1 = temporary(mask2)
    features1 = temporary(features2)

    alt_mask1 = temporary(alt_mask2)
    alt_features1 = temporary(alt_features2)

    ; graphical output of features, labels
    ;=========================================
    window,0
    display_yafta,img1,min=-3*threshold,max=3*threshold, $
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

endfor

; Include most recent step's features.
;=====================================
all_features = [all_features, features1]
alt_features = [alt_features, alt_features1]

; Write data to YAFTA_dualmask_out.sav
;=====================================
save,blong,all_features,all_masks,alt_features,alt_masks,$
	threshold,min_size,dx,filename='YAFTA_dualmask_out.sav'

end


