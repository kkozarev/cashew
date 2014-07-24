;+
;
; NAME: match_features_v01.pro
;
; USAGE: match_features_v01, features1, features2, mask1, mask2, img1, img2
; 
; PURPOSE: Associates features from Step 1 with features from Step 2,
;   using the combined information of data arrays from both steps
;   (IMG1 and IMG2), masks from both steps (MASK1 and MASK2), and
;   arrays of features' structures from both steps (FEATURES1 and FEATURES2)  
;   (Suffix "2" is from current step, "1" is from previous step!)
;
; ARGUMENTS: 
;     FEATURES1 = array of structures for each features from Step 1
;     FEATURES2 = array of structures for each features from Step 2
;     MASK1 = array with FEATURES1's labels in pixels from Step 1
;     MASK2 = array with FEATURES2's labels in pixels from Step 2
;     IMG1 = 2-D data array from Step 1
;     IMG2 = 2-D data array from Step 2   
;
; OPTIONAL ARGUMENTS/KEYWORDS: 
;     OLD_MAX_LABEL = *MUST* set this to max label over all prior steps, 
;         if max label in FEATURES1 does not contain this max label        
;
; SIDE EFFECTS/ROUTINES CALLED: Changes labels in FEATURES2, MASK2.
; 
; HISTORY: Written ??? and commented on 28 March 2005, BTW.
;          Comment re:OLD_MAX_LABEL added 04 March 2010, BTW.
;- 

pro match_features_v01, features1, features2, mask1, mask2, img1, img2, $
                        old_max_label = old_max_label

; Match using three masks: the old one, the original one from the new 
; time step, and the modified one from the new time step, which might 
; get re-modified with a pass or two...

nx = n_elements(mask1(*,0))
ny = n_elements(mask1(0,*))

n_features1 = n_elements(features1.label)
labels2 = features2(uniq(features2(sort(features2.label)).label)).label
n_features2 = n_elements(labels2)

if not(keyword_set(old_max_label)) then $
  old_max_label = max(features1.label)

new_label = old_max_label + 1

step1 = features1(0).step
step2 = step1 + 1

new_mask = mask2

; ONE-TO-ONE MATCHES: look for these first, err toward this hypoth.
for j = 0,n_features2-1 do begin

    ; get labels of overlapping same-sign pixels
    where2j = where(abs(mask2) eq j+1)  ; overlap addresses,
    match_mask = mask1(where2j)*features2(j).sign ; signed labels,
    samesign = where(match_mask gt 0, n_samesign) ; keep same-signes labels

    ; (could store (n_oppsign ne 0) for collision purposes?)
    ; oppsign = where(match_mask lt 0, n_oppsign)
 
    if (n_samesign eq 0) then begin ; no match = new flux
        features2(j).label = new_label
        new_mask(where2j) = features2(j).label*features2(j).sign
        features2(j).src = -step2
        new_label = new_label + 1
    endif else begin ; otherwise, assume one-to-one match, for starters

        ; bin to find # of overlapping pixels for each label
        match_mask = match_mask(samesign)
        match_fracs = histogram(match_mask,reverse_indices=revind)

        ; find label & index of greatest overlap
        max_frac = max(match_fracs, match_index)
        match_label = match_mask(revind(revind(match_index)))
        match_index = where(features1.label eq match_label)

        features2(j).label = features1(match_index).label ;  update label
        new_mask(where2j) = features2(j).label*features2(j).sign ; update mask
        features2(j).src   = features1(match_index).label ; record "source"
        features1(match_index).trm = features1(match_index).label ; propagate
    endelse

endfor


; FRAGMENTATIONS: remove duplicate labels
lab2 = features2.label
lab2 = lab2(sort(lab2))
lab2 = lab2(uniq(lab2))
n_lab2 = n_elements(lab2)

for j = 0,n_lab2-1 do begin
    this_label = where(features2.label eq lab2(j), n_this)

    if (n_this gt 1) then begin ; a fragmentation!
        ; sort same-labels by flux
        index_order = reverse(sort(features2(this_label).phi))

        for k = 1,n_this-1 do begin ; largest stays, others renamed
            this_index = this_label(index_order(k))
            features2(this_index).label = new_label
            features2(this_index).src = $
		features2(this_label(index_order(0))).label 
            addresses = long(strsplit(features2(this_index).mask_str,/extract))
            new_mask(addresses) = features2(this_index).label* $
              features2(this_index).sign
            new_label = new_label + 1
        endfor  ; closes reassignments of multiple labels
    endif       ; closes multiple label check
endfor          ; closes fragment reassignment


; COLLISIONS: see why some elements were not propagated
for j = 0,n_features1-1 do begin

    propagated = where(features2.label eq features1(j).label, n_prop)
    if (n_prop eq 0) then begin

        ; get labels of overlapping same-sign pixels
        where1j = where(abs(mask1) eq features1(j).label) ; overlap addresses, 
        match_mask = new_mask(where1j)*features1(j).sign ;  signed labels, 
        samesign = where(match_mask gt 0, n_samesign) ; keep same-signed labels
        
        ; (could store (n_oppsign ne 0) for collision purposes?)
        ; oppsign = where(match_mask lt 0, n_oppsign)

        if (n_samesign eq 0) then $ ; no match = terminated!
            features1(j).trm = -step2 $
        else begin ; either collisions, or wrongly "poached" labels

            ; bin to find # of overlapping pixels for each label 
            match_mask = match_mask(samesign) ; keep nonzero same signs
            match_labs = match_mask(uniq(match_mask, sort(match_mask)))
            n_match = n_elements(match_labs)
            match_indices = fltarr(n_match)
            match_fluxes = fltarr(n_match)

            for k=0,n_match-1 do begin
                match_indices(k) = where(features2.label eq match_labs(k))
                match_addresses = $
                  where((abs(mask1) eq features1(j).label) and $
                        (abs(new_mask) eq features2(match_indices(k)).label))
                match_fluxes(k) = abs(total(img1(match_addresses) + $
                                            img2(match_addresses))) 
            endfor

            index_order = reverse(sort(match_fluxes))
            collision = 1.      ; assume a collision occured

            for k = 0.,n_match-1. do begin
                match_index = match_indices(index_order(k))

                ; Features that switched labels might've done so wrongly...
                if (features2(match_index).label ne $
                    features2(match_index).src) then begin 

                    ;reclaim_string =   $
                    ;  strcompress( "Reclaiming pole from collision:"+ $
                    ;               string(fix(match_index+1))+$
                    ;              ' was sourced as '+ $
                    ;               string(features2(match_index).src)+ $
                    ;               ' , but really belongs to '+ $
                    ;               string(features1(j).label) )
                    ;print,reclaim_string
                    
                    ; update new pole's fields
                    features2(match_index).label = features1(j).label
                    features2(match_index).src = features1(j).label
                    features1(j).trm = features1(j).label

                    addresses = $
                      long(strsplit(features2(match_index).mask_str,/ext))

                    new_mask(addresses) = features1(j).label* $
                      features1(j).sign
                    collision = 0 ; records fact of no collision 
                    k = n_match-1. ; breaks this for loop 
                endif 
            endfor       ; closes loop over matches w/different labels
            
            ; if not bad propagation, then assume collision w/0th overlap
            if (collision eq 1) then features1(j).trm = $
              features2(match_indices(index_order(0))).label
            
        endelse ; closes (n_samesign ne 0) case
     
    endif                       ; closes j-th not-propagated check

endfor                          ; closes not-propagated check loop

mask2 = new_mask

end








