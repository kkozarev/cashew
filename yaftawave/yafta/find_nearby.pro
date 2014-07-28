;+
;
; NAME: find_nearby.pro
;
; USAGE: find_nearby, features, masks, gap, nearby, addresses,
;   labels=labels
; 
; PURPOSE: Find features which lie "near" oppositely signed features.
;   "Near" is determined by the integer parameter GAP; features whose
;   edges are separated by a distance (in pixels) less than or equal to
;   GAP are "near" each other. By default, returns pairs of nearby
;   features indices; optionally, can return these pairs' labels
;   instead.  
;
; ARGUMENTS:
;    FEATURES = array of structures of each feature; checks for
;      simultaneity, so this array can contain features from multiple
;      steps
;    MASKS = this 3-D is a concatenation of 2-D mask arrays from which
;      FEATURES was created; 3rd dimension should have as many
;      components as there are distinct steps in FEATURES
;    GAP = integer; oppositely signed features separated by this
;      number of pixels or less are labelled nearby; DEFAULT = 1
; 
; RETURNS:
;   NEARBY = (2 x N) array of pairs of nearby features' indices
;     in the array FEATURES 
;   ADDRESSES = N-element array of strings, each element of which
;     contains the pixel addresses of dilated masks that overlapped
;     Retrieve overlapping pixels of k-th pair of features via:
;       IDL>  pixels = long(strsplit(addresses(k),/extract))
;
; OPTIONAL ARGUMENTS/KEYWORDS:
;   LABELS = set this keyword to return the labels of the nearby
;     features (DEFAULT is to return indices)
;    
; SIDE EFFECTS/ROUTINES CALLED: DILATE.PRO
; 
; HISTORY: Written 27 March 2005, BTW
;
;- 

pro find_nearby, features, masks, gap, nearby, addresses, labels=labels

if not(keyword_set(gap)) then gap = 1
if not(keyword_set(labels)) then labels = 0

; will use this to dilate features' masks
n_gap = 2*gap + 1
dilmask = replicate(1, n_gap, n_gap)

; initialize now for concatenation later
nearby = intarr(2)
addresses = ''

; define steps at which features are compared
min_step = min(features.step)
max_step = max(features.step)
n_step = max_step - min_step + 1

zmask = fix(masks(*,*,0))
zmask(*,*) = 0

for i = 0,n_step-1 do begin

    stepi = i + min_step
    posind = where(features.sign gt 0 and features.step eq stepi, n_pos)
    negind = where(features.sign lt 0 and features.step eq stepi, n_neg)

    if (n_pos ne 0) and (n_neg ne 0) then begin
        pos = features(posind)
        neg = features(negind)

        ; build dilated mask of negative features
        negmask = masks(*,*,i)
        negmask(where(negmask gt 0)) = 0
        negmask = abs(negmask)
        for j = 0,n_neg-1 do begin
            jmask = zmask
            jaddr = long(strsplit(neg(j).mask_str,/extract))
            jmask(jaddr) = 1.
            jmask2 = fix(dilate(jmask,dilmask))
            jaddr2 = where(jmask2 ne 0) 
            ; dilated labels can overlap/conflict; labels already present 
            znegmask = where(negmask(jaddr2) eq 0, n_zneg)
            if (n_zneg ne 0) then negmask(jaddr2(znegmask)) = neg(j).label
        endfor

        ; build dilated mask for each positive, check for overlap
        for j = 0,n_pos-1 do begin
            posmask = zmask
            posmask(long(strsplit(pos(j).mask_str,/extract))) = 1
            posdil = fix(dilate(posmask, dilmask))

            overlap = posdil * negmask 
            addresses_j = where(overlap ne 0, n_overlap)
            if (n_overlap ne 0) then begin
                olabels = overlap(addresses_j)
                olabels = olabels(uniq(olabels,sort(olabels)))
                n_olabels = n_elements(olabels)
                for k = 0,n_olabels-1 do begin
                    klabel = olabels(k)

                    ; store addresses of overlapping pix
                    kwhere = where(overlap eq klabel)
                    addresses = [addresses, strjoin(string(kwhere))]
                    
                    ; store label or index?  (default is labels = 0)
                    if (labels) then begin 
                        possave = features(posind(j)).label
                        negsave = klabel 
                    endif else begin
                        possave = posind(j)
                        negsave = where(features.label eq klabel and $
                                        features.step eq stepi)
                    endelse
                    nearby = [[nearby],[possave,negsave]]

                endfor ; ends loop over k labels that overlap jth pos. feat.

            endif ; ends check that at least one label overlaps

        endfor ; ends loop over positive features

    endif ; ends check that pos. & neg. features exist at step i

endfor ; ends loop over steps in feature array

; remove null initializations
n_nearby = n_elements(nearby)/2
if (n_nearby eq 1) then nearby = -1 else begin
    nearby = nearby(*,1:n_nearby-1)
    addresses = addresses(1:n_nearby-1)
endelse

end

