;+
;
; NAME: CONTIGUOUS_MASK.PRO
;
; USAGE: contiguous_mask, img, altmask, threshold=threshold, unipolar=unipolar
;
; PURPOSE: Calls IDL's region-growing algorithm
;	   LABEL_REGION.PRO, which groups contiguous
; 	   pixels above user-supplied threshold into
;	   labelled regions.  For use with magnetograms,
;          labels are signed by data's sign.
;
; ARGUMENTS:
;   IMG = (N_x,N_y) image data to be grouped
;   MASK = (N_x,N_y) output array, with signed group labels
;          in each pixel; pixels below threshold are not
;          grouped, and receive a zero label
;
; OPTIONAL ARGUMENTS/KEYWORDS:
;   THRESHOLD = if set to a number greater than unity,
;	   pixels below this threshold in absolute value
;          are not grouped, and receive a zero label;
;          if set to a number less than unity, pixels with
;	   smaller absolute values than this fraction of
;	   IMG's absolute peak value receive a zero label;
;	   DEFAULT = 1*sigma
;   UNIPOLAR = set this if IMG contains only positive data
;   NOT_ALL = set this keyword to override the default searching 
;	   each pixel's 8 nearest neighbors (consistent with 
; 	   RANKDOWN.PRO)
;   PAD = set this keyword if IMG has non-zero pixels on its
;          edge rows and columns; RANKDOWN will then pad IMG,
;          calculate MASK, and un-pad IMG
;
; SIDE EFFECTS/ROUTINES CALLED:
;   calls PAD_ARRAY.PRO, LABEL_REGION.PRO
;
; HISTORY: BTW, 02 May 2008 -- From alt_mask.pro; fixed /unipolar bug.
;
;-

pro contiguous_mask, img, mask, threshold=threshold, $
	pad=pad, not_all=not_all, unipolar=unipolar

; data array MUST be padded w/border of zeros  
;==============================================
if (keyword_set(pad)) then img = pad_array(img, /silent) ; if not, do so! 

if (keyword_set(not_all)) then all_neighbors = 0 else all_neighbors = 1

if (keyword_set(threshold)) then $
  if (threshold lt 1.) then threshold = .1*max(abs(img))
if (n_elements(threshold) eq 0) then begin
    stats = moment(abs(img))
    threshold = sqrt(stats(1))
    print,'% CONTIGUOUS_MASK: Using 1-sigma threshold of ', $
      string(threshold)
endif

nx = n_elements(img(*,0))
ny = n_elements(img(0,*))
fnx = float(nx)

mask = lonarr(nx,ny)

zeros = where( abs(img) le threshold)
nonz  = where(abs(img) gt threshold)

; first label positive pixels
;============================
pos_msk = img
below = where(img lt threshold, n_below)
if (n_below ne 0) then pos_msk(below) = 0
pos_lab = label_region(pos_msk, all_neighbors=all_neighbors,/ulong)
n_pos = max(pos_lab)

; if necessary, label negatives
;==============================
if (keyword_set(unipolar)) then mask = pos_lab else begin

	neg_msk = -img
	below = where(-img lt threshold, n_below)
	if (n_below ne 0) then neg_msk(below) = 0
	neg_lab = label_region(neg_msk, all_neighbors=all_neighbors,/ulong)
	n_neg = max(neg_lab)


	; offset pos mask by # of neg
	;=============================
	nonz_pos = where(pos_lab ne 0)
	modified_pos = float(pos_lab)
	modified_pos(nonz_pos) = modified_pos(nonz_pos) + n_neg

	; make a combined, signed mask with labels for both
	;==================================================
	mask = -1*float(neg_lab) + modified_pos

endelse

; if padded herein, then gotta un-pad!
;=====================================
if (keyword_set(pad)) then begin     
	img  =  img(1:nx-2,1:ny-2) 
	mask = mask(1:nx-2,1:ny-2)   ; un-pad mask, too! same size = good
endif

mask = long(mask)

end


