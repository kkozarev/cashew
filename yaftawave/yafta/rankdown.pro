;+
;
; NAME: RANKDOWN.PRO
;
; USAGE: rankdown, img, mask, threshold=threshold, pad=1
; 
; PURPOSE: A flux-ranked, downhill labelling algorithm that groups
;          collections of pixels together that lie on the same
;          ``hills'' in absolute field strength together.
;
; ARGUMENTS:
;   IMG = (N_x,N_y) image data to be grouped
;   MASK = (N_x,N_y) output array, with signed group labels
;          in each pixel; pixels below threshold are not
;          grouped, and receive a zero label
;
; OPTIONAL ARGUMENTS/KEYWORDS:
;   THRESHOLD = pixels below this threshold in abs. value are
;          not grouped, and receive a zero label; if THRESHOLD value
;          passed to RANKDOWN is nonzero but less than unity, then 
;          the imposed threshold is this input fraction of the max. 
;          absolute signal in IMG; if no THRESHOLD is input, the 
;          DEFAULT is 1-sigma in intensity (and might be less than 1).
;   PAD = set this keyword if IMG has non-zero pixels on its
;          edge rows and columns; RANKDOWN will then pad IMG,
;          calculate MASK, and un-pad IMG
;
; SIDE EFFECTS/ROUTINES CALLED:
;   calls PAD_ARRAY.PRO
;
; HISTORY: BTW, 23 Mar 2005: written, based on Longcope ranking scheme
;          BTW, 06 Aug 2007: changed TEMPMASK init. to LONG integers
;          BTW, 21 May 2008: slightly edited this header comment section
;- 

pro rankdown, img, mask, threshold=threshold, pad=pad, $
	unipolar=unipolar

; data array MUST be padded w/border of zeros  
;==============================================
if (keyword_set(pad)) then img = pad_array(img, /silent) ; if not, do so! 

if (keyword_set(threshold)) then $
  if (threshold lt 1.) then threshold = .1*max(abs(img))
if (n_elements(threshold) eq 0) then begin
    stats = moment(abs(img))
    threshold = sqrt(stats(1))
    print,'% RANDOWNHILL: Using 1-sigma threshold of ', $
      string(threshold)
endif

nx = n_elements(img(*,0))
ny = n_elements(img(0,*))
fnx = float(nx)

mask = lonarr(nx,ny)

; construct array of shifts: none, then CCW from straight right
; (edge pixels are below threshold & ignored, so all shifts okay)
;================================================================
offsets = [ 0, 1, nx+1, nx, nx-1, -1,  -nx-1, -nx, -nx+1]

; loop over pos. pix, then neg. pix
;===================================
if (keyword_set(unipolar)) then nloop = 1 else nloop = 2

for q2 = 0,nloop-1 do begin

	if (q2 eq 0) then tempimg = img else tempimg = img*(-1)

	; 1-D array of ranked pixel addresses
	;=====================================
	ranks = reverse(sort(tempimg))  ; highest-to-lowest

	zeros = where(tempimg lt threshold, n_zeros)
	n_check = fnx*ny - n_zeros 	; only check above threshold

	tempmask = lonarr(nx,ny)
	tempmask(ranks) = lindgen(nx*ny)+1L 
	if (n_zeros ne 0) then tempmask(zeros) = n_check 

	current_label = 2
	for q1 = 1.,n_check - 1 do begin

	    ; neighbor subarray
	    ;===================	
	    maxnbr = max( tempimg( ranks(q1) + offsets ), maxpix ) 
	    nbrlabels = tempmask( ranks(q1) + offsets ) 

	    ; neighbor w/lowest label, or a new local maximum?
	    ;===================================================
	    if (maxpix ne 0) then tempmask(ranks(q1)) = nbrlabels(maxpix) $
		else begin
			tempmask(ranks(q1)) = current_label
			current_label = current_label + 1
		endelse

	endfor

	; offset 2nd iteration by max label from 1st
	;=============================================
	if (q2 eq 1) then tempmask = tempmask + max(mask) 

	tempmask(zeros) = 0 
        if (q2 eq 1) then tempmask = -tempmask
	mask = mask + tempmask

endfor 

; if padded herein, then gotta un-pad!
;=====================================
if (keyword_set(pad)) then begin     
	img  =  img(1:nx-2,1:ny-2) 
	mask = mask(1:nx-2,1:ny-2)   ; un-pad mask, too! same size = good
endif

mask = fix(mask)

end




